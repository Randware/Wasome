use crate::context::{FunctionContext, LLVMContext};
use crate::Codegen;
use ast::data_type::DataType;
use ast::symbol::{EnumSymbol, StructSymbol};
use ast::TypedAST;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::{AddressSpace, IntPredicate};

impl<'ctx> Codegen<'ctx> {
    /// Reads the reference count from the first field of a heap-allocated value.
    ///
    /// The refcount is stored at index 0 of the base heap-allocated struct layout
    /// (defined in [`GlobalRegistry`](crate::global_registry::GlobalRegistry)).
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for IR operations
    /// * `pointer` - The pointer to the heap-allocated value
    ///
    /// # Returns
    ///
    /// The current reference count as an LLVM `IntValue`.
    fn read_refcount(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        pointer: PointerValue<'ctx>,
    ) -> IntValue<'ctx> {
        let refc = llvm_context
            .builder()
            .build_struct_gep(
                llvm_context.global_registry().base_heap_allocated(),
                pointer,
                0,
                "gep_refc",
            )
            .unwrap();
        llvm_context
            .builder()
            .build_load(self.context.i32_type(), refc, "load_refc")
            .unwrap()
            .into_int_value()
    }

    /// Writes a value to the reference count field of a heap-allocated value.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for IR operations
    /// * `pointer` - The pointer to the heap-allocated value
    /// * `value` - The new reference count value
    pub fn write_refcount(
        llvm_context: &LLVMContext<'ctx>,
        pointer: PointerValue<'ctx>,
        value: IntValue<'ctx>,
    ) {
        #[expect(clippy::missing_panics_doc, reason = "infallible")]
        let refc = llvm_context
            .builder()
            .build_struct_gep(
                llvm_context.global_registry().base_heap_allocated(),
                pointer,
                0,
                "gep_refc",
            )
            .unwrap();
        #[expect(clippy::missing_panics_doc, reason = "infallible")]
        llvm_context.builder().build_store(refc, value).unwrap();
    }

    /// Increments the reference count of a heap-allocated value by the given amount.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for IR operations
    /// * `pointer` - The pointer to the heap-allocated value
    /// * `amount` - The amount to increment by
    fn compile_add_refcount(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        pointer: PointerValue<'ctx>,
        amount: u32,
    ) {
        let current = self.read_refcount(llvm_context, pointer);
        let result = llvm_context
            .builder()
            .build_int_add(
                current,
                self.context.i32_type().const_int(u64::from(amount), false),
                "add_refc",
            )
            .unwrap();
        Self::write_refcount(llvm_context, pointer, result);
    }

    /// Decrements the reference count of a heap-allocated value by the given amount.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for IR operations
    /// * `pointer` - The pointer to the heap-allocated value
    /// * `amount` - The amount to decrement by
    ///
    /// # Returns
    ///
    /// The new reference count value as an LLVM [`IntValue`].
    fn compile_sub_refcount(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        pointer: PointerValue<'ctx>,
        amount: u32,
    ) -> IntValue<'ctx> {
        let current = self.read_refcount(llvm_context, pointer);
        let result = llvm_context
            .builder()
            .build_int_sub(
                current,
                self.context.i32_type().const_int(u64::from(amount), false),
                "sub_refc",
            )
            .unwrap();
        Self::write_refcount(llvm_context, pointer, result);
        result
    }

    /// Increments the reference count of a heap-allocated value by 1.
    ///
    /// Called when a pointer value is copied (e.g., on load from alloca or struct field access).
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for IR operations
    /// * `to_inc` - The pointer to the heap-allocated value
    pub(crate) fn compile_inc_refcount(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        to_inc: PointerValue<'ctx>,
    ) {
        self.compile_add_refcount(llvm_context, to_inc, 1);
    }

    /// Drops a reference-counted value by loading the pointer and delegating to `compile_val_drop`.
    ///
    /// For struct and enum types, loads the pointer from the alloca slot and then calls
    /// [`compile_val_drop`](Self::compile_val_drop) to decrement the refcount and potentially
    /// call the drop function. For non-struct/enum types, this is a no-op.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for IR operations
    /// * `func` - The mutable [`FunctionContext`] for block management
    /// * `dt` - The [`DataType`] of the value
    /// * `to_drop` - The pointer to the heap-allocated value
    pub(crate) fn compile_val_ref_drop(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        func: &mut FunctionContext<'ctx>,
        dt: &DataType,
        to_drop: PointerValue<'ctx>,
    ) {
        match dt {
            DataType::Struct(_) | DataType::Enum(_) => {
                let val = llvm_context
                    .builder()
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        to_drop,
                        "drop_load",
                    )
                    .unwrap();
                self.compile_val_drop(llvm_context, func, dt, val);
            }
            _ => (),
        }
    }

    /// Decrements the reference count for a struct or enum value, calling the drop function
    /// if the count reaches zero.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for IR operations
    /// * `func` - The mutable [`FunctionContext`] for block management
    /// * `dt` - The [`DataType`] of the value (must be Struct or Enum)
    /// * `to_drop` - The LLVM [`BasicValueEnum`] representing the value
    pub(crate) fn compile_val_drop(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        func: &mut FunctionContext<'ctx>,
        dt: &DataType,
        to_drop: BasicValueEnum<'ctx>,
    ) {
        match dt {
            DataType::Struct(st) => {
                self.compile_struct_dec_refcount(
                    llvm_context,
                    func,
                    st,
                    to_drop.into_pointer_value(),
                );
            }
            DataType::Enum(en) => {
                self.compile_enum_dec_refcount(
                    llvm_context,
                    func,
                    en,
                    to_drop.into_pointer_value(),
                );
            }
            _ => (),
        }
    }

    /// Increments the reference count when a pointer value is copied (e.g., on load from alloca).
    ///
    /// Only applies to pointer values (struct and enum types). Non-pointer values are no-ops.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for IR operations
    /// * `to_create` - The LLVM [`BasicValueEnum`] to increment
    pub(crate) fn compile_val_create(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        to_create: BasicValueEnum<'ctx>,
    ) {
        if to_create.is_pointer_value() {
            self.compile_inc_refcount(llvm_context, to_create.into_pointer_value());
        }
    }

    /// Decrements the reference count for a struct, calling its drop function when the count reaches zero.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for IR operations
    /// * `func` - The mutable [`FunctionContext`] for block management
    /// * `struc` - The struct symbol
    /// * `to_dec_ref` - The pointer to the heap-allocated struct
    pub(crate) fn compile_struct_dec_refcount(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        func: &mut FunctionContext<'ctx>,
        struc: &StructSymbol<TypedAST>,
        to_dec_ref: PointerValue<'ctx>,
    ) {
        let tr = llvm_context.type_registry();
        let lowered = tr.get_struct(struc).expect("Unknown struct");
        self.compile_dec_refcount(llvm_context, func, lowered.on_drop(), to_dec_ref);
    }

    /// Decrements the reference count for an enum, calling its drop function when the count reaches zero.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for IR operations
    /// * `func` - The mutable [`FunctionContext`] for block management
    /// * `enu` - The enum symbol
    /// * `to_dec_ref` - The pointer to the heap-allocated enum
    pub(crate) fn compile_enum_dec_refcount(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        func: &mut FunctionContext<'ctx>,
        enu: &EnumSymbol<TypedAST>,
        to_dec_ref: PointerValue<'ctx>,
    ) {
        let tr = llvm_context.type_registry();
        let lowered = tr.get_enum(enu).expect("Unknown enum");
        self.compile_dec_refcount(llvm_context, func, lowered.on_drop(), to_dec_ref);
    }

    /// Decrements the reference count and conditionally calls the drop function if it reaches zero.
    ///
    /// Generates a conditional branch:
    /// ```text
    ///     [decrement refcount]
    ///        |
    ///        v
    ///     [refcount == 0?] --> yes --> drop_bb --> [call drop function] --> [uncond branch]
    ///        |                                                            |
    ///        | (no)                                                       |
    ///        v                                                            v
    ///     after_bb <-----------------------------------------------------
    /// ```
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for IR operations
    /// * `func` - The mutable [`FunctionContext`] for block management
    /// * `on_drop` - The drop function [`FunctionValue`] to call
    /// * `to_dec_ref` - The pointer to the heap-allocated value
    pub(crate) fn compile_dec_refcount(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        func: &mut FunctionContext<'ctx>,
        on_drop: FunctionValue<'ctx>,
        to_dec_ref: PointerValue<'ctx>,
    ) {
        let dec = self.compile_sub_refcount(llvm_context, to_dec_ref, 1);

        let current_function = func.current_function();
        let drop_bb = llvm_context
            .context()
            .append_basic_block(current_function, "drop");
        let after_bb = llvm_context
            .context()
            .append_basic_block(current_function, "after");

        llvm_context
            .builder()
            .build_conditional_branch(
                llvm_context
                    .builder()
                    .build_int_compare(
                        IntPredicate::EQ,
                        dec,
                        llvm_context.context().i32_type().const_int(0, false),
                        "cond",
                    )
                    .unwrap(),
                drop_bb,
                after_bb,
            )
            .unwrap();
        func.set_current_block(llvm_context.builder(), drop_bb);
        llvm_context
            .builder()
            .build_call(
                on_drop,
                &[to_dec_ref.as_basic_value_enum().into()],
                "drop_struct",
            )
            .unwrap();
        llvm_context
            .builder()
            .build_unconditional_branch(after_bb)
            .unwrap();
        func.set_current_block(llvm_context.builder(), after_bb);
    }

    /// Generates the drop function body for a struct.
    ///
    /// The drop process:
    /// 1. If a `predrop` function exists:
    ///    a. Sets refcount to 2 (ghost refcount to prevent infinite loops)
    ///    b. Calls the predrop function
    ///    c. If refcount is not 1 (the predrop function revived the struct), returns early
    ///    d. Otherwise, decrements refcount and continues
    /// 2. For each struct field that is a struct or enum:
    ///    a. Gets the field pointer
    ///    b. Calls `compile_val_ref_drop` to recursively drop it
    /// 3. Frees the allocated memory using `free`
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for IR operations
    /// * `func` - The mutable [`FunctionContext`] for block management
    /// * `struc` - The struct symbol
    /// * `to_drop` - The pointer to the heap-allocated struct
    pub(crate) fn compile_struct_drop(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        func: &mut FunctionContext<'ctx>,
        struc: &StructSymbol<TypedAST>,
        to_drop: PointerValue<'ctx>,
    ) {
        let tr = llvm_context.type_registry();
        let lowered = tr.get_struct(struc).expect("Unknown struct");

        let after_block = llvm_context
            .context()
            .append_basic_block(func.current_function(), "after_block");

        match lowered.predrop() {
            None => {
                llvm_context
                    .builder()
                    .build_unconditional_branch(after_block)
                    .unwrap();
            }
            Some(predrop) => {
                let drop_abort_block = llvm_context
                    .context()
                    .append_basic_block(func.current_function(), "drop_abort_block");

                // Set the refcount to 2
                // This way, it can never be dropped again and can't cause infinite loops
                self.compile_add_refcount(llvm_context, to_drop, 2);
                llvm_context
                    .builder()
                    .build_call(
                        predrop,
                        &[to_drop.as_basic_value_enum().into()],
                        "predrop",
                    )
                    .unwrap();

                let refc_val = self.read_refcount(llvm_context, to_drop);

                llvm_context
                    .builder()
                    .build_conditional_branch(
                        llvm_context
                            .builder()
                            .build_int_compare(
                                IntPredicate::EQ,
                                refc_val,
                                llvm_context.context().i32_type().const_int(1, false),
                                "cond",
                            )
                            .unwrap(),
                        after_block,
                        drop_abort_block,
                    )
                    .unwrap();
                func.set_current_block(llvm_context.builder(), drop_abort_block);
                // Remove the ghost refcount
                  self.compile_struct_dec_refcount(llvm_context, func, struc, to_drop);
                llvm_context.builder().build_return(None).unwrap();
            }
        }
        func.set_current_block(llvm_context.builder(), after_block);

        for (i, field) in lowered.fields().iter().enumerate() {
            match field.data_type() {
                DataType::Struct(_) | DataType::Enum(_) => {
                    let field_ptr = llvm_context
                        .builder()
                        .build_struct_gep(
                            lowered.lowered(),
                            to_drop,
                            u32::try_from(i + 1).unwrap(),
                            "gep_field_drop",
                        )
                        .unwrap();
                    self.compile_val_ref_drop(llvm_context, func, field.data_type(), field_ptr);
                }
                _ => (),
            }
        }
        self.dealloc(
            llvm_context,
            to_drop,
            lowered.lowered().size_of().expect("Should be sized"),
        );
        let size = llvm_context
            .builder()
            .build_int_truncate(
                lowered.lowered().size_of().expect("Should be sized"),
                self.context.i32_type(),
                "size_resize",
            )
            .unwrap();
        llvm_context
            .builder()
            .build_call(
                llvm_context.global_registry().free(),
                &[to_drop.into(), size.into()],
                "drop_struct",
            )
            .unwrap();
        llvm_context.builder().build_return(None).unwrap();
    }

    /// Generates the drop function body for an enum.
    ///
    /// The drop process:
    /// 1. Reads the discriminant tag from the enum's base struct layout
    /// 2. Creates condition blocks and drop blocks for each variant
    /// 3. Chains conditional branches: checks if tag matches each variant in order
    /// 4. For the matching variant:
    ///    a. Recursively drops any struct or enum fields within the variant
    ///    b. Frees the variant's memory using `free`
    /// 5. Returns after all drop blocks
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for IR operations
    /// * `func` - The mutable [`FunctionContext`] for block management
    /// * `en` - The enum symbol
    /// * `to_drop` - The pointer to the heap-allocated enum
    pub(crate) fn compile_enum_drop(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        func: &mut FunctionContext<'ctx>,
        en: &EnumSymbol<TypedAST>,
        to_drop: PointerValue<'ctx>,
    ) {
        let tr = llvm_context.type_registry();
        let lowered = tr.get_enum(en).expect("Unknown struct");
        let current_function = func.current_function();

        let after_block = llvm_context
            .context()
            .append_basic_block(current_function, "after");
        let (cond_blocks, drop_blocks): (Vec<_>, Vec<_>) = lowered
            .variants()
            .iter()
            .map(|_| {
                (llvm_context
                    .context()
                    .append_basic_block(current_function, "cond"),
                 llvm_context
                     .context()
                     .append_basic_block(current_function, "drop"))
            }).unzip();
        let tag = llvm_context
            .builder()
            .build_struct_gep(
                llvm_context.global_registry().base_enum(),
                to_drop,
                1,
                "tag_gep",
            )
            .expect("Enum must have tag");
        let tag = llvm_context
            .builder()
            .build_load(self.context.i32_type(), tag, "load_load")
            .unwrap();
        llvm_context
            .builder()
            .build_unconditional_branch(cond_blocks.first().copied().unwrap_or(after_block))
            .unwrap();
        for (i, (variant, (cond, drop))) in lowered
            .variants()
            .iter()
            .zip(cond_blocks.iter().zip(drop_blocks.iter()))
            .enumerate()
        {
            let no_match = cond_blocks.get(i + 1).copied().unwrap_or(after_block);
            func.set_current_block(llvm_context.builder(), *cond);
            llvm_context
                .builder()
                .build_conditional_branch(
                    llvm_context
                        .builder()
                        .build_int_compare(
                            IntPredicate::EQ,
                            tag.into_int_value(),
                            llvm_context.context().i32_type().const_int(i as u64, false),
                            "cond",
                        )
                        .unwrap(),
                    *drop,
                    no_match,
                )
                .unwrap();
            func.set_current_block(llvm_context.builder(), *drop);
            for (i, dt) in variant.0.fields().iter().enumerate() {
                match dt {
                    DataType::Struct(_) | DataType::Enum(_) => {
                        let field = llvm_context
                            .builder()
                            .build_struct_gep(
                                variant.1,
                                to_drop,
                                u32::try_from(i + 2).unwrap(),
                                "gep_field_drop",
                            )
                            .unwrap();
                        self.compile_val_ref_drop(llvm_context, func, dt, field);
                    }
                    _ => (),
                }
            }
            self.dealloc(
                llvm_context,
                to_drop,
                variant.1.size_of().expect("Should be sized"),
            );
            llvm_context
                .builder()
                .build_unconditional_branch(after_block)
                .unwrap();
        }
        func.set_current_block(llvm_context.builder(), after_block);
        llvm_context.builder().build_return(None).unwrap();
    }

    fn dealloc(&self, llvm_context: &LLVMContext, to_dealloc: PointerValue, size: IntValue) {
        let size = llvm_context
            .builder()
            .build_int_truncate(size, self.context.i32_type(), "size_resize")
            .unwrap();
        llvm_context
            .builder()
            .build_call(
                llvm_context.global_registry().free(),
                &[to_dealloc.into(), size.into()],
                "drop",
            )
            .unwrap();
    }
}
