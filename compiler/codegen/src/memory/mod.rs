use crate::Codegen;
use crate::context::{FunctionContext, LLVMContext};
use ast::TypedAST;
use ast::data_type::DataType;
use ast::symbol::{EnumSymbol, StructSymbol};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::{AddressSpace, IntPredicate};

impl<'ctx> Codegen<'ctx> {
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

    fn write_refcount(
        llvm_context: &LLVMContext<'ctx>,
        pointer: PointerValue<'ctx>,
        value: IntValue<'ctx>,
    ) {
        let refc = llvm_context
            .builder()
            .build_struct_gep(
                llvm_context.global_registry().base_heap_allocated(),
                pointer,
                0,
                "gep_refc",
            )
            .unwrap();
        llvm_context.builder().build_store(refc, value).unwrap();
    }

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

    pub(crate) fn compile_inc_refcount(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        to_generate: PointerValue<'ctx>,
    ) {
        self.compile_add_refcount(llvm_context, to_generate, 1);
    }

    pub(crate) fn compile_val_ref_drop(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        func: &mut FunctionContext<'ctx>,
        dt: &DataType,
        to_generate: PointerValue<'ctx>,
    ) {
        match dt {
            DataType::Struct(_) | DataType::Enum(_) => {
                let val = llvm_context
                    .builder()
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        to_generate,
                        "drop_load",
                    )
                    .unwrap();
                self.compile_val_drop(llvm_context, func, dt, val);
            }
            _ => (),
        }
    }

    pub(crate) fn compile_val_drop(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        func: &mut FunctionContext<'ctx>,
        dt: &DataType,
        to_generate: BasicValueEnum<'ctx>,
    ) {
        match dt {
            DataType::Struct(st) => {
                self.compile_struct_dec_refcount(
                    llvm_context,
                    func,
                    st,
                    to_generate.into_pointer_value(),
                );
            }
            DataType::Enum(en) => {
                self.compile_enum_dec_refcount(
                    llvm_context,
                    func,
                    en,
                    to_generate.into_pointer_value(),
                );
            }
            _ => (),
        }
    }

    pub(crate) fn compile_val_create(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        to_generate: BasicValueEnum<'ctx>,
    ) {
        if to_generate.is_pointer_value() {
            self.compile_inc_refcount(llvm_context, to_generate.into_pointer_value());
        }
    }

    pub(crate) fn compile_struct_dec_refcount(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        func: &mut FunctionContext<'ctx>,
        struc: &StructSymbol<TypedAST>,
        to_generate: PointerValue<'ctx>,
    ) {
        let tr = llvm_context.type_registry();
        let lowered = tr.get_struct(struc).expect("Unknown struct");
        self.compile_dec_refcount(llvm_context, func, lowered.on_drop(), to_generate);
    }

    pub(crate) fn compile_enum_dec_refcount(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        func: &mut FunctionContext<'ctx>,
        enu: &EnumSymbol<TypedAST>,
        to_generate: PointerValue<'ctx>,
    ) {
        let tr = llvm_context.type_registry();
        let lowered = tr.get_enum(enu).expect("Unknown enum");
        self.compile_dec_refcount(llvm_context, func, lowered.on_drop(), to_generate);
    }

    pub(crate) fn compile_dec_refcount(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        func: &mut FunctionContext<'ctx>,
        on_drop: FunctionValue<'ctx>,
        to_generate: PointerValue<'ctx>,
    ) {
        let dec = self.compile_sub_refcount(llvm_context, to_generate, 1);

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
                &[to_generate.as_basic_value_enum().into()],
                "drop_struct",
            )
            .unwrap();
        llvm_context
            .builder()
            .build_unconditional_branch(after_bb)
            .unwrap();
        func.set_current_block(llvm_context.builder(), after_bb);
    }

    pub(crate) fn compile_struct_drop(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        func: &mut FunctionContext<'ctx>,
        struc: &StructSymbol<TypedAST>,
        to_generate: PointerValue<'ctx>,
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
                self.compile_add_refcount(llvm_context, to_generate, 2);
                llvm_context
                    .builder()
                    .build_call(
                        predrop,
                        &[to_generate.as_basic_value_enum().into()],
                        "predrop",
                    )
                    .unwrap();

                let refc_val = self.read_refcount(llvm_context, to_generate);

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
                self.compile_struct_dec_refcount(llvm_context, func, struc, to_generate);
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
                            to_generate,
                            u32::try_from(i + 1).unwrap(),
                            "gep_field_drop",
                        )
                        .unwrap();
                    self.compile_val_ref_drop(llvm_context, func, field.data_type(), field_ptr);
                }
                _ => (),
            }
        }
        llvm_context.builder().build_free(to_generate).unwrap();
    }

    pub(crate) fn compile_enum_drop(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        func: &mut FunctionContext<'ctx>,
        en: &EnumSymbol<TypedAST>,
        to_generate: PointerValue<'ctx>,
    ) {
        let tr = llvm_context.type_registry();
        let lowered = tr.get_enum(en).expect("Unknown struct");
        let current_function = func.current_function();

        let after_block = llvm_context
            .context()
            .append_basic_block(current_function, "after");
        let cond_blocks = lowered
            .variants()
            .iter()
            .map(|_| {
                llvm_context
                    .context()
                    .append_basic_block(current_function, "cond")
            })
            .collect::<Vec<_>>();
        let drop_blocks = lowered
            .variants()
            .iter()
            .map(|_| {
                llvm_context
                    .context()
                    .append_basic_block(current_function, "drop")
            })
            .collect::<Vec<_>>();
        let tag = llvm_context
            .builder()
            .build_struct_gep(
                llvm_context.global_registry().base_enum(),
                to_generate,
                1,
                "tag_gep",
            )
            .expect("Enum must have tag");
        let tag = llvm_context
            .builder()
            .build_load(self.context.i32_type(), tag, "load_load")
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
                                to_generate,
                                u32::try_from(i + 2).unwrap(),
                                "gep_field_drop",
                            )
                            .unwrap();
                        self.compile_val_ref_drop(llvm_context, func, dt, field);
                    }
                    _ => (),
                }
            }
            llvm_context
                .builder()
                .build_unconditional_branch(after_block)
                .unwrap();
        }
        func.set_current_block(llvm_context.builder(), after_block);
        llvm_context.builder().build_free(to_generate).unwrap();
    }
}
