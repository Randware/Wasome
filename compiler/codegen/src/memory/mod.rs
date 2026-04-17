use crate::context::LLVMContext;
use crate::Codegen;
use ast::data_type::DataType;
use ast::symbol::{EnumSymbol, StructSymbol};
use ast::TypedAST;
use inkwell::values::{BasicValue, FunctionValue, PointerValue};
use inkwell::IntPredicate;

impl<'ctx, 'fc> Codegen<'ctx> {
    pub(crate) fn compile_inc_refcount(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        to_generate: PointerValue<'ctx>,
    ) {
        let refc = llvm_context
            .builder()
            .build_struct_gep(
                llvm_context.global_registry().base_heap_allocated(),
                to_generate,
                0,
                "gep_refc_inc",
            )
            .unwrap();
        let refc_val = llvm_context
            .builder()
            .build_load(self.context.i32_type(), refc, "load_refc_inc")
            .unwrap();
        let inc = llvm_context
            .builder()
            .build_int_add(
                refc_val.into_int_value(),
                self.context.i32_type().const_int(1, false),
                "inc_refc",
            )
            .unwrap();
        llvm_context.builder().build_store(refc, inc).unwrap();
    }

    pub(crate) fn compile_struct_dec_refcount(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        func: &FunctionValue<'ctx>,
        struc: &StructSymbol<TypedAST>,
        to_generate: PointerValue<'ctx>,
    ) {
        let tr = llvm_context.type_registry();
        let lowered = tr.get_struct(struc).expect("Unknown struct");
        let refc = llvm_context
            .builder()
            .build_struct_gep(
                llvm_context.global_registry().base_heap_allocated(),
                to_generate,
                0,
                "gep_refc_dec",
            )
            .unwrap();
        let refc_val = llvm_context
            .builder()
            .build_load(self.context.i32_type(), refc, "load_refc_dec")
            .unwrap();
        let dec = llvm_context
            .builder()
            .build_int_sub(
                refc_val.into_int_value(),
                self.context.i32_type().const_int(1, false),
                "dec_refc",
            )
            .unwrap();
        llvm_context.builder().build_store(refc, dec).unwrap();

        let drop_bb = llvm_context.context().append_basic_block(*func, "drop");
        let after_bb = llvm_context.context().append_basic_block(*func, "after");

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
        llvm_context.builder().position_at_end(drop_bb);
        llvm_context
            .builder()
            .build_call(
                lowered.on_drop(),
                &[to_generate.as_basic_value_enum().into()],
                "drop_struct",
            )
            .unwrap();
        llvm_context
            .builder()
            .build_unconditional_branch(after_bb)
            .unwrap();
        llvm_context.builder().position_at_end(after_bb);
    }

    pub(crate) fn compile_enum_dec_refcount(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        func: &FunctionValue<'ctx>,
        enu: &EnumSymbol<TypedAST>,
        to_generate: PointerValue<'ctx>,
    ) {
        let tr = llvm_context.type_registry();
        let lowered = tr.get_enum(enu).expect("Unknown enum");
        let refc = llvm_context
            .builder()
            .build_struct_gep(
                llvm_context.global_registry().base_heap_allocated(),
                to_generate,
                0,
                "gep_refc_dec",
            )
            .unwrap();
        let refc_val = llvm_context
            .builder()
            .build_load(self.context.i32_type(), refc, "load_refc_dec")
            .unwrap();
        let dec = llvm_context
            .builder()
            .build_int_sub(
                refc_val.into_int_value(),
                self.context.i32_type().const_int(1, false),
                "dec_refc",
            )
            .unwrap();
        llvm_context.builder().build_store(refc, dec).unwrap();

        let drop_bb = llvm_context.context().append_basic_block(*func, "drop");
        let after_bb = llvm_context.context().append_basic_block(*func, "after");

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
        llvm_context.builder().position_at_end(drop_bb);
        llvm_context
            .builder()
            .build_call(
                lowered.on_drop(),
                &[to_generate.as_basic_value_enum().into()],
                "drop_struct",
            )
            .unwrap();
        llvm_context
            .builder()
            .build_unconditional_branch(after_bb)
            .unwrap();
        llvm_context.builder().position_at_end(after_bb);
    }

    pub(crate) fn compile_struct_drop(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        func: &FunctionValue<'ctx>,
        struc: &StructSymbol<TypedAST>,
        to_generate: PointerValue<'ctx>,
    ) {
        let tr = llvm_context.type_registry();
        let lowered = tr.get_struct(struc).expect("Unknown struct");

        let after_block = llvm_context
            .context()
            .append_basic_block(*func, "after_block");

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
                    .append_basic_block(*func, "drop_abort_block");

                // Set the refcount to 2
                // This way, it can never be dropped again and can't cause infinite loops
                self.compile_inc_refcount(llvm_context, to_generate);
                self.compile_inc_refcount(llvm_context, to_generate);
                llvm_context
                    .builder()
                    .build_call(
                        predrop,
                        &[to_generate.as_basic_value_enum().into()],
                        "predrop",
                    )
                    .unwrap();

                let refc = llvm_context
                    .builder()
                    .build_struct_gep(
                        llvm_context.global_registry().base_heap_allocated(),
                        to_generate,
                        0,
                        "gep_refc_dec",
                    )
                    .unwrap();
                let refc_val = llvm_context
                    .builder()
                    .build_load(self.context.i32_type(), refc, "load_refc_dec")
                    .unwrap();

                llvm_context
                    .builder()
                    .build_conditional_branch(
                        llvm_context
                            .builder()
                            .build_int_compare(
                                IntPredicate::EQ,
                                refc_val.into_int_value(),
                                llvm_context.context().i32_type().const_int(1, false),
                                "cond",
                            )
                            .unwrap(),
                        after_block,
                        drop_abort_block,
                    )
                    .unwrap();
                llvm_context.builder().position_at_end(drop_abort_block);
                // Remove the ghost refcount
                self.compile_struct_dec_refcount(llvm_context, func, struc, to_generate);
                llvm_context.builder().build_return(None).unwrap();
            }
        }
        llvm_context.builder().position_at_end(after_block);

        for (i, field) in lowered.fields().iter().enumerate() {
            match field.data_type() {
                DataType::Struct(st) => {
                    let field = llvm_context
                        .builder()
                        .build_struct_gep(
                            lowered.lowered(),
                            to_generate,
                            (i + 1) as u32,
                            "gep_field_drop",
                        )
                        .unwrap();
                    self.compile_struct_dec_refcount(llvm_context, func, st, field)
                }
                DataType::Enum(en) => {
                    let field = llvm_context
                        .builder()
                        .build_struct_gep(
                            lowered.lowered(),
                            to_generate,
                            (i + 1) as u32,
                            "gep_field_drop",
                        )
                        .unwrap();
                    self.compile_enum_dec_refcount(llvm_context, func, en, field)
                }
                _ => (),
            }
        }
        llvm_context.builder().build_free(to_generate).unwrap();
    }

    pub(crate) fn compile_enum_drop(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        func: &FunctionValue<'ctx>,
        en: &EnumSymbol<TypedAST>,
        to_generate: PointerValue<'ctx>,
    ) {
        let tr = llvm_context.type_registry();
        let lowered = tr.get_enum(en).expect("Unknown struct");

        let after_block = llvm_context.context().append_basic_block(*func, "after");
        let cond_blocks = lowered
            .variants()
            .iter()
            .map(|variant| llvm_context.context().append_basic_block(*func, "cond"))
            .collect::<Vec<_>>();
        let drop_blocks = lowered
            .variants()
            .iter()
            .map(|variant| llvm_context.context().append_basic_block(*func, "drop"))
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
            llvm_context.builder().position_at_end(*cond);
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
            llvm_context.builder().position_at_end(*drop);
            for (i, field) in variant.0.fields().iter().enumerate() {
                match field {
                    DataType::Struct(st) => {
                        let field = llvm_context
                            .builder()
                            .build_struct_gep(
                                variant.1,
                                to_generate,
                                (i + 2) as u32,
                                "gep_field_drop",
                            )
                            .unwrap();
                        self.compile_struct_dec_refcount(llvm_context, func, st, field)
                    }
                    DataType::Enum(en) => {
                        let field = llvm_context
                            .builder()
                            .build_struct_gep(
                                variant.1,
                                to_generate,
                                (i + 2) as u32,
                                "gep_field_drop",
                            )
                            .unwrap();
                        self.compile_enum_dec_refcount(llvm_context, func, en, field)
                    }
                    _ => (),
                }
            }
            llvm_context
                .builder()
                .build_unconditional_branch(after_block)
                .unwrap();
        }
        llvm_context.builder().position_at_end(after_block);
        llvm_context.builder().build_free(to_generate).unwrap();
    }
}

