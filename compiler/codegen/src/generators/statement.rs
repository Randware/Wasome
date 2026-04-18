use crate::Codegen;
use crate::context::{LLVMContext, StatementContext};
use crate::symbols::VariableTable;
use ast::TypedAST;
use ast::data_type::{DataType, Typed};
use ast::expression::FunctionCall;
use ast::statement::{
    Conditional, ControlStructure, IfEnumVariant, Loop, LoopType, Return, Statement,
    StructFieldAssignment, VariableAssignment, VariableDeclaration,
};
use ast::symbol::DirectlyAvailableSymbol;
use ast::traversal::statement_traversal::StatementTraversalHelper;
use inkwell::values::BasicValue;
use inkwell::{AddressSpace, IntPredicate};
use std::ops::Deref;

impl<'ctx, 'fc> Codegen<'ctx> {
    pub(crate) fn compile_statement(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        vars: &mut VariableTable<'ctx>,
        to_generate: &StatementTraversalHelper<TypedAST>,
    ) {
        match to_generate.inner().deref() {
            Statement::VariableAssignment(va) => {
                self.compile_variable_assignment(llvm_context, vars, statement_context, va);
            }
            Statement::StructFieldAssignment(sfa) => {
                self.compile_struct_field_assignment(llvm_context, vars, statement_context, sfa);
            }
            Statement::VariableDeclaration(vd) => {
                self.compile_variable_declaration(llvm_context, vars, statement_context, vd);
            }
            Statement::Expression(expr) => {
                self.compile_expression(llvm_context, vars, statement_context, expr);
            }
            Statement::Return(ret) => {
                self.compile_return(llvm_context, vars, statement_context, ret);
            }
            Statement::ControlStructure(contrl) => self.compile_control_structure(
                llvm_context,
                statement_context,
                vars,
                to_generate,
                contrl,
            ),
            Statement::Codeblock(_) => {
                self.compile_codeblock(llvm_context, statement_context, vars, to_generate);
            }
            Statement::VoidFunctionCall(vc) => {
                self.compile_void_call(llvm_context, vars, statement_context, vc);
            }
            Statement::Break => Codegen::compile_break(llvm_context, statement_context),
        }
        for var in to_generate
            .symbols_defined_directly_in()
            .into_iter()
            .chain(to_generate.inner().get_direct_child_only_symbols())
        {
            if let DirectlyAvailableSymbol::Variable(var) = var {
                let ptr = vars.lookup(var).expect("Unknown variable");
                self.compile_val_ref_drop(llvm_context, statement_context.function_context_mut(), var.data_type(), ptr.pointer);
            }
        }
    }

    pub(crate) fn compile_variable_assignment(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &VariableAssignment<TypedAST>,
    ) {
        let var = vars
            .lookup(to_generate.variable())
            .expect("Assign to undeclared variable");
        match to_generate.variable().data_type() {
            DataType::Struct(st) => {
                let to_drop = llvm_context
                    .builder()
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        var.pointer,
                        "drop_load",
                    )
                    .unwrap();
                self.compile_struct_dec_refcount(
                    llvm_context,
                    statement_context.function_context_mut(),
                    st,
                    to_drop.into_pointer_value(),
                );
            }
            DataType::Enum(en) => {
                let to_drop = llvm_context
                    .builder()
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        var.pointer,
                        "drop_load",
                    )
                    .unwrap();
                self.compile_enum_dec_refcount(
                    llvm_context,
                    statement_context.function_context_mut(),
                    en,
                    to_drop.into_pointer_value(),
                );
            }
            _ => (),
        }
        let val =
            self.compile_expression(llvm_context, vars, statement_context, to_generate.value());
        llvm_context
            .builder()
            .build_store(var.pointer, val)
            .unwrap();
    }

    pub(crate) fn compile_variable_declaration(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &mut VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &VariableDeclaration<TypedAST>,
    ) {
        let prt = llvm_context
            .builder()
            .build_alloca(
                llvm_context.lower_type(to_generate.variable().data_type()),
                to_generate.variable().name(),
            )
            .unwrap();
        vars.insert(to_generate.variable_owned(), prt);
        let val =
            self.compile_expression(llvm_context, vars, statement_context, to_generate.value());
        llvm_context.builder().build_store(prt, val).unwrap();
    }

    pub(crate) fn compile_return(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &Return<TypedAST>,
    ) {
        let to_ret = to_generate
            .to_return()
            .map(|to_ret| self.compile_expression(llvm_context, vars, statement_context, to_ret));
        llvm_context
            .builder()
            .build_return(to_ret.as_ref().map(|val| val as &dyn BasicValue))
            .unwrap();
    }

    pub(crate) fn compile_control_structure(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        vars: &mut VariableTable<'ctx>,
        statement: &StatementTraversalHelper<TypedAST>,
        to_generate: &ControlStructure<TypedAST>,
    ) {
        match to_generate {
            ControlStructure::Conditional(cond) => {
                self.compile_conditional(llvm_context, statement_context, vars, statement, cond);
            }
            ControlStructure::IfEnumVariant(iev) => {
                self.compile_if_enum_variant(llvm_context, statement_context, vars, statement, iev);
            }
            ControlStructure::Loop(loop_inner) => {
                self.compile_loop(llvm_context, statement_context, vars, statement, loop_inner);
            }
        }
    }

    pub(crate) fn compile_conditional(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        vars: &mut VariableTable<'ctx>,
        statement: &StatementTraversalHelper<TypedAST>,
        to_generate: &Conditional<TypedAST>,
    ) {
        let cond = self.compile_expression(
            llvm_context,
            vars,
            statement_context,
            to_generate.condition(),
        );

        let curr_block = statement_context.function_context().current_block();
        let true_block = self.context.append_basic_block(
            statement_context.function_context().current_function(),
            "true",
        );
        let after_block = self.context.append_basic_block(
            statement_context.function_context().current_function(),
            "after",
        );
        let false_block = if to_generate.else_statement().is_none() {
            after_block
        } else {
            let false_block = self.context.append_basic_block(
                statement_context.function_context().current_function(),
                "false",
            );
            statement_context.set_current_block(llvm_context, false_block);
            let false_statement = statement.get_child(1).expect("There is no else block");
            self.compile_statement(llvm_context, statement_context, vars, &false_statement);
            llvm_context
                .builder()
                .build_unconditional_branch(after_block)
                .unwrap();
            false_block
        };
        statement_context.set_current_block(llvm_context, true_block);
        let true_statement = statement.get_child(0).expect("There is no then block");
        self.compile_statement(llvm_context, statement_context, vars, &true_statement);
        llvm_context
            .builder()
            .build_unconditional_branch(after_block)
            .unwrap();
        statement_context.set_current_block(llvm_context, curr_block);

        llvm_context
            .builder()
            .build_conditional_branch(
                cond.into_int_value(),
                true_block,
                false_block,
            )
            .unwrap();
        statement_context.set_current_block(llvm_context, after_block);
    }

    pub(crate) fn compile_loop(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        vars: &mut VariableTable<'ctx>,
        statement: &StatementTraversalHelper<TypedAST>,
        to_generate: &Loop<TypedAST>,
    ) {
        let curr_block = statement_context.function_context().current_block();
        let cond_block = self.context.append_basic_block(
            statement_context.function_context().current_function(),
            "cond",
        );
        let loop_block = self.context.append_basic_block(
            statement_context.function_context().current_function(),
            "loop",
        );
        let after_block = self.context.append_basic_block(
            statement_context.function_context().current_function(),
            "after",
        );

        statement_context.set_current_block(llvm_context, cond_block);

        match to_generate.loop_type() {
            LoopType::Infinite => {
                llvm_context
                    .builder()
                    .build_unconditional_branch(loop_block)
                    .unwrap();
            }
            LoopType::While(cond) => {
                let cond = self.compile_expression(llvm_context, vars, statement_context, cond);
                llvm_context
                    .builder()
                    .build_conditional_branch(
                        cond.into_int_value(),
                        loop_block,
                        after_block,
                    )
                    .unwrap();
            }
            LoopType::For {
                start: _,
                cond,
                after_each: _after_each,
            } => {
                statement_context.set_current_block(llvm_context, curr_block);
                self.compile_statement(
                    llvm_context,
                    statement_context,
                    vars,
                    &statement.get_child(0).expect("There is no start block!"),
                );
                llvm_context
                    .builder()
                    .build_unconditional_branch(cond_block)
                    .unwrap();
                statement_context.set_current_block(llvm_context, cond_block);
                let cond = self.compile_expression(llvm_context, vars, statement_context, cond);
                llvm_context
                    .builder()
                    .build_conditional_branch(
                        cond.into_int_value(),
                        loop_block,
                        after_block,
                    )
                    .unwrap();
            }
        }

        statement_context.set_current_block(llvm_context, loop_block);
        if matches!(to_generate.loop_type(), LoopType::For { .. }) {
            self.compile_statement(
                llvm_context,
                statement_context,
                vars,
                &statement
                    .get_child(2)
                    .expect("To after each does not exist!"),
            );
        } else {
            statement_context.set_current_block(llvm_context, curr_block);
            llvm_context
                .builder()
                .build_unconditional_branch(cond_block)
                .unwrap();
            statement_context.set_current_block(llvm_context, loop_block);
        }
        let to_loop_on = statement
            .get_child(to_generate.to_loop_on_index())
            .expect("To loop on does not exist!");
        let mut inner_statement_context = statement_context.with_last_breakable_block(after_block);
        let sf = llvm_context
            .builder()
            .build_call(llvm_context.global_registry().stacksave(), &[], "stacksave")
            .unwrap()
            .try_as_basic_value()
            .unwrap_basic();
        self.compile_statement(
            llvm_context,
            &mut inner_statement_context,
            vars,
            &to_loop_on,
        );
        llvm_context
            .builder()
            .build_call(
                llvm_context.global_registry().stackrestore(),
                &[sf.into()],
                "stackrestore",
            )
            .unwrap();
        llvm_context
            .builder()
            .build_unconditional_branch(cond_block)
            .unwrap();

        statement_context.set_current_block(llvm_context, after_block);
    }

    pub(crate) fn compile_codeblock(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        vars: &mut VariableTable<'ctx>,
        to_generate: &StatementTraversalHelper<TypedAST>,
    ) {
        let mut index = 0;
        while let Some(statement) = to_generate.get_child(index) {
            index += 1;
            self.compile_statement(llvm_context, statement_context, vars, &statement);
        }
    }

    pub(crate) fn compile_break(
        llvm_context: &LLVMContext<'ctx>,
        statement_context: &StatementContext<'ctx, 'fc>,
    ) {
        llvm_context
            .builder()
            .build_unconditional_branch(
                statement_context
                    .last_breakable_block()
                    .expect("Break outside loop"),
            )
            .unwrap();
    }

    pub(crate) fn compile_void_call(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &FunctionCall<TypedAST>,
    ) {
        let func = llvm_context
            .type_registry()
            .get_function(to_generate.function())
            .expect("Call to unknown function!");
        let args = to_generate
            .args()
            .iter()
            .map(|arg| {
                self.compile_expression(llvm_context, vars, statement_context, arg)
                    .into()
            })
            .collect::<Vec<_>>();
        debug_assert!(
            llvm_context
                .builder()
                .build_call(func, &args, "call")
                .unwrap()
                .try_as_basic_value()
                .basic()
                .is_none(),
            "Non-void call as statement"
        );
        for arg in args.iter().zip(to_generate.args()) {
            match arg.1.data_type() {
                DataType::Struct(st) => self.compile_struct_dec_refcount(
                    llvm_context,
                    statement_context.function_context_mut(),
                    &st,
                    arg.0.into_pointer_value(),
                ),
                DataType::Enum(en) => self.compile_enum_dec_refcount(
                    llvm_context,
                    statement_context.function_context_mut(),
                    &en,
                    arg.0.into_pointer_value(),
                ),
                _ => (),
            }
        }
    }

    pub(crate) fn compile_struct_field_assignment(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &StructFieldAssignment<TypedAST>,
    ) {
        let of = self
            .compile_expression(
                llvm_context,
                vars,
                statement_context,
                to_generate.struct_source(),
            )
            .into_pointer_value();
        let DataType::Struct(struct_type) = to_generate.struct_source().data_type() else {
            unreachable!()
        };
        let tr = llvm_context.type_registry();
        let struct_type = tr.get_struct(&struct_type).expect("Unknown struct");
        let struct_field = struct_type
            .fields()
            .iter()
            .position(|field| field == to_generate.struct_field())
            .expect("Unknown field");
        let field = llvm_context
            .builder()
            .build_struct_gep(
                struct_type.lowered(),
                of,
                u32::try_from(struct_field).unwrap() + 1,
                "field_access_gep",
            )
            .expect("Unknown struct field");
        let val =
            self.compile_expression(llvm_context, vars, statement_context, to_generate.value());
        self.compile_val_ref_drop(llvm_context, statement_context.function_context_mut(), to_generate.struct_field().data_type(), field);
        llvm_context.builder().build_store(field, val).unwrap();
    }

    pub(crate) fn compile_if_enum_variant(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        vars: &mut VariableTable<'ctx>,
        statement: &StatementTraversalHelper<TypedAST>,
        to_generate: &IfEnumVariant<TypedAST>,
    ) {
        let of = self
            .compile_expression(
                llvm_context,
                vars,
                statement_context,
                to_generate.assignment_expression(),
            )
            .into_pointer_value();
        let enum_type = to_generate.condition_enum();
        let tr = llvm_context.type_registry();
        let enum_type_lowered = tr.get_enum(enum_type).expect("Unregistered enum");
        let expected_tag = enum_type_lowered
            .index_of(to_generate.condition_enum_variant())
            .expect("Unknown enum variant");
        let variant = enum_type_lowered
            .lookup(to_generate.condition_enum_variant())
            .expect("Unknown enum variant");

        let match_block = self.context.append_basic_block(
            statement_context.function_context().current_function(),
            "match",
        );
        let after_block = self.context.append_basic_block(
            statement_context.function_context().current_function(),
            "after",
        );

        let tag = llvm_context
            .builder()
            .build_struct_gep(llvm_context.global_registry().base_enum(), of, 1, "tag_gep")
            .expect("Enum must have tag");
        let tag = llvm_context
            .builder()
            .build_load(self.context.i32_type(), tag, "load_load")
            .unwrap();

        llvm_context
            .builder()
            .build_conditional_branch(
                llvm_context
                    .builder()
                    .build_int_compare(
                        IntPredicate::EQ,
                        tag.into_int_value(),
                        llvm_context
                            .context()
                            .i32_type()
                            .const_int(expected_tag as u64, false),
                        "cond",
                    )
                    .unwrap(),
                match_block,
                after_block,
            )
            .unwrap();

        statement_context.set_current_block(llvm_context, match_block);

        for (i, var) in to_generate.variables().iter().enumerate() {
            let prt = llvm_context
                .builder()
                .build_alloca(llvm_context.lower_type(var.data_type()), var.name())
                .unwrap();
            vars.insert(var.clone(), prt);
            let val = llvm_context
                .builder()
                .build_struct_gep(variant, of, u32::try_from(i + 2).unwrap(), "enum_field_gep")
                .expect("Unknown enum field");
            let val = llvm_context
                .builder()
                .build_load(llvm_context.lower_type(var.data_type()), val, "load_load")
                .unwrap();

            llvm_context.builder().build_store(prt, val).unwrap();
        }
        self.compile_statement(
            llvm_context,
            statement_context,
            vars,
            &statement.get_child(0).expect("There is no then statement"),
        );
        llvm_context
            .builder()
            .build_unconditional_branch(after_block)
            .unwrap();
        statement_context.set_current_block(llvm_context, after_block);
        self.compile_enum_dec_refcount(
            llvm_context,
            statement_context.function_context_mut(),
            enum_type,
            of,
        );
    }
}
