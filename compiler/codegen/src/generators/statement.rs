use crate::Codegen;
use crate::context::{LLVMContext, StatementContext};
use crate::symbols::VariableTable;
use ast::TypedAST;
use ast::expression::FunctionCall;
use ast::statement::{
    Conditional, ControlStructure, Loop, LoopType, Return, Statement, VariableAssignment,
    VariableDeclaration,
};
use ast::traversal::statement_traversal::StatementTraversalHelper;
use inkwell::IntPredicate;
use inkwell::values::BasicValue;
use std::ops::Deref;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn compile_statement(
        &mut self,
        llvm_context: &mut LLVMContext<'ctx>,
        statement_context: &StatementContext<'ctx>,
        vars: &mut VariableTable<'ctx>,
        to_generate: &StatementTraversalHelper<TypedAST>,
    ) {
        match to_generate.inner().deref() {
            Statement::VariableAssignment(va) => {
                self.compile_variable_assignment(llvm_context, vars, va)
            }
            Statement::StructFieldAssignment(sfa) => todo!(),
            Statement::VariableDeclaration(vd) => {
                self.compile_variable_declaration(llvm_context, vars, vd)
            }
            Statement::Expression(expr) => {
                self.compile_expression(llvm_context, vars, expr);
            }
            Statement::Return(ret) => self.compile_return(llvm_context, vars, ret),
            Statement::ControlStructure(contrl) => self.compile_control_structure(
                llvm_context,
                statement_context,
                vars,
                to_generate,
                contrl,
            ),
            Statement::Codeblock(_) => {
                self.compile_codeblock(llvm_context, statement_context, vars, to_generate)
            }
            Statement::VoidFunctionCall(vc) => self.compile_void_call(llvm_context, vars, vc),
            Statement::Break => self.compile_break(llvm_context, statement_context),
        }
    }

    pub(crate) fn compile_variable_assignment(
        &mut self,
        llvm_context: &mut LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        to_generate: &VariableAssignment<TypedAST>,
    ) {
        let var = vars
            .lookup(to_generate.variable())
            .expect("Assign to undeclared variable");
        let val = self.compile_expression(llvm_context, vars, to_generate.value());
        llvm_context
            .builder()
            .build_store(var.pointer, val.into_basic_value_enum())
            .unwrap();
    }

    pub(crate) fn compile_variable_declaration(
        &mut self,
        llvm_context: &mut LLVMContext<'ctx>,
        vars: &mut VariableTable<'ctx>,
        to_generate: &VariableDeclaration<TypedAST>,
    ) {
        let prt = llvm_context
            .builder()
            .build_alloca(
                llvm_context
                    .lower_type(to_generate.variable().data_type())
                    .expect("Unregistered type"),
                to_generate.variable().name(),
            )
            .unwrap();
        vars.insert(to_generate.variable_owned(), prt);
        let val = self.compile_expression(llvm_context, vars, to_generate.value());
        llvm_context
            .builder()
            .build_store(prt, val.into_basic_value_enum())
            .unwrap();
    }

    pub(crate) fn compile_return(
        &mut self,
        llvm_context: &mut LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        to_generate: &Return<TypedAST>,
    ) {
        let to_ret = to_generate.to_return().map(|to_ret| {
            self.compile_expression(llvm_context, vars, to_ret)
                .into_basic_value_enum()
        });
        llvm_context
            .builder()
            .build_return(to_ret.as_ref().map(|val| val as &dyn BasicValue))
            .unwrap();
    }

    pub(crate) fn compile_control_structure(
        &mut self,
        llvm_context: &mut LLVMContext<'ctx>,
        statement_context: &StatementContext<'ctx>,
        vars: &mut VariableTable<'ctx>,
        statement: &StatementTraversalHelper<TypedAST>,
        to_generate: &ControlStructure<TypedAST>,
    ) {
        match to_generate {
            ControlStructure::Conditional(cond) => {
                self.compile_conditional(llvm_context, statement_context, vars, statement, cond)
            }
            ControlStructure::IfEnumVariant(_) => todo!(),
            ControlStructure::Loop(loop_inner) => {
                self.compile_loop(llvm_context, statement_context, vars, statement, loop_inner)
            }
        }
    }

    pub(crate) fn compile_conditional(
        &mut self,
        llvm_context: &mut LLVMContext<'ctx>,
        statement_context: &StatementContext<'ctx>,
        vars: &mut VariableTable<'ctx>,
        statement: &StatementTraversalHelper<TypedAST>,
        to_generate: &Conditional<TypedAST>,
    ) {
        let cond = self.compile_expression(llvm_context, vars, to_generate.condition());

        let curr_block = statement_context
            .current_function()
            .get_last_basic_block()
            .expect("There are no blocks");
        let true_block = self
            .context
            .append_basic_block(statement_context.current_function(), "true");
        let after_block = self
            .context
            .append_basic_block(statement_context.current_function(), "after");
        let false_block = match to_generate.else_statement() {
            None => curr_block,
            Some(_) => {
                let false_block = self
                    .context
                    .append_basic_block(statement_context.current_function(), "false");
                llvm_context.builder().position_at_end(false_block);
                self.compile_statement(
                    llvm_context,
                    statement_context,
                    vars,
                    &statement.get_child(1).expect("There is no else block"),
                );
                llvm_context
                    .builder()
                    .build_unconditional_branch(after_block)
                    .unwrap();
                llvm_context.builder().position_at_end(curr_block);
                false_block
            }
        };
        llvm_context.builder().position_at_end(true_block);
        self.compile_statement(
            llvm_context,
            statement_context,
            vars,
            &statement.get_child(0).expect("There is no then block"),
        );
        llvm_context
            .builder()
            .build_unconditional_branch(after_block)
            .unwrap();
        llvm_context.builder().position_at_end(curr_block);

        llvm_context
            .builder()
            .build_conditional_branch(
                llvm_context
                    .builder()
                    .build_int_compare(
                        IntPredicate::EQ,
                        cond.into_bool(),
                        llvm_context.context().bool_type().const_zero(),
                        "cond",
                    )
                    .unwrap(),
                false_block,
                true_block,
            )
            .unwrap();
        llvm_context.builder().position_at_end(after_block);
    }

    pub(crate) fn compile_loop(
        &mut self,
        llvm_context: &mut LLVMContext<'ctx>,
        statement_context: &StatementContext<'ctx>,
        vars: &mut VariableTable<'ctx>,
        statement: &StatementTraversalHelper<TypedAST>,
        to_generate: &Loop<TypedAST>,
    ) {
        let curr_block = statement_context
            .current_function()
            .get_last_basic_block()
            .expect("There are no blocks");
        let cond_block = self
            .context
            .append_basic_block(statement_context.current_function(), "cond");
        let loop_block = self
            .context
            .append_basic_block(statement_context.current_function(), "loop");
        let after_block = self
            .context
            .append_basic_block(statement_context.current_function(), "after");
        let statement_context = statement_context.with_last_breakable_block(after_block);

        llvm_context.builder().position_at_end(loop_block);
        self.compile_statement(
            llvm_context,
            &statement_context,
            vars,
            &statement
                .get_child(to_generate.to_loop_on_index())
                .expect("To loop on does not exist!"),
        );
        llvm_context.builder().position_at_end(cond_block);

        match to_generate.loop_type() {
            LoopType::Infinite => {
                llvm_context
                    .builder()
                    .build_unconditional_branch(loop_block)
                    .unwrap();
            }
            LoopType::While(cond) => {
                let cond = self.compile_expression(llvm_context, vars, cond);
                llvm_context
                    .builder()
                    .build_conditional_branch(
                        llvm_context
                            .builder()
                            .build_int_compare(
                                IntPredicate::EQ,
                                cond.into_bool(),
                                llvm_context.context().bool_type().const_zero(),
                                "cond",
                            )
                            .unwrap(),
                        after_block,
                        loop_block,
                    )
                    .unwrap();
            }
            LoopType::For {
                start: _,
                cond,
                after_each: _after_each,
            } => {
                llvm_context.builder().position_at_end(curr_block);
                self.compile_statement(
                    llvm_context,
                    &statement_context,
                    vars,
                    &statement.get_child(0).expect("There is no start block!"),
                );
                llvm_context.builder().position_at_end(cond_block);
                let cond = self.compile_expression(llvm_context, vars, cond);
                llvm_context
                    .builder()
                    .build_conditional_branch(
                        llvm_context
                            .builder()
                            .build_int_compare(
                                IntPredicate::EQ,
                                cond.into_bool(),
                                llvm_context.context().bool_type().const_zero(),
                                "cond",
                            )
                            .unwrap(),
                        after_block,
                        loop_block,
                    )
                    .unwrap();
                llvm_context.builder().position_at_end(loop_block);
                self.compile_statement(
                    llvm_context,
                    &statement_context,
                    vars,
                    &statement
                        .get_child(2)
                        .expect("To after each does not exist!"),
                );
            }
        }

        llvm_context.builder().position_at_end(loop_block);
        llvm_context
            .builder()
            .build_unconditional_branch(cond_block)
            .unwrap();

        llvm_context.builder().position_at_end(curr_block);
        llvm_context
            .builder()
            .build_unconditional_branch(cond_block)
            .unwrap();
        llvm_context.builder().position_at_end(after_block);
    }

    pub(crate) fn compile_codeblock(
        &mut self,
        llvm_context: &mut LLVMContext<'ctx>,
        statement_context: &StatementContext<'ctx>,
        vars: &mut VariableTable<'ctx>,
        to_generate: &StatementTraversalHelper<TypedAST>,
    ) {
        let mut index = 0;
        while let Some(statement) = to_generate.get_child(index) {
            index += 1;
            self.compile_statement(llvm_context, &statement_context, vars, &statement);
        }
    }

    pub(crate) fn compile_break(
        &mut self,
        llvm_context: &mut LLVMContext<'ctx>,
        statement_context: &StatementContext<'ctx>,
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
        llvm_context: &mut LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
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
                self.compile_expression(llvm_context, vars, arg)
                    .into_basic_value_enum()
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
                .is_some(),
            "Non-void call as statement"
        );
    }
}
