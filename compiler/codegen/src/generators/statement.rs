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
use inkwell::IntPredicate;
use inkwell::values::BasicValue;
use std::ops::Deref;

impl<'ctx, 'fc> Codegen<'ctx> {
    /// Compiles a statement by dispatching to the appropriate handler based on statement type.
    ///
    /// After generating code, drops all variables defined directly in the statement to manage
    /// reference counts for heap-allocated values.
    ///
    /// Supported statement types:
    /// * `VariableAssignment` - Delegates to [`compile_variable_assignment`](Self::compile_variable_assignment)
    /// * `StructFieldAssignment` - Delegates to [`compile_struct_field_assignment`](Self::compile_struct_field_assignment)
    /// * `VariableDeclaration` - Delegates to [`compile_variable_declaration`](Self::compile_variable_declaration)
    /// * `Expression` - Compiles the expression as a statement while discarding its result
    /// * `Return` - Delegates to [`compile_return`](Self::compile_return)
    /// * `ControlStructure` - Delegates to [`compile_control_structure`](Self::compile_control_structure)
    /// * `Codeblock` - Delegates to [`compile_codeblock`](Self::compile_codeblock)
    /// * `VoidFunctionCall` - Delegates to [`compile_void_call`](Self::compile_void_call)
    /// * `Break` - Delegates to [`compile_break`](Self::compile_break)
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lookups and IR operations
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `vars` - The mutable [`VariableTable`] for variable registration and lookups
    /// * `to_generate` - The statement to compile
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
            // We return a DirectlyAvailableSymbol from symbols_defined_directly_in and get_direct_child_only_symbols
            // despite only variables being returned
            // This is to make it more future-proof
            if let DirectlyAvailableSymbol::Variable(var) = var {
                let ptr = vars.lookup(var).expect("Unknown variable");
                self.compile_val_ref_drop(
                    llvm_context,
                    statement_context.function_context_mut(),
                    var.data_type(),
                    ptr,
                );
            }
        }
    }

    /// Compiles a variable assignment, dropping the old value and storing the new one.
    ///
    /// For struct/enum types, the old value's reference count is decremented (and the drop
    /// function is called if it reaches zero). The new value is then compiled and stored.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lookups and IR operations
    /// * `vars` - The [`VariableTable`] containing the variable's alloca pointer
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `to_generate` - The variable assignment to compile
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
        let val =
            self.compile_expression(llvm_context, vars, statement_context, to_generate.value());
        self.compile_val_ref_drop(
            llvm_context,
            statement_context.function_context_mut(),
            to_generate.variable().data_type(),
            var,
        );
        llvm_context.builder().build_store(var, val).unwrap();
    }

    /// Compiles a variable declaration, creating an alloca slot, inserting into the variable table,
    /// and storing the initialized value.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lookups and IR operations
    /// * `vars` - The mutable [`VariableTable`] for registering the new variable
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `to_generate` - The variable declaration to compile
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

    /// Compiles a return statement, optionally compiling the return value expression.
    ///
    /// For void functions, emits a `return` with no value. For non-void functions,
    /// compiles the return expression and emits a `return` with the result.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lookups and IR operations
    /// * `vars` - The [`VariableTable`] for variable lookups
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `to_generate` - The return statement to compile
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

    /// Dispatches to the appropriate control structure handler (conditional, enum variant match, or loop).
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lookups and IR operations
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `vars` - The mutable [`VariableTable`] for variable registration
    /// * `statement` - The statement traversal helper for child access
    /// * `to_generate` - The control structure to compile
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

    /// Compiles an if/else conditional, generating true, false, and after blocks with
    /// conditional branching based on the compiled condition expression.
    ///
    /// Control flow structure:
    /// ```text
    ///     curr_block
    ///        |
    ///        v
    ///     [cond branch] --> true_block --> after_block
    ///        |
    ///        v
    ///     false_block (optional)
    ///        |
    ///        v
    ///     after_block
    /// ```
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lookups and IR operations
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `vars` - The mutable [`VariableTable`] for variable registration
    /// * `statement` - The statement traversal helper for child access
    /// * `to_generate` - The conditional to compile
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
            .build_conditional_branch(cond.into_int_value(), true_block, false_block)
            .unwrap();
        statement_context.set_current_block(llvm_context, after_block);
    }

    /// Compiles a loop (infinite, while, or for), using stack save/restore to prevent excessive
    /// memory use for long loops.
    ///
    /// Control flow structure:
    /// ```text
    ///     curr_block
    ///        |
    ///        v
    ///     cond_block
    ///        |
    ///        +-- infinite: uncond branch to loop_block
    ///        |
    ///        +-- while/for: [cond branch] --> loop_block / after_block
    ///        |
    ///        v
    ///     loop_block
    ///        |
    ///        +-- for: compile after_each statement
    ///        |
    ///        v
    ///     [stacksave]
    ///        |
    ///     body statement
    ///        |
    ///     [stackrestore]
    ///        |
    ///        v
    ///     [uncond branch to cond_block]
    /// ```
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lookups and IR operations
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `vars` - The mutable [`VariableTable`] for variable registration
    /// * `statement` - The statement traversal helper for child access
    /// * `to_generate` - The loop to compile
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
                    .build_conditional_branch(cond.into_int_value(), loop_block, after_block)
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
                    .build_conditional_branch(cond.into_int_value(), loop_block, after_block)
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

    /// Compiles a code block by iterating over all child statements in order.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lookups and IR operations
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `vars` - The mutable [`VariableTable`] for variable registration
    /// * `to_generate` - The statement traversal helper for child access
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

    /// Compiles a break statement by branching to the last breakable block.
    ///
    /// The breakable block is set by [`compile_loop`](Self::compile_loop) via
    /// [`StatementContext::with_last_breakable_block`](StatementContext::with_last_breakable_block).
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for the [`Builder`]
    /// * `statement_context` - The [`StatementContext`] containing the breakable block
    ///
    /// # Panics
    ///
    /// If called outside a loop context (no breakable block set).
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

    /// Compiles a void function call as a statement.
    ///
    /// Delegates to [`compile_call`](Codegen::compile_call) and asserts
    /// that the result is void.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lookups and IR operations
    /// * `vars` - The [`VariableTable`] for variable lookups
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `to_generate` - The function call expression to compile
    pub(crate) fn compile_void_call(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &FunctionCall<TypedAST>,
    ) {
        let call = self.compile_call(llvm_context, vars, statement_context, to_generate);
        debug_assert!(
            call.try_as_basic_value().basic().is_none(),
            "Non-void call as statement"
        );
    }

    /// Compiles a struct field assignment, dropping the old field value and storing the new one.
    ///
    /// For struct/enum field types, the old field value's reference count is decremented
    /// (and the drop function is called if it reaches zero). The new value is then
    /// stored into the field.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lookups and IR operations
    /// * `vars` - The [`VariableTable`] for variable lookups
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `to_generate` - The struct field assignment to compile
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
        self.compile_val_ref_drop(
            llvm_context,
            statement_context.function_context_mut(),
            to_generate.struct_field().data_type(),
            field,
        );
        llvm_context.builder().build_store(field, val).unwrap();
    }

    /// Compiles an if-enum-variant pattern match, loading the discriminant tag and branching
    /// to the matching variant's code block. Creates alloca slots for each variant field.
    ///
    /// Control flow structure:
    /// ```text
    ///     [load enum pointer]
    ///        |
    ///        v
    ///     [load discriminant tag]
    ///        |
    ///        v
    ///     [tag == expected?] --> yes --> match_block
    ///        |                            |
    ///        | (no)                       | [create alloca for each field]
    ///        v                            | [load each field value]
    ///     after_block                      | [store into alloca]
    ///        ^                            | [compile body statement]
    ///        |____________________________|
    ///        |
    ///        v
    ///     [decrement enum refcount]
    /// ```
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lookups and IR operations
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `vars` - The mutable [`VariableTable`] for registering variant field variables
    /// * `statement` - The statement traversal helper for child access
    /// * `to_generate` - The enum variant match to compile
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
