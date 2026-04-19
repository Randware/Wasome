use crate::Codegen;
use crate::context::{FunctionContext, LLVMContext, StatementContext};
use crate::symbols::VariableTable;
use ast::TypedAST;
use ast::traversal::function_traversal::FunctionTraversalHelper;

impl<'ctx> Codegen<'ctx> {
    /// Compiles a function body, creating alloca slots for parameters and generating
    /// code for the statement root. Emits a return instruction for non-void functions.
    ///
    /// The compilation process:
    /// 1. Retrieves the LLVM [`FunctionValue`] from the symbol registry
    /// 2. Creates a main basic block and [`FunctionContext`]
    /// 3. For each function parameter:
    ///    - Creates an `alloca` slot
    ///    - Stores the parameter value into the slot
    ///    - Registers the variable in the [`VariableTable`]
    /// 4. Compiles the function body statement
    /// 5. Emits a `return` instruction for non-void functions
    ///     - This is to ensure that all functions return
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lookups and IR operations
    /// * `to_generate` - The function traversal helper containing the function declaration and body
    pub(crate) fn compile_function(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        to_generate: &FunctionTraversalHelper<TypedAST>,
    ) {
        let func = llvm_context
            .type_registry()
            .get_function(to_generate.inner().declaration())
            .unwrap();
        let main_bb = self.context.append_basic_block(func, "main");
        let mut function_context = FunctionContext::new(func, main_bb);
        llvm_context.builder().position_at_end(main_bb);
        let mut vars = VariableTable::new();
        for (i, param) in func.get_param_iter().enumerate() {
            let param_var = to_generate.inner().declaration().params()[i].clone();
            let name = param_var.name();
            param.set_name(name);
            let var = llvm_context
                .builder()
                .build_alloca(llvm_context.lower_type(param_var.data_type()), name)
                .unwrap();

            llvm_context.builder().build_store(var, param).unwrap();

            vars.insert(param_var, var);
        }

        let root = to_generate
            .ref_to_implementation()
            .expect("Compile implementation of external function");

        self.compile_statement(
            llvm_context,
            &mut StatementContext::new(None, &mut function_context),
            &mut vars,
            &root,
        );
        if to_generate.inner().declaration().return_type().is_none() {
            llvm_context.builder().build_return(None).unwrap();
        }
    }
}
