use crate::Codegen;
use crate::context::{LLVMContext, StatementContext};
use crate::symbols::VariableTable;
use ast::TypedAST;
use ast::traversal::function_traversal::FunctionTraversalHelper;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn compile_function(
        &mut self,
        llvm_context: &mut LLVMContext<'ctx>,
        to_generate: &FunctionTraversalHelper<TypedAST>,
    ) {
        let func = llvm_context
            .type_registry()
            .get_function(to_generate.inner().declaration())
            .unwrap();
        let main_bb = self.context.append_basic_block(func, "main");
        llvm_context.builder().position_at_end(main_bb);
        let mut vars = VariableTable::new();
        for (i, param) in func.get_param_iter().enumerate() {
            let param_var = to_generate.inner().declaration().params()[i].clone();
            let name = param_var.name();
            param.set_name(name);
            let var = llvm_context
                .builder()
                .build_alloca(
                    llvm_context.lower_type(param_var.data_type()).unwrap(),
                    name,
                )
                .unwrap();

            llvm_context.builder().build_store(var, param).unwrap();

            vars.insert(param_var, var);
        }

        let root = to_generate
            .ref_to_implementation()
            .expect("Compile implementation of external function");

        self.compile_statement(
            llvm_context,
            &StatementContext::new(None, func),
            &mut vars,
            &root,
        );
        if to_generate.inner().declaration().return_type().is_none() {
            llvm_context.builder().build_return(None).unwrap();
        }
    }
}
