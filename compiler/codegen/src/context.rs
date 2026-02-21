use std::{collections::HashMap, io::Write};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassBuilderOptions,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
};

use crate::types::{CodegenTypes, ModuleContext, OptLevel};

pub struct LLVMContext<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    modules: HashMap<String, ModuleContext<'ctx>>,
    machine: TargetMachine,
    types: CodegenTypes<'ctx>,
    opt_level: OptLevel,
}

impl<'ctx> LLVMContext<'ctx> {
    pub fn new(context: &'ctx Context, opt_level: OptLevel) -> Self {
        let builder = context.create_builder();

        Target::initialize_webassembly(&InitializationConfig::default());

        let triple = TargetTriple::create("wasm32-unknown-unknown");
        let target = Target::from_triple(&triple).expect("Failed to get WASM target.");
        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                opt_level.into(),
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();
        let layout = machine.get_target_data().get_data_layout();

        let types = CodegenTypes::new(context, &layout);

        Self {
            context,
            builder,
            modules: HashMap::new(),
            machine,
            types,
            opt_level,
        }
    }

    pub fn dump_ir<W: Write>(&self, mut output: W) -> Result<(), std::io::Error> {
        for module in self.modules.values() {
            let ir_string = module.inner.print_to_string();

            output.write_all(ir_string.to_bytes())?;

            output.write_all(b"\n")?;
        }

        Ok(())
    }

    pub fn print_ir(&self) -> Result<(), std::io::Error> {
        self.dump_ir(std::io::stdout())
    }

    /// Adds a module.
    ///
    /// If the module with that name is not present, [`None`] is returned.
    ///
    /// If the there is a module with the same name, the module is updated, and the old
    /// modules is returned.
    pub fn add_module(&mut self, module_name: impl Into<String>) -> Option<ModuleContext<'ctx>> {
        let name = module_name.into();
        let module = self.context.create_module(&name);
        module.set_triple(&self.machine.get_triple());
        module.set_data_layout(&self.machine.get_target_data().get_data_layout());

        self.modules.insert(name, module.into())
    }

    pub fn apply_passes(&mut self) {
        self.modules.values_mut().for_each(|module| {
            let passes = PassBuilderOptions::create();

            // Explicitly lock down expensive passes at O0
            if self.opt_level == OptLevel::O0 {
                passes.set_loop_vectorization(false);
                passes.set_loop_unrolling(false);
                passes.set_loop_interleaving(false);
            }

            module
                .inner
                .run_passes(self.opt_level.as_llvm_pipeline(), &self.machine, passes)
                .expect("Could not run passes");
        });
    }

    pub fn get_module(&self, name: impl AsRef<str>) -> Option<&ModuleContext<'ctx>> {
        self.modules.get(name.as_ref())
    }

    pub fn context(&self) -> &'ctx Context {
        self.context
    }

    pub fn builder(&self) -> &Builder<'ctx> {
        &self.builder
    }

    pub fn machine(&self) -> &TargetMachine {
        &self.machine
    }

    pub fn types(&self) -> &CodegenTypes<'ctx> {
        &self.types
    }
}
