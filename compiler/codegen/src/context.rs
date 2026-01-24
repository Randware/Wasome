use std::{collections::HashMap, io::Write};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
};

use crate::types::{CodegenTypes, OptLevel};

pub struct LLVMContext<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    modules: HashMap<String, Module<'ctx>>,
    machine: TargetMachine,
    types: CodegenTypes<'ctx>,
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
        }
    }

    pub fn dump_ir<W: Write>(&self, mut output: W) -> Result<(), std::io::Error> {
        for module in self.modules.values() {
            let ir_string = module.print_to_string();

            output.write_all(ir_string.to_bytes())?;

            output.write_all(b"\n")?;
        }

        Ok(())
    }

    pub fn print_ir(&self) -> Result<(), std::io::Error> {
        self.dump_ir(std::io::stdout())
    }

    pub fn add_module(&mut self, module_name: impl Into<String>) -> Option<Module<'ctx>> {
        let name = module_name.into();
        let module = self.context.create_module(&name);
        module.set_triple(&self.machine.get_triple());
        module.set_data_layout(&self.machine.get_target_data().get_data_layout());
        self.modules.insert(name, module)
    }

    pub fn get_module(&self, name: impl AsRef<str>) -> Option<&Module<'ctx>> {
        self.modules.get(name.as_ref())
    }
}
