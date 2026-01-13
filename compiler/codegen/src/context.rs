use std::io::Write;

use inkwell::{
    OptimizationLevel,
    builder::Builder,
    context::Context,
    module::Module,
    targets::{
        CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetMachineOptions,
        TargetTriple,
    },
};

use crate::{temp::Cli, types::CodegenTypes};

pub struct LLVMContext<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    machine: TargetMachine,
    types: CodegenTypes<'ctx>,
}

impl<'ctx> LLVMContext<'ctx> {
    pub fn new(context: &'ctx Context, config: Cli) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("wasome_module");

        Target::initialize_webassembly(&InitializationConfig::default());

        let triple = TargetTriple::create("wasm32-unknown-unknown");
        let target = Target::from_triple(&triple).expect("Failed to get WASM target.");
        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                config.opt_level.into(),
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();
        let layout = machine.get_target_data().get_data_layout();

        module.set_triple(&triple);
        module.set_data_layout(&layout);
        let types = CodegenTypes::new(context, &layout);

        Self {
            context,
            builder,
            module,
            machine,
            types,
        }
    }

    pub fn dumb_ir<W: Write>(&self, mut output: W) -> Result<(), std::io::Error> {
        output.write_all(self.module.print_to_string().to_bytes())
    }

    pub fn print_ir(&self) -> Result<(), std::io::Error> {
        self.dumb_ir(std::io::stdout())
    }
}
