use crate::global_registry::GlobalRegistry;
use crate::{
    errors::CodegenError,
    symbols::SymbolRegistry,
    types::{CodegenTypes, ModuleContext, OptLevel},
};
use ast::data_type::DataType;
use inkwell::basic_block::BasicBlock;
use inkwell::values::FunctionValue;
use inkwell::{
    builder::Builder,
    context::Context,
    passes::PassBuilderOptions,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
    types::{BasicType, BasicTypeEnum},
};
use std::cell::{Ref, RefCell, RefMut};
use std::{collections::HashMap, io::Write};

pub struct LLVMContext<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    modules: HashMap<String, ModuleContext<'ctx>>,
    machine: TargetMachine,
    registry: RefCell<SymbolRegistry<'ctx>>,
    types: CodegenTypes<'ctx>,
    opt_level: OptLevel,
    global_registry: GlobalRegistry<'ctx>,
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

        let global_registry = GlobalRegistry::new(context);
        Self {
            context,
            builder,
            modules: HashMap::new(),
            machine,
            registry: RefCell::new(SymbolRegistry::new()),
            types,
            opt_level,
            global_registry,
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

    pub fn lower_type(
        &self,
        data_type: &DataType,
    ) -> Result<BasicTypeEnum<'ctx>, CodegenError<'ctx>> {
        Ok(match data_type {
            DataType::Char => self.context.i32_type().as_basic_type_enum(),
            DataType::U8 => self.context.i8_type().as_basic_type_enum(),
            DataType::S8 => self.context.i8_type().as_basic_type_enum(),
            DataType::U16 => self.context.i16_type().as_basic_type_enum(),
            DataType::S16 => self.context.i16_type().as_basic_type_enum(),
            DataType::U32 => self.context.i32_type().as_basic_type_enum(),
            DataType::S32 => self.context.i32_type().as_basic_type_enum(),
            DataType::U64 => self.context.i64_type().as_basic_type_enum(),
            DataType::S64 => self.context.i64_type().as_basic_type_enum(),
            DataType::Bool => self.context.bool_type().as_basic_type_enum(),
            DataType::F32 => self.context.f32_type().as_basic_type_enum(),
            DataType::F64 => self.context.f32_type().as_basic_type_enum(),
            DataType::Struct(_) | DataType::Enum(_) => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .as_basic_type_enum(),
        })
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

    pub fn type_registry(&self) -> Ref<SymbolRegistry<'ctx>> {
        self.registry.borrow()
    }
    pub fn type_registry_mut(&self) -> RefMut<SymbolRegistry<'ctx>> {
        self.registry.borrow_mut()
    }

    pub fn types(&self) -> &CodegenTypes<'ctx> {
        &self.types
    }

    pub fn global_registry(&self) -> &GlobalRegistry<'ctx> {
        &self.global_registry
    }
}

pub(crate) struct StatementContext<'ctx> {
    last_breakable_block: Option<BasicBlock<'ctx>>,
    current_function: FunctionValue<'ctx>,
}

impl<'ctx> StatementContext<'ctx> {
    pub fn new(
        last_breakable_block: Option<BasicBlock<'ctx>>,
        current_function: FunctionValue<'ctx>,
    ) -> Self {
        Self {
            last_breakable_block,
            current_function,
        }
    }

    pub fn last_breakable_block(&self) -> Option<BasicBlock<'ctx>> {
        self.last_breakable_block
    }

    pub fn current_function(&self) -> FunctionValue<'ctx> {
        self.current_function
    }

    pub fn with_last_breakable_block(&self, last_breakable_block: BasicBlock<'ctx>) -> Self {
        Self {
            last_breakable_block: Some(last_breakable_block),
            current_function: self.current_function,
        }
    }
}
