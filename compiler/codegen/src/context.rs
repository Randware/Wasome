use crate::global_registry::GlobalRegistry;
use crate::symbols::SymbolRegistry;
use crate::types::{CodegenTypes, OptLevel};
use ast::data_type::DataType;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::FunctionValue;
use std::cell::{Ref, RefCell, RefMut};
use std::io::Write;

pub struct LLVMContext<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
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

        let module = context.create_module("wasome");

        let global_registry = GlobalRegistry::new(context, &module);

        module.set_triple(&machine.get_triple());
        module.set_data_layout(&machine.get_target_data().get_data_layout());
        Self {
            context,
            builder,
            module,
            machine,
            registry: RefCell::new(SymbolRegistry::new()),
            types,
            opt_level,
            global_registry,
        }
    }

    pub fn dump_ir<W: Write>(&self, mut output: W) -> Result<(), std::io::Error> {
        let ir_string = self.module.print_to_string();

        output.write_all(ir_string.to_bytes())?;

        output.write_all(b"\n")?;

        Ok(())
    }

    pub fn print_ir(&self) -> Result<(), std::io::Error> {
        self.dump_ir(std::io::stdout())
    }

    pub fn get_object(&self) -> Vec<u8> {
        self.print_ir().unwrap();
        let buffer = self
            .machine()
            .write_to_memory_buffer(&self.module, FileType::Object)
            .expect("Failed to emit object to memory");
        buffer.as_slice().to_vec()
    }

    pub fn apply_passes(&self) {
        let passes = PassBuilderOptions::create();

        // Explicitly lock down expensive passes at O0
        if self.opt_level == OptLevel::O0 {
            passes.set_loop_vectorization(false);
            passes.set_loop_unrolling(false);
            passes.set_loop_interleaving(false);
        }

        self.module
            .run_passes(self.opt_level.as_llvm_pipeline(), &self.machine, passes)
            .expect("Could not run passes");
    }

    pub fn lower_type(&self, data_type: &DataType) -> BasicTypeEnum<'ctx> {
        match data_type {
            DataType::U8 | DataType::S8 => self.context.i8_type().as_basic_type_enum(),
            DataType::U16 | DataType::S16 => self.context.i16_type().as_basic_type_enum(),
            DataType::Char | DataType::U32 | DataType::S32 => {
                self.context.i32_type().as_basic_type_enum()
            }
            DataType::U64 | DataType::S64 => self.context.i64_type().as_basic_type_enum(),
            DataType::Bool => self.context.bool_type().as_basic_type_enum(),
            DataType::F32 | DataType::F64 => self.context.f32_type().as_basic_type_enum(),
            DataType::Struct(_) | DataType::Enum(_) => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .as_basic_type_enum(),
        }
    }

    pub const fn context(&self) -> &'ctx Context {
        self.context
    }

    pub const fn builder(&self) -> &Builder<'ctx> {
        &self.builder
    }

    pub const fn machine(&self) -> &TargetMachine {
        &self.machine
    }

    pub fn type_registry(&self) -> Ref<'_, SymbolRegistry<'ctx>> {
        self.registry.borrow()
    }
    pub fn type_registry_mut(&self) -> RefMut<'_, SymbolRegistry<'ctx>> {
        self.registry.borrow_mut()
    }

    pub const fn types(&self) -> &CodegenTypes<'ctx> {
        &self.types
    }

    pub const fn global_registry(&self) -> &GlobalRegistry<'ctx> {
        &self.global_registry
    }

    pub const fn module(&self) -> &Module<'ctx> {
        &self.module
    }
}

pub struct FunctionContext<'ctx> {
    current_function: FunctionValue<'ctx>,
    current_block: BasicBlock<'ctx>,
}

impl<'ctx> FunctionContext<'ctx> {
    pub const fn new(
        current_function: FunctionValue<'ctx>,
        current_block: BasicBlock<'ctx>,
    ) -> Self {
        Self {
            current_function,
            current_block,
        }
    }

    pub const fn current_function(&self) -> FunctionValue<'ctx> {
        self.current_function
    }

    pub const fn current_block(&self) -> BasicBlock<'ctx> {
        self.current_block
    }

    pub fn set_current_block(&mut self, builder: &Builder<'ctx>, block: BasicBlock<'ctx>) {
        self.current_block = block;
        builder.position_at_end(block);
    }
}

pub struct StatementContext<'ctx, 'fc> {
    last_breakable_block: Option<BasicBlock<'ctx>>,
    function_context: &'fc mut FunctionContext<'ctx>,
}

impl<'ctx, 'fc> StatementContext<'ctx, 'fc> {
    pub const fn new(
        last_breakable_block: Option<BasicBlock<'ctx>>,
        function_context: &'fc mut FunctionContext<'ctx>,
    ) -> Self {
        Self {
            last_breakable_block,
            function_context,
        }
    }

    pub const fn last_breakable_block(&self) -> Option<BasicBlock<'ctx>> {
        self.last_breakable_block
    }

    pub const fn function_context(&self) -> &FunctionContext<'ctx> {
        self.function_context
    }

    pub const fn function_context_mut(&mut self) -> &mut FunctionContext<'ctx> {
        self.function_context
    }

    pub fn set_current_block(&mut self, llvm_context: &LLVMContext<'ctx>, block: BasicBlock<'ctx>) {
        self.function_context
            .set_current_block(llvm_context.builder(), block);
    }

    pub const fn with_last_breakable_block(
        &mut self,
        last_breakable_block: BasicBlock<'ctx>,
    ) -> StatementContext<'ctx, '_> {
        StatementContext {
            last_breakable_block: Some(last_breakable_block),
            function_context: self.function_context,
        }
    }
}
