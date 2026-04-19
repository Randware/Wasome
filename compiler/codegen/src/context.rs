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

/// The central LLVM context for code generation.
///
/// The [`LLVMContext`] is the core of the compilation pipeline. It owns and coordinates all
/// LLVM-level resources needed to generate WebAssembly object code:
///
/// * **LLVM resources** - The [`Context`], [`Builder`], [`Module`], and [`TargetMachine`] for
///   all IR operations and target-specific code emission.
/// * **Type system** - Cached [`CodegenTypes`] for fast LLVM type access and the [`lower_type`]
///   method for converting AST [`DataType`] values to LLVM types.
/// * **Symbol registry** - A [`RefCell<SymbolRegistry>`] that maps AST symbols to their LLVM
///   representations, accessed via [`type_registry`] and [`type_registry_mut`].
/// * **Global registry** - Shared LLVM types and function declarations via [`GlobalRegistry`].
/// * **Optimization** - Configured optimization level and the [`apply_passes`] method that runs
///   the LLVM pass pipeline.
/// * **Output** - IR dumping via [`dump_ir`]/[`print_ir`] and object code generation via [`get_object`].
///
/// [`lower_type`]: Self::lower_type
/// [`type_registry`]: Self::type_registry
/// [`type_registry_mut`]: Self::type_registry_mut
/// [`apply_passes`]: Self::apply_passes
/// [`dump_ir`]: Self::dump_ir
/// [`print_ir`]: Self::print_ir
/// [`get_object`]: Self::get_object
pub struct LLVMContext<'ctx> {
    /// The underlying LLVM [`Context`].
    context: &'ctx Context,
    /// The LLVM [`Builder`] for inserting instructions at the current position.
    builder: Builder<'ctx>,
    /// The LLVM [`Module`] named `"wasome"` that holds all generated IR.
    module: Module<'ctx>,
    /// The LLVM [`TargetMachine`] configured for WebAssembly (wasm32-unknown-unknown).
    machine: TargetMachine,
    /// The symbol registry, wrapped in a [`RefCell`] for interior mutability.
    registry: RefCell<SymbolRegistry<'ctx>>,
    /// Cached LLVM type instances for common Rust data types.
    types: CodegenTypes<'ctx>,
    /// The optimization level for the LLVM pass pipeline.
    opt_level: OptLevel,
    /// Shared LLVM types and function declarations.
    global_registry: GlobalRegistry<'ctx>,
}

impl<'ctx> LLVMContext<'ctx> {
    /// Creates a new `LLVMContext` targeting WebAssembly (wasm32-unknown-unknown).
    ///
    /// Initializes all LLVM resources needed for code generation:
    /// 1. Creates an LLVM [`Builder`] for instruction emission
    /// 2. Initializes the WebAssembly target
    /// 3. Creates a [`TargetMachine`] configured for wasm32-unknown-unknown
    /// 4. Builds the [`CodegenTypes`] type cache from the target data layout
    /// 5. Creates the LLVM [`Module`] named `"wasome"`
    /// 6. Creates the [`GlobalRegistry`] with shared types and function declarations
    /// 7. Sets the module's target triple and data layout to match the target machine
    ///
    /// # Arguments
    ///
    /// * `context` - The LLVM [`Context`] to use for all IR operations
    /// * `opt_level` - The optimization level for the LLVM pass pipeline
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

    /// Writes the LLVM IR representation of the current module to the given writer.
    ///
    /// Serializes the entire LLVM module to a string and writes it to the provided
    /// [`Write`] destination, followed by a newline character.
    ///
    /// # Arguments
    ///
    /// * `output` - The writer to output the IR to
    ///
    /// # Returns
    ///
    /// * `Ok(())` - The IR was written successfully
    /// * `Err(std::io::Error)` - An I/O error occurred while writing
    pub fn dump_ir<W: Write>(&self, mut output: W) -> Result<(), std::io::Error> {
        let ir_string = self.module.print_to_string();

        output.write_all(ir_string.to_bytes())?;

        output.write_all(b"\n")?;

        Ok(())
    }

    /// Prints the LLVM IR representation of the current module to stdout.
    ///
    /// A convenience wrapper around [`dump_ir`](Self::dump_ir) that writes to
    /// [`std::io::stdout`]. Useful for debugging the generated IR.
    ///
    /// # Returns
    ///
    /// * `Ok(())` - The IR was printed successfully
    /// * `Err(std::io::Error)` - An I/O error occurred while writing
    pub fn print_ir(&self) -> Result<(), std::io::Error> {
        self.dump_ir(std::io::stdout())
    }

    /// Applies optimization passes and returns the compiled object code as a byte vector.
    ///
    /// This is the final step of the compilation pipeline. It:
    /// 1. Runs the LLVM optimization pipeline via [`apply_passes`](Self::apply_passes)
    /// 2. Emits the object code to a memory buffer via the target machine
    /// 3. Returns the buffer contents as a `Vec<u8>`
    ///
    /// # Returns
    ///
    /// A vector containing the compiled WebAssembly object code bytes.
    pub fn get_object(&self) -> Vec<u8> {
        self.apply_passes();
        let buffer = self
            .machine()
            .write_to_memory_buffer(&self.module, FileType::Object)
            .expect("Failed to emit object to memory");
        buffer.as_slice().to_vec()
    }

    /// Runs the LLVM optimization pipeline configured by the current [`OptLevel`].
    ///
    /// Uses the LLVM Pass Manager to run the optimization pipeline. At O0,
    /// expensive passes (loop vectorization, unrolling, interleaving) are explicitly
    /// disabled to preserve the original code structure for debugging.
    ///
    /// The pipeline string is obtained from [`OptLevel::as_llvm_pipeline`](OptLevel::as_llvm_pipeline).
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

    /// Lowers an AST [`DataType`] to its corresponding LLVM [`BasicTypeEnum`].
    ///
    /// Maps AST data types to LLVM types as follows:
    /// * `U8`, `S8` → `i8`
    /// * `U16`, `S16` → `i16`
    /// * `Char`, `U32`, `S32` → `i32`
    /// * `U64`, `S64` → `i64`
    /// * `Bool` → `i1` (bool)
    /// * `F32` → `float`
    /// * `F64` → `double`
    /// * `Struct(_)` → `ptr`
    /// * `Enum(_)` → `ptr`
    ///
    /// # Arguments
    ///
    /// * `data_type` - The AST [`DataType`] to lower
    ///
    /// # Returns
    ///
    /// The corresponding LLVM [`BasicTypeEnum`].
    pub fn lower_type(&self, data_type: &DataType) -> BasicTypeEnum<'ctx> {
        match data_type {
            DataType::U8 | DataType::S8 => self.context.i8_type().as_basic_type_enum(),
            DataType::U16 | DataType::S16 => self.context.i16_type().as_basic_type_enum(),
            DataType::Char | DataType::U32 | DataType::S32 => {
                self.context.i32_type().as_basic_type_enum()
            }
            DataType::U64 | DataType::S64 => self.context.i64_type().as_basic_type_enum(),
            DataType::Bool => self.context.bool_type().as_basic_type_enum(),
            DataType::F32 => self.context.f32_type().as_basic_type_enum(),
            DataType::F64 => self.context.f64_type().as_basic_type_enum(),
            DataType::Struct(_) | DataType::Enum(_) => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .as_basic_type_enum(),
        }
    }

    /// Returns a reference to the underlying LLVM [`Context`].
    pub const fn context(&self) -> &'ctx Context {
        self.context
    }

    /// Returns a reference to the LLVM [`Builder`] for instruction emission.
    pub const fn builder(&self) -> &Builder<'ctx> {
        &self.builder
    }

    /// Returns a reference to the [`TargetMachine`] for WebAssembly.
    pub const fn machine(&self) -> &TargetMachine {
        &self.machine
    }

    /// Returns an immutable borrow of the [`SymbolRegistry`].
    ///
    /// Used for lookups of registered types and functions during code generation.
    pub fn type_registry(&self) -> Ref<'_, SymbolRegistry<'ctx>> {
        self.registry.borrow()
    }
    /// Returns a mutable borrow of the [`SymbolRegistry`].
    ///
    /// Used for registering new types and functions during the compilation phases.
    pub fn type_registry_mut(&self) -> RefMut<'_, SymbolRegistry<'ctx>> {
        self.registry.borrow_mut()
    }

    /// Returns a reference to the cached [`CodegenTypes`].
    pub const fn types(&self) -> &CodegenTypes<'ctx> {
        &self.types
    }

    /// Returns a reference to the [`GlobalRegistry`] with shared types and declarations.
    pub const fn global_registry(&self) -> &GlobalRegistry<'ctx> {
        &self.global_registry
    }

    /// Returns a reference to the LLVM [`Module`].
    pub const fn module(&self) -> &Module<'ctx> {
        &self.module
    }
}

/// Holds the current function and insertion point during code generation for a single function.
///
/// The [`FunctionContext`] tracks the LLVM [`FunctionValue`] being compiled and the
/// current [`BasicBlock`] where new instructions are inserted. It is created fresh for
/// each function during the implementation phase.
pub struct FunctionContext<'ctx> {
    /// The LLVM [`FunctionValue`] currently being compiled.
    current_function: FunctionValue<'ctx>,
    /// The current [`BasicBlock`] where instructions are being emitted.
    current_block: BasicBlock<'ctx>,
}

impl<'ctx> FunctionContext<'ctx> {
    /// Creates a new `FunctionContext` for the given LLVM function and basic block.
    ///
    /// # Arguments
    ///
    /// * `current_function` - The LLVM [`FunctionValue`] being compiled
    /// * `current_block` - The initial [`BasicBlock`] for instruction emission
    pub const fn new(
        current_function: FunctionValue<'ctx>,
        current_block: BasicBlock<'ctx>,
    ) -> Self {
        Self {
            current_function,
            current_block,
        }
    }

    /// Returns the current LLVM function being compiled.
    pub const fn current_function(&self) -> FunctionValue<'ctx> {
        self.current_function
    }

    /// Returns the current basic block where instructions are being emitted.
    pub const fn current_block(&self) -> BasicBlock<'ctx> {
        self.current_block
    }

    /// Sets the current basic block and positions the builder at its end.
    ///
    /// # Arguments
    ///
    /// * `builder` - The LLVM [`Builder`] to reposition
    /// * `block` - The new current [`BasicBlock`]
    pub fn set_current_block(&mut self, builder: &Builder<'ctx>, block: BasicBlock<'ctx>) {
        self.current_block = block;
        builder.position_at_end(block);
    }
}

/// Context for generating statements within a function.
///
/// The [`StatementContext`] wraps a [`FunctionContext`] and tracks the last breakable
/// block, which is used to implement `break` statements in loops. When a `break` is
/// encountered, a branch is emitted to this block.
///
/// The breakable block is set when entering loop constructs via [`with_last_breakable_block`](Self::with_last_breakable_block)
/// and is `None` outside of loop contexts.
pub struct StatementContext<'ctx, 'fc> {
    /// The last breakable block, if inside a loop.
    ///
    /// When `Some`, this is the basic block that `break` statements should branch to.
    /// When `None`, a `break` statement would be an error.
    last_breakable_block: Option<BasicBlock<'ctx>>,
    /// The mutable [`FunctionContext`] for the function being compiled.
    function_context: &'fc mut FunctionContext<'ctx>,
}

impl<'ctx, 'fc> StatementContext<'ctx, 'fc> {
    /// Creates a new `StatementContext` for the given function context and optional breakable block.
    ///
    /// # Arguments
    ///
    /// * `last_breakable_block` - The block to branch to on `break`, or `None` outside loops
    /// * `function_context` - The mutable [`FunctionContext`] for the current function
    pub const fn new(
        last_breakable_block: Option<BasicBlock<'ctx>>,
        function_context: &'fc mut FunctionContext<'ctx>,
    ) -> Self {
        Self {
            last_breakable_block,
            function_context,
        }
    }

    /// Returns the last breakable block, if one exists.
    ///
    /// Used by [`compile_break`](crate::Codegen::compile_break) to determine the target
    /// block for break statements.
    pub const fn last_breakable_block(&self) -> Option<BasicBlock<'ctx>> {
        self.last_breakable_block
    }

    /// Returns a reference to the function context.
    pub const fn function_context(&self) -> &FunctionContext<'ctx> {
        self.function_context
    }

    /// Returns a mutable reference to the function context.
    pub const fn function_context_mut(&mut self) -> &mut FunctionContext<'ctx> {
        self.function_context
    }

    /// Sets the current basic block through the function context.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] providing the [`Builder`]
    /// * `block` - The new current [`BasicBlock`]
    pub fn set_current_block(&mut self, llvm_context: &LLVMContext<'ctx>, block: BasicBlock<'ctx>) {
        self.function_context
            .set_current_block(llvm_context.builder(), block);
    }

    /// Returns a new `StatementContext` with the given block as the last breakable block.
    ///
    /// This is used when entering a loop body to track where `break` statements should jump.
    /// The returned context borrows `self`'s function context for the lifetime of the loop.
    ///
    /// # Arguments
    ///
    /// * `last_breakable_block` - The block to branch to on `break` within this loop
    ///
    /// # Returns
    ///
    /// A new [`StatementContext`] with the breakable block set.
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
