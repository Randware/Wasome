use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::intrinsics::Intrinsic;
use inkwell::module::Module;
use inkwell::types::{BasicType, FunctionType, StructType};
use inkwell::values::FunctionValue;

/// Holds shared LLVM types and function declarations used across all code generation.
///
/// The [`GlobalRegistry`] provides access to LLVM types and function declarations that are
/// shared across all compilation units. This includes:
///
/// * **Base struct layouts** - The foundational LLVM struct types used as templates for all
///   heap-allocated values (enums and structs). These define the common layout of a refcount
///   field and are used for pointer arithmetic during reference counting operations.
/// * **Stack intrinsics** - Declarations for `llvm.stacksave` and `llvm.stackrestore`, used
///   to implement `break` statements in loops by saving and restoring the stack pointer.
/// * **Memory management** - Declarations for `malloc` and `free` for heap allocation during
///   struct and enum instantiation.
/// * **Drop function type** - The function type signature for all `drop` functions, which
///   take a single pointer and recursively deallocate heap-allocated values.
pub struct GlobalRegistry<'ctx> {
    /// The base struct type for all enums: `(i32 refcount, i32 discriminant)`.
    ///
    /// This struct layout serves as the template for the discriminant and size fields
    /// that are prepended to every enum variant allocation.
    base_enum: StructType<'ctx>,
    /// The base struct type for all heap-allocated values: `(i32 refcount)`.
    ///
    /// This struct layout serves as the template for the reference count field that is
    /// prepended to every struct and enum allocation. Used for refcount read/write operations.
    base_heap_allocated: StructType<'ctx>,
    /// Declaration of the `llvm.stacksave` intrinsic.
    ///
    /// Returns a pointer to the current stack position. Used before executing loop bodies
    /// to enable `break` statements to restore the stack.
    stacksave: FunctionValue<'ctx>,
    /// Declaration of the `llvm.stackrestore` intrinsic.
    ///
    /// Restores the stack to a previously saved position. Used after loop body execution
    /// to clean up stack allocations made within the loop.
    stackrestore: FunctionValue<'ctx>,
    /// The function type for `drop` functions: `void (ptr)`.
    ///
    /// All `drop` functions for structs and enums conform to this signature.
    /// Takes a pointer to the heap-allocated value and recursively deallocates it.
    drop: FunctionType<'ctx>,
    /// Declaration of the C `malloc` function: `ptr (i32 size)`.
    ///
    /// Allocates the specified number of bytes on the heap. Used by [`allocate`](crate::Codegen::allocate)
    /// when creating new struct and enum instances.
    malloc: FunctionValue<'ctx>,
    /// Declaration of the C `free` function: `void (ptr, i32 size)`.
    ///
    /// Frees the specified heap allocation. The size parameter is passed for WASM compatibility.
    /// Used by drop functions to deallocate memory.
    free: FunctionValue<'ctx>,
    /// The panic handler
    ///
    /// Called when a panic occurs
    panic: FunctionValue<'ctx>,
}

impl<'ctx> GlobalRegistry<'ctx> {
    /// Creates a new `GlobalRegistry` with base struct types and declarations for
    /// stack intrinsics, `malloc`, `free`, and the `drop` function type.
    ///
    /// Initializes all shared LLVM types and function declarations needed for code generation:
    /// - The base enum struct type with discriminant and size fields (two i32 values)
    /// - The base heap-allocated struct type with a refcount field (one i32 value)
    /// - Stack save/restore intrinsics for loop break support
    /// - `malloc` and `free` function declarations for heap memory management
    /// - The `drop` function type signature for deallocation
    ///
    /// # Arguments
    ///
    /// * `ctx` - The LLVM [`Context`] for creating types and function declarations
    /// * `module` - The LLVM [`Module`] for adding function declarations
    pub fn new(ctx: &'ctx Context, module: &'_ Module<'ctx>) -> Self {
        let base_heap_allocated = ctx.struct_type(&[ctx.i32_type().as_basic_type_enum()], false);
        let base_enum = ctx.struct_type(
            &[
                ctx.i32_type().as_basic_type_enum(),
                ctx.i32_type().as_basic_type_enum(),
            ],
            false,
        );

        let ptr_type = ctx.ptr_type(AddressSpace::default()).as_basic_type_enum();
        let stacksave_intrinsic =
            Intrinsic::find("llvm.stacksave").expect("Hardcoded intrinsic should exist");
        let stacksave = stacksave_intrinsic
            .get_declaration(module, &[ptr_type])
            .unwrap();

        let stackrestore_intrinsic =
            Intrinsic::find("llvm.stackrestore").expect("Hardcoded intrinsic should exist");
        let stackrestore = stackrestore_intrinsic
            .get_declaration(
                module,
                &[ctx.ptr_type(AddressSpace::default()).as_basic_type_enum()],
            )
            .unwrap();
        let drop = ctx.void_type().fn_type(
            &[ctx
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum()
                .into()],
            false,
        );

        let malloc = ctx
            .ptr_type(AddressSpace::default())
            .fn_type(&[ctx.i32_type().into()], false);
        let malloc = module.add_function("malloc", malloc, None);
        let free = ctx
            .void_type()
            .fn_type(&[ctx.i32_type().into(), ctx.i32_type().into()], false);
        let free = module.add_function("free", free, None);
        let panic = ctx.void_type().fn_type(&[], false);
        let panic = module.add_function("panic", panic, None);

        Self {
            base_enum,
            base_heap_allocated,
            stacksave,
            stackrestore,
            drop,
            malloc,
            free,
            panic,
        }
    }

    pub const fn base_enum(&self) -> StructType<'ctx> {
        self.base_enum
    }

    pub const fn base_heap_allocated(&self) -> StructType<'ctx> {
        self.base_heap_allocated
    }

    pub const fn stacksave(&self) -> FunctionValue<'ctx> {
        self.stacksave
    }

    pub const fn stackrestore(&self) -> FunctionValue<'ctx> {
        self.stackrestore
    }

    pub const fn drop(&self) -> FunctionType<'ctx> {
        self.drop
    }

    pub const fn malloc(&self) -> FunctionValue<'ctx> {
        self.malloc
    }

    pub const fn free(&self) -> FunctionValue<'ctx> {
        self.free
    }

    pub const fn panic(&self) -> FunctionValue<'ctx> {
        self.panic
    }
}
