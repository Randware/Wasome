use inkwell::types::FloatType;
use inkwell::{
    context::Context,
    data_layout::DataLayout,
    targets::TargetData,
    types::{IntType, VoidType},
};

mod opt_level;
pub use opt_level::OptLevel;

/// Cached LLVM type instances for common Rust data types.
///
/// Pre-allocated during initialization to avoid repeated lookups during code generation.
/// The [`usize`] type is determined dynamically based on the target data layout (pointer-sized integer).
///
/// This struct is stored in [`LLVMContext`](crate::context::LLVMContext) and provides fast access
/// to commonly used LLVM types throughout the compilation pipeline.
pub struct CodegenTypes<'ctx> {
    /// The void type, used for functions that return no value.
    pub void: VoidType<'ctx>,
    /// The pointer-sized integer type, determined by the target data layout.
    pub usize: IntType<'ctx>,
    /// The 64-bit unsigned integer type.
    pub u64: IntType<'ctx>,
    /// The 64-bit signed integer type.
    pub i64: IntType<'ctx>,
    /// The 64-bit floating-point type (double).
    pub f64: FloatType<'ctx>,
    /// The 32-bit unsigned integer type.
    pub u32: IntType<'ctx>,
    /// The 32-bit signed integer type.
    pub i32: IntType<'ctx>,
    /// The 32-bit floating-point type (float).
    pub f32: FloatType<'ctx>,
    /// The 16-bit unsigned integer type.
    pub u16: IntType<'ctx>,
    /// The 16-bit signed integer type.
    pub i16: IntType<'ctx>,
    /// The 8-bit unsigned integer type.
    pub u8: IntType<'ctx>,
    /// The 8-bit signed integer type.
    pub i8: IntType<'ctx>,
    /// The boolean type (1-bit integer).
    pub bool: IntType<'ctx>,
    /// The character type (32-bit integer, storing Unicode code points).
    pub char: IntType<'ctx>,
}

impl<'ctx> CodegenTypes<'ctx> {
    /// Creates a new `CodegenTypes` from the given LLVM context and target data layout.
    ///
    /// The pointer-sized integer type (`usize`) is determined dynamically from the target
    /// data layout using [`Context::ptr_sized_int_type`]. All other types are obtained
    /// directly from the LLVM context.
    ///
    /// # Arguments
    ///
    /// * `context` - The LLVM [`Context`] to obtain types from
    /// * `layout` - The target data layout, used to determine the pointer-sized integer type
    pub fn new(context: &'ctx Context, layout: &DataLayout) -> Self {
        let target_data = TargetData::create(layout.as_str().to_str().unwrap());

        let usize_type = context.ptr_sized_int_type(&target_data, None);

        Self {
            void: context.void_type(),
            i32: context.i32_type(),
            f32: context.f32_type(),
            u16: context.i16_type(),
            i16: context.i16_type(),
            bool: context.bool_type(),
            i8: context.i8_type(),
            usize: usize_type,
            u64: context.i64_type(),
            i64: context.i64_type(),
            f64: context.f64_type(),
            u32: context.i32_type(),
            u8: context.i8_type(),
            char: context.i32_type(),
        }
    }
}
