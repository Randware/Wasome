use inkwell::types::FloatType;
use inkwell::{
    context::Context,
    data_layout::DataLayout,
    targets::TargetData,
    types::{IntType, VoidType},
};

mod opt_level;
pub use opt_level::OptLevel;
mod module;

pub struct CodegenTypes<'ctx> {
    pub void: VoidType<'ctx>,
    pub usize: IntType<'ctx>,
    pub u64: IntType<'ctx>,
    pub i64: IntType<'ctx>,
    pub f64: FloatType<'ctx>,
    pub u32: IntType<'ctx>,
    pub i32: IntType<'ctx>,
    pub f32: FloatType<'ctx>,
    pub u16: IntType<'ctx>,
    pub i16: IntType<'ctx>,
    pub u8: IntType<'ctx>,
    pub i8: IntType<'ctx>,
    pub bool: IntType<'ctx>,
    pub char: IntType<'ctx>,
}

impl<'ctx> CodegenTypes<'ctx> {
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
