use inkwell::{
    context::Context,
    data_layout::DataLayout,
    targets::TargetData,
    types::{IntType, VoidType},
};

pub struct CodegenTypes<'ctx> {
    pub void: VoidType<'ctx>,
    pub i32: IntType<'ctx>,
    pub bool: IntType<'ctx>,
    pub i8: IntType<'ctx>,
    pub usize: IntType<'ctx>,
}

impl<'ctx> CodegenTypes<'ctx> {
    pub fn new(context: &'ctx Context, layout: &DataLayout) -> Self {
        let target_data = TargetData::create(layout.as_str().to_str().unwrap());

        let usize_type = context.ptr_sized_int_type(&target_data, None);

        Self {
            void: context.void_type(),
            i32: context.i32_type(),
            bool: context.bool_type(),
            i8: context.i8_type(),
            usize: usize_type,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum OptLevel {
    O0,
    O1,
    O2,
    O3,
}

impl Into<inkwell::OptimizationLevel> for OptLevel {
    fn into(self) -> inkwell::OptimizationLevel {
        match self {
            OptLevel::O0 => inkwell::OptimizationLevel::None,
            OptLevel::O1 => inkwell::OptimizationLevel::Less,
            OptLevel::O2 => inkwell::OptimizationLevel::Default,
            OptLevel::O3 => inkwell::OptimizationLevel::Aggressive,
        }
    }
}
