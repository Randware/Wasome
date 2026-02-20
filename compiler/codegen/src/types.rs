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
    // -O0: No optimization. Fastest compile, best for debugging
    O0,
    // -O1: Basic optimizations. Good for speeding up test runs
    O1,
    // -O2: Standard release. Fast execution, reasonable compile time
    O2,
    // -O3: Max speed. Aggressive inlining/unrolling. Can bloat binary
    O3,
    // -Os: Optimize for size. Like O2, but restricts code bloat
    Os,
    // -Oz: Minimum size at all costs. Disables unrolling
    Oz,
}

impl OptLevel {
    /// Grabs the exact pipeline string for the LLVM New Pass Manager
    pub const fn as_llvm_pipeline(&self) -> &'static str {
        match self {
            Self::O0 => "default<O0>",
            Self::O1 => "default<O1>",
            Self::O2 => "default<O2>",
            Self::O3 => "default<O3>",
            Self::Os => "default<Os>",
            Self::Oz => "default<Oz>",
        }
    }
}

impl From<OptLevel> for &'static str {
    fn from(opt: OptLevel) -> Self {
        opt.as_llvm_pipeline()
    }
}

impl From<OptLevel> for inkwell::OptimizationLevel {
    fn from(val: OptLevel) -> Self {
        match val {
            OptLevel::O0 => inkwell::OptimizationLevel::None,
            OptLevel::O1 => inkwell::OptimizationLevel::Less,
            OptLevel::O2 => inkwell::OptimizationLevel::Default,
            OptLevel::O3 => inkwell::OptimizationLevel::Aggressive,
            // Os and Oz don't have their own OptLevel because they get treated they same
            // by the `TargetMachine` creation
            OptLevel::Os => inkwell::OptimizationLevel::Default,
            OptLevel::Oz => inkwell::OptimizationLevel::Default,
        }
    }
}
