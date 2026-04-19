/// Optimization level for LLVM pass pipeline execution.
///
/// Maps to LLVM's New Pass Manager optimization pipelines. Each variant corresponds
/// to a different trade-off between compile time and generated code performance.
///
/// The optimization level is configured during LLVM context creation
/// and determines which passes are applied via the LLVM pass pipeline.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum OptLevel {
    /// `-O0`: No optimization. Lowest compile time, best for debugging.
    ///
    /// Disables expensive passes like loop vectorization, unrolling, and interleaving.
    /// The generated code preserves the original structure of the source for maximum debuggability.
    O0,
    /// `-O1`: Basic optimizations. Good for speeding up test runs.
    ///
    /// Applies a modest set of optimizations that improve performance without significantly
    /// increasing compile time.
    O1,
    /// `-O2`: Standard release. Fast execution, reasonable compile time.
    ///
    /// The default optimization level for production builds. Applies a comprehensive set of
    /// optimizations that provide strong runtime performance.
    O2,
    /// `-O3`: Max speed. Aggressive inlining and loop unrolling. Can bloat binary size.
    ///
    /// Applies the most aggressive optimization passes, including function inlining,
    /// loop unrolling, and vectorization. May increase binary size significantly.
    O3,
    /// `-Os`: Optimize for size. Like O2, but restricts code bloat.
    ///
    /// Similar to O2 but with constraints to minimize generated code size.
    Os,
    /// `-Oz`: Minimum size at all costs. Disables unrolling.
    ///
    /// The most aggressive size optimization. Disables loop unrolling and other
    /// transformations that favor code size over execution speed.
    Oz,
}

impl OptLevel {
    /// Returns the LLVM New Pass Manager pipeline string for this optimization level.
    ///
    /// The returned string is used to configure the optimization pipeline
    /// via the LLVM pass builder.
    ///
    /// # Returns
    ///
    /// A static string in the format `"default::<level>"` that identifies the
    /// optimization pipeline to run. For example, `"default<O2>"` for O2 optimization.
    pub const fn as_llvm_pipeline(self) -> &'static str {
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
    /// Converts an [`OptLevel`] to its LLVM pipeline string representation.
    ///
    /// This is a convenience alias for [`OptLevel::as_llvm_pipeline`].
    fn from(opt: OptLevel) -> Self {
        opt.as_llvm_pipeline()
    }
}

impl From<OptLevel> for inkwell::OptimizationLevel {
    /// Converts an [`OptLevel`] to the corresponding [`inkwell::OptimizationLevel`].
    ///
    /// This mapping is used to configure the target machine's optimization level.
    fn from(val: OptLevel) -> Self {
        match val {
            OptLevel::O0 => Self::None,
            OptLevel::O1 => Self::Less,
            OptLevel::O2 | OptLevel::Os | OptLevel::Oz => Self::Default,
            OptLevel::O3 => Self::Aggressive,
        }
    }
}
