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
    #[must_use]
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

#[cfg(test)]
mod tests {
    use super::*;

    // === OptLevel::as_llvm_pipeline ===

    #[test]
    fn as_llvm_pipeline_o0() {
        assert_eq!(OptLevel::O0.as_llvm_pipeline(), "default<O0>");
    }

    #[test]
    fn as_llvm_pipeline_o1() {
        assert_eq!(OptLevel::O1.as_llvm_pipeline(), "default<O1>");
    }

    #[test]
    fn as_llvm_pipeline_o2() {
        assert_eq!(OptLevel::O2.as_llvm_pipeline(), "default<O2>");
    }

    #[test]
    fn as_llvm_pipeline_o3() {
        assert_eq!(OptLevel::O3.as_llvm_pipeline(), "default<O3>");
    }

    #[test]
    fn as_llvm_pipeline_os() {
        assert_eq!(OptLevel::Os.as_llvm_pipeline(), "default<Os>");
    }

    #[test]
    fn as_llvm_pipeline_oz() {
        assert_eq!(OptLevel::Oz.as_llvm_pipeline(), "default<Oz>");
    }

    // === From<OptLevel> for &'static str ===

    #[test]
    fn from_optlevel_to_str_all() {
        let cases: [(OptLevel, &str); 6] = [
            (OptLevel::O0, "default<O0>"),
            (OptLevel::O1, "default<O1>"),
            (OptLevel::O2, "default<O2>"),
            (OptLevel::O3, "default<O3>"),
            (OptLevel::Os, "default<Os>"),
            (OptLevel::Oz, "default<Oz>"),
        ];
        for (level, expected) in cases {
            let s: &str = level.into();
            assert_eq!(
                s, expected,
                "From<OptLevel> for &str mismatch for {level:?}"
            );
        }
    }

    #[test]
    fn from_str_is_consistent_with_as_llvm_pipeline() {
        // .into() and .as_llvm_pipeline() must produce identical results
        let levels = [
            OptLevel::O0,
            OptLevel::O1,
            OptLevel::O2,
            OptLevel::O3,
            OptLevel::Os,
            OptLevel::Oz,
        ];
        for level in levels {
            let via_trait: &str = level.into();
            let via_method = level.as_llvm_pipeline();
            assert_eq!(
                via_trait, via_method,
                "From<OptLevel> and as_llvm_pipeline() diverge for {level:?}: trait={via_trait}, method={via_method}"
            );
        }
    }

    // === From<OptLevel> for inkwell::OptimizationLevel ===

    #[test]
    fn from_optlevel_to_inkwell_o0() {
        let level: inkwell::OptimizationLevel = OptLevel::O0.into();
        assert_eq!(level, inkwell::OptimizationLevel::None);
    }

    #[test]
    fn from_optlevel_to_inkwell_o1() {
        let level: inkwell::OptimizationLevel = OptLevel::O1.into();
        assert_eq!(level, inkwell::OptimizationLevel::Less);
    }

    #[test]
    fn from_optlevel_to_inkwell_o2() {
        let level: inkwell::OptimizationLevel = OptLevel::O2.into();
        assert_eq!(level, inkwell::OptimizationLevel::Default);
    }

    #[test]
    fn from_optlevel_to_inkwell_o3() {
        let level: inkwell::OptimizationLevel = OptLevel::O3.into();
        assert_eq!(level, inkwell::OptimizationLevel::Aggressive);
    }

    #[test]
    fn from_optlevel_to_inkwell_os() {
        let level: inkwell::OptimizationLevel = OptLevel::Os.into();
        assert_eq!(level, inkwell::OptimizationLevel::Default);
    }

    #[test]
    fn from_optlevel_to_inkwell_oz() {
        let level: inkwell::OptimizationLevel = OptLevel::Oz.into();
        assert_eq!(level, inkwell::OptimizationLevel::Default);
    }
}
