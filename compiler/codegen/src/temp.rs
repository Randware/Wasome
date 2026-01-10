use std::path::PathBuf;
pub struct Cli {
    pub file: Option<PathBuf>,

    pub opt_level: OptLevel,

    /// Comma separated list of LLVM passes (use opt for a list, also see https://www.llvm.org/docs/Passes.html)
    pub passes: String,

    /// When compiling a file, specifies an output file to write to
    pub output: PathBuf,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum OptLevel {
    O0,
    O1,
    O2,
    O3,
}

impl Cli {
    pub fn new(
        file: Option<PathBuf>,
        opt_level: OptLevel,
        passes: String,
        output: PathBuf,
    ) -> Self {
        Self {
            file,
            opt_level,
            passes,
            output,
        }
    }

    pub fn mockup() -> Self {
        Self::new(
            None,
            OptLevel::O1,
            "instcombine,reassociate,gvn,simplifycfg,mem2reg".to_string(),
            PathBuf::new(),
        )
    }
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
