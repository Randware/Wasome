use std::io;

use thiserror::Error;

/// The parsed human-readable error from the wasm-ld commannd
#[derive(Error, Debug)]
pub enum LinkerError {
    #[error("{}", if cfg!(debug_assertions) {
        "System linker 'wasm-ld' not found in PATH. Please install the LLVM toolchain (e.g., `brew install lld` or `apt install lld`)."
    } else {
        "The bundled 'wasm-ld' binary is missing. It should be located next to this executable. Please cleanly reinstall the application."
    })]
    LldNotFound,

    #[error("File system I/O error occurred: {0}")]
    Io(#[from] std::io::Error),

    #[error("Invalid Wasm file provided.\n  LLVM Output: {0}")]
    InvalidInputFile(String),

    #[error("Missing a required function or symbol.\n  LLVM Output: {0}")]
    UndefinedSymbol(String),

    #[error("Function signature or type mismatch between files.\n  LLVM Output: {0}")]
    TypeMismatch(String),

    #[error("Memory limit exceeded during linking.\n  LLVM Output: {0}")]
    MemoryConfigurationError(String),

    #[error("An unknown linker error occurred.\n  LLVM Output: {0}")]
    Unknown(String),
}

impl LinkerError {
    /// Parses the raw stderr output from the wasm-ld command output
    pub fn from_stderr(stderr: &str) -> Self {
        // Iterate through the lines of stderr to find the actual error
        for line in stderr.lines() {
            // Skip lines that aren't explicit errors or warnings
            if !line.contains("wasm-ld: error:") && !line.contains("wasm-ld: warning:") {
                continue;
            }

            // Match against the known LLVM source patterns
            if line.contains("not a wasm file")
                || line.contains("neither Wasm object file nor LLVM bitcode")
            {
                return Self::InvalidInputFile(line.trim().to_string());
            }
            if line.contains("undefined symbol:") {
                return Self::UndefinedSymbol(line.trim().to_string());
            }
            if line.contains("type mismatch:") || line.contains("signature mismatch:") {
                return Self::TypeMismatch(line.trim().to_string());
            }
            if line.contains("memory too small")
                || line.contains("memory too large")
                || line.contains("heap too large")
            {
                return Self::MemoryConfigurationError(line.trim().to_string());
            }
            if line.contains("failed to open") || line.contains("failed to write output") {
                return Self::Io(io::Error::new(
                    io::ErrorKind::Other,
                    line.trim().to_string(),
                ));
            }
        }

        // Fallback if LLVM gives a weird error that was not mapped
        Self::Unknown(stderr.trim().to_string())
    }
}
