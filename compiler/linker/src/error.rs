use std::io;

use thiserror::Error;

/// Represents the most common failures that can occur during the WebAssembly linking phase
#[derive(Error, Debug)]
pub enum LinkerError {
    /// Emitted when the `wasm-ld` binary cannot be located on the host system
    ///
    /// * In **Debug** mode it tells the dev how to install the lld
    /// * In **Release** mode it tells the user to reinstall (somehow the bundled file was deleted)
    #[error("{}", if cfg!(debug_assertions) {
        "System linker 'wasm-ld' not found in PATH. Please install the LLVM toolchain (e.g., `brew install lld` or `apt install lld`)."
    } else {
        "The bundled 'wasm-ld' binary is missing. It should be located next to this executable. Please cleanly reinstall the application."
    })]
    LldNotFound,

    /// Emitted when a standard File System or OS operation fails
    #[error("File system I/O error occurred: {0}")]
    Io(#[from] std::io::Error),

    /// Emitted when `wasm-ld` rejects an input file because it is corrupt or not a valid WebAssembly object file
    #[error("Invalid Wasm file provided.\n  LLVM Output: {0}")]
    InvalidInputFile(String),

    /// Emitted when a piece of Wasome code tries to call a function or use a variable that does not exist
    #[error("Missing a required function or symbol.\n  LLVM Output: {0}")]
    UndefinedSymbol(String),

    /// Emitted when two files define the same function differently, or an imported signature clashes with an export
    #[error("Function signature or type mismatch between files.\n  LLVM Output: {0}")]
    TypeMismatch(String),

    #[error("Memory limit exceeded during linking.\n  LLVM Output: {0}")]
    MemoryConfigurationError(String),

    /// A catch-all variant for when `wasm-ld` returns an error, but the parser couldn't identify the specific cause.
    #[error("An unknown linker error occurred.\n  LLVM Output: {0}")]
    Unknown(String),
}

impl LinkerError {
    /// Parses the raw `stderr` text output from the `wasm-ld` command into a structured `LinkerError`.
    ///
    /// # Arguments
    /// * `stderr` - The raw string captured from the linker's standard error stream.
    pub fn from_stderr(stderr: &str) -> Self {
        for line in stderr.lines() {
            if !line.contains("wasm-ld: error:") && !line.contains("wasm-ld: warning:") {
                continue;
            }

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
                return Self::Io(io::Error::other(line.trim().to_string()));
            }
        }

        // Fallback if LLVM gives a weird error that was not mapped
        Self::Unknown(stderr.trim().to_string())
    }
}
