use std::{
    fs::{self, File},
    io::{self, Read, Write},
    iter,
    marker::PhantomData,
    path::Path,
    process::Command,
};

use tempfile::tempdir;

mod error;
mod lld;

pub use error::LinkerError;

/// During this state the linker is just collecting files
pub struct Init;

/// When reaching this state the user of this lib can call `.link()`
pub struct Ready;

/// Trait for shared functionality of [Init] and [Ready]
pub trait Shared {}
impl Shared for Init {}
impl Shared for Ready {}

/// A wrapper around the WebAssembly linker (`wasm-ld`).
pub struct Linker<State> {
    /// The collection of object files (`.o`) to be linked.
    files: Vec<OFile>,
    __state: PhantomData<State>,
}

impl Linker<Init> {
    /// Creates a new, empty `Linker` builder in the `Init` state.
    /// To transition, call `.build()`
    pub fn builder() -> Self {
        Linker {
            files: Vec::new(),
            __state: PhantomData,
        }
    }

    /// Consumes the `Init` state and transitions the `Linker` into the `Ready` state.
    pub fn build(self) -> Linker<Ready> {
        Linker {
            files: self.files,
            __state: PhantomData,
        }
    }

    /// Create a new linker and skip the building process (`builder()`).
    /// Use this if every OFile is known at initialization
    pub fn new(files: impl IntoIterator<Item = OFile>) -> Linker<Ready> {
        Linker {
            files: files.into_iter().collect(),
            __state: PhantomData,
        }
    }

    /// Returns a slice of the object files
    pub fn files(&self) -> &[OFile] {
        &self.files
    }
}

impl<State: Shared> Linker<State> {
    /// Add a multiple [OFiles](OFile) to the linking process
    pub fn add_files(&mut self, files: impl IntoIterator<Item = OFile>) -> &mut Self {
        self.files.extend(files);
        self
    }

    /// Add a single [OFile] to the linking process
    pub fn add_file(&mut self, file: OFile) -> &mut Self {
        self.add_files(iter::once(file))
    }

    /// Returns a reference to the slice of object files
    pub fn get_files(&self) -> &[OFile] {
        &self.files
    }
}

impl Linker<Ready> {
    /// Executes the linking process using the system's `wasm-ld` binary.
    ///
    /// # Errors
    /// Returns a tuple containing `(self, LinkerError)` if the linking fails
    /// Returning `self` allows the caller to recover the linker state/inspect the inputs
    /// or retry the operation without losing the underlying memory allocation.
    pub fn link(self) -> Result<OFile, (Self, LinkerError)> {
        let lld = match lld::find_lld() {
            Ok(path) => path,
            Err(e) => {
                return Err((
                    self,
                    if e.kind() == io::ErrorKind::NotFound {
                        LinkerError::LldNotFound
                    } else {
                        LinkerError::Io(e)
                    },
                ));
            }
        };

        let tempdir = match tempdir() {
            Ok(temp) => temp,
            Err(e) => return Err((self, e.into())),
        };

        let mut output = Command::new(lld);
        output.args(["-flavor", "wasm"]);

        for (index, o_file) in self.get_files().iter().enumerate() {
            let path = tempdir.path().join(format!("wasome_input_{}.o", index));
            let mut file = match File::create(&path) {
                Ok(file) => file,
                Err(e) => return Err((self, e.into())),
            };

            if let Err(e) = file.write_all(&o_file.data) {
                return Err((self, e.into()));
            }

            output.arg(path);
        }

        let out_path = tempdir.path().join("output.wasm");
        let output = match output.arg("-o").arg(&out_path).output() {
            Ok(o) => o,
            Err(e) => return Err((self, e.into())),
        };

        if !output.status.success() {
            let error = String::from_utf8_lossy(&output.stderr);
            return Err((self, LinkerError::from_stderr(&error)));
        }

        let mut output = match File::open(out_path) {
            Ok(o) => o,
            Err(e) => return Err((self, e.into())),
        };

        let mut linked = Vec::new();
        let _ = output
            .read_to_end(&mut linked)
            .map_err(|e| (self, LinkerError::Io(e)))?;

        Ok(linked.into())
    }
}

/// Represents an in-memory binary file payload
pub struct OFile {
    pub data: Vec<u8>,
}

impl OFile {
    /// Creates a new `OFile` from an iterator of bytes
    pub fn new(data: impl IntoIterator<Item = u8>) -> Self {
        OFile {
            data: data.into_iter().collect(),
        }
    }

    /// Reads a file from the host filesystem directly into memory and wraps it as an [`OFile`]
    pub fn from_path(path: &Path) -> io::Result<Self> {
        Ok(OFile {
            data: fs::read(path)?,
        })
    }
}

impl From<Vec<u8>> for OFile {
    fn from(value: Vec<u8>) -> Self {
        OFile { data: value }
    }
}
