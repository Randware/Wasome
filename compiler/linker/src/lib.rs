use std::{
    fs::File,
    io::{self, Read, Write},
    iter,
    marker::PhantomData,
    process::Command,
};

use tempfile::tempdir;

use crate::error::LinkerError;

mod error;
mod lld;

#[derive(Debug)]
pub struct LLD {}

// Our states
pub struct Init;
pub struct Ready;
pub trait Shared {}

impl Shared for Init {}
impl Shared for Ready {}

pub struct Linker<State> {
    files: Vec<OFile>,
    __state: PhantomData<State>,
}

impl Linker<Init> {
    pub fn builder() -> Self {
        Linker {
            files: Vec::new(),
            __state: PhantomData,
        }
    }

    pub fn ready(self) -> Linker<Ready> {
        Linker {
            files: self.files,
            __state: PhantomData,
        }
    }

    pub fn new(files: impl IntoIterator<Item = OFile>) -> Linker<Ready> {
        Linker {
            files: files.into_iter().collect(),
            __state: PhantomData,
        }
    }

    pub fn files(&self) -> &[OFile] {
        &self.files
    }
}

impl<State: Shared> Linker<State> {
    pub fn add_files(mut self, files: impl IntoIterator<Item = OFile>) -> Self {
        self.files.extend(files);
        self
    }

    pub fn add_file(self, file: OFile) -> Self {
        self.add_files(iter::once(file))
    }

    pub fn get_files(&self) -> &[OFile] {
        &self.files
    }
}

impl Linker<Ready> {
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
        let output = match output.args(["-o", out_path.to_str().unwrap()]).output() {
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

pub struct OFile {
    pub data: Vec<u8>,
}

impl From<Vec<u8>> for OFile {
    fn from(value: Vec<u8>) -> Self {
        OFile { data: value }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//
//     #[test]
//     fn it_works() {
//         assert_eq!(4, 4);
//     }
// }
