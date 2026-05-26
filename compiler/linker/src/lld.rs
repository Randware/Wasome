use std::{env, io, path::PathBuf, process::Command};

/// Locates a WebAssembly-compatible LLVM linker (`wasm-ld` or `lld`) on the host system.
///
/// The Wasome compiler needs the LLVM's linker. It tries 4 steps (only 3 of them in prod) to find
/// the lld
///
/// 1. **Explicit Override:** Checks the `WASOME_LINKER` environment variable.
///    if the user wants to override the linker for whatever reason.
/// 2. **Bundled LLD:** Looks for `wasm-ld` in the exact same directory
///    Looks for the Wasome's bundled LLD.
/// 3. **System PATH:** Checks if `wasm-ld` or the generic `lld` are available
///    in the operating system path.
/// 4. **Rust Sysroot:** Only in DEBUG builds!
///    this is somewhat of a hack. It uses the rust intern linker (rust-ld), but it is just for
///    developers who compile this (with rust obv) it is perfectly fine.
///
/// # Returns
/// * `Ok(PathBuf)` - The absolute path (or verifiable command string) to a working linker.
/// * `Err(io::Error)` - If no valid WebAssembly linker could be found anywhere on the system.
///
pub fn find_lld() -> Result<PathBuf, io::Error> {
    let target_bin: PathBuf = executable_name("wasm-ld").into();

    // Option 1: User provides custom linker
    if let Ok(path) = env::var("WASOME_LINKER") {
        let path: PathBuf = path.into();
        match check_lld(&path) {
            Ok(_) => return Ok(path),
            Err(err) => return Err(err),
        }
    }

    // Option 2: wasm-ld is in the lib directory (../lib/wasm-ld relative to the executable)
    if let Ok(mut current_dir) = std::env::current_exe() {
        current_dir.pop(); // ./bin/waso -> ../bin/
        current_dir.pop(); // ./bin/ -> ./
        current_dir.push("lib"); // ./ -> ./lib/
        current_dir.push(&target_bin);  // ./lib/ -> ./lib/waso

        if check_lld(&current_dir).is_ok() {
            return Ok(current_dir);
        }
    }

    // Option 3: Look for the command in sys path
    let lld_bin: PathBuf = executable_name("lld").into();
    if check_lld(&target_bin).is_ok() {
        return Ok(target_bin);
    } else if check_lld(&lld_bin).is_ok() {
        return Ok(lld_bin);
    }

    // Option 4: (ONLY IN DEV ENV) Use rust's own LLVM
    if cfg!(debug_assertions)
        && let Ok(out) = Command::new("rustc").args(["--print", "sysroot"]).output()
        && let Ok(rustsys_path) = String::from_utf8(out.stdout)
        && let Ok(target) = env::var("TARGET")
    {
        let mut path = PathBuf::from(rustsys_path.trim());
        path.extend([
            "lib",
            "rustlib",
            target.as_str(),
            "bin",
            executable_name("rust-lld").as_str(),
        ]);

        return check_lld(&path).map(|_| path);
    }

    Err(io::Error::new(
        io::ErrorKind::NotFound,
        "Could not find wasm-ld or lld.",
    ))
}

/// Verifies that the given path points to a valid/executable LLVM linker capable of WebAssembly.
///
/// # Arguments
/// * `path` - A reference to the `PathBuf` representing the linker executable.
fn check_lld(path: &PathBuf) -> Result<(), io::Error> {
    Command::new(path)
        .args(["-flavor", "wasm"])
        .arg("-v")
        .output()
        .map(|_| ())
}

/// Helper function to normalize executable names across different operating systems.
///
/// # Arguments
/// * `name` - The base name of the executable
fn executable_name(name: &str) -> String {
    if cfg!(target_os = "windows") {
        format!("{}.exe", name)
    } else {
        name.to_string()
    }
}

#[cfg(test)]
mod tests {
    use crate::lld::find_lld;

    #[test]
    fn is_lld_present() {
        assert!(find_lld().is_ok())
    }
}
