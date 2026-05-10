use std::{
    env::{self},
    io::{self},
    path::{Path, PathBuf},
    process::Command,
};

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

    // Option 2: wasm-ld is next to the wasome bin
    if let Ok(mut current_dir) = std::env::current_exe() {
        current_dir.pop();
        current_dir.push(target_bin);

        return check_lld(&current_dir).map(|_| current_dir);
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

fn check_lld(path: &PathBuf) -> Result<(), io::Error> {
    Command::new(path)
        .args(["--flavor", "wasm"])
        .arg("-v")
        .output()
        .map(|_| ())
}

fn executable_name(name: &str) -> String {
    if cfg!(target_os = "windows") {
        format!("{}.exe", name)
    } else {
        name.to_string()
    }
}
