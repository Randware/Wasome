use std::env;

/// This scripts lets the `find_lld()` function know on which target it runs.
/// This has to be done this way, since cargo only exposes the **TARGET** env for building scripts
fn main() {
    // Gets the TARGET env and sets it for the source code
    println!("cargo:rustc-env=TARGET={}", &env::var("TARGET").unwrap());

    // Small optimization. Tells cargo to only rebuild this script
    // if the TARGET env changes.
    println!("cargo:rerun-if-changed-env=TARGET");
}
