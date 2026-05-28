use alloc::string::String;
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
extern "C" {
    // Don't name it print to prevent name collisions
    pub fn print_str(s: &str);
    pub fn read_line_internal() -> String;
}

pub fn exit(_code: i32) {
    // Web wasm doesn't really have an exit functionality aside from returning early
    // So the best-effort approximation is to just crash the program.
    #[cfg(target_arch = "wasm32")]
    core::arch::wasm32::unreachable();
    // Infinite loop as fallback if we for some reason don't compile to wasm32
    #[cfg(not(target_arch = "wasm32"))]
    #[allow(clippy::empty_loop)]
    loop {}
}

#[wasm_bindgen]
pub fn main() {
    unsafe {
        _start();
    }
}

unsafe extern "C" {
    fn _start();
}
