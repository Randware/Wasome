#![no_std]

extern crate alloc;
mod wasome_mem;
mod wasome_vec;

use alloc::string::String;
use core::panic::PanicInfo;

#[link(wasm_import_module = "wasi_snapshot_preview1")]
unsafe extern "C" {
    fn fd_write(fd: i32, iovs: *const Iovec, iovs_len: usize, nwritten: *mut usize) -> i32;
    safe fn proc_exit(code: i32);
}
#[repr(C)]
struct Iovec {
    buf: *const u8,
    len: usize,
}
#[global_allocator]
static A: dlmalloc::GlobalDlmalloc = dlmalloc::GlobalDlmalloc;
/// # Safety
///
/// The passed char must be a valid char
#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_char(to_print: u32) {
    unsafe {
        print(String::from(char::from_u32_unchecked(to_print)).as_str());
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn panic() -> ! {
    panic_internal(None)
}

#[repr(C)]
pub struct WasomeComposite {
    refc: *mut u32,
}

impl WasomeComposite {
    /// # Safety
    ///
    /// `of` must be a valid (and not null) pointer
    pub unsafe fn inc_rc(of: *mut Self) {
        unsafe { *(*of).refc += 1 }
    }
}

fn print(s: &str) {
    let iovec = Iovec {
        buf: s.as_ptr(),
        len: s.len(),
    };
    let mut nwritten: usize = 0;
    // SAFETY:
    // FD 1 always exists
    unsafe {
        fd_write(1, &iovec, 1, &mut nwritten);
    }
}

fn exit(code: i32) {
    proc_exit(code)
}

// Prevent errors
#[cfg(not(test))]
#[panic_handler]
fn rust_panic(info: &PanicInfo) -> ! {
    panic_internal(info.message().as_str())
}

fn panic_internal(msg: Option<&str>) -> ! {
    print("\nPANIC!\n");
    if let Some(msg) = msg {
        print(msg);
        print("\n");
    }
    print("\n");
    exit(1);
    #[cfg(target_arch = "wasm32")]
    core::arch::wasm32::unreachable();
    // Infinite loop as final fallback if we for some reason don't compile to wasm32
    #[cfg(not(target_arch = "wasm32"))]
    #[allow(clippy::empty_loop)]
    loop {}
}

#[cfg(test)]
mod tests {
    #[test]
    fn test() {
        assert_eq!(1, 1);
    }
}