#![no_std]

extern crate alloc;
mod wasome_mem;
mod wasome_string;
mod wasome_vec;

use crate::wasome_string::WasomeString;
use alloc::string::String;
use core::mem::forget;
use core::panic::PanicInfo;

#[link(wasm_import_module = "wasi_snapshot_preview1")]
unsafe extern "C" {
    fn fd_write(fd: i32, iovs: *const Iovec, iovs_len: usize, nwritten: *mut usize) -> i32;
    fn fd_read(fd: i32, iovs: *const MutIovec, iovs_len: usize, nwritten: *mut usize) -> i32;
    safe fn proc_exit(code: i32);
}
#[repr(C)]
struct Iovec {
    buf: *const u8,
    len: usize,
}

#[repr(C)]
struct MutIovec {
    buf: *mut u8,
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

/// # Safety
///
/// The passed char must be a valid char
#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_string(to_print: *mut WasomeString) {
    let string = unsafe { WasomeString::as_string(to_print) };
    print(&string);
    forget(string)
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

fn read_line() -> String {
    let mut res = String::new();
    let mut curr_char: u32 = 0;
    loop {
        let mut buf = [0_u8; 1];
        let iovec = MutIovec {
            buf: buf.as_mut_ptr(),
            len: 1,
        };
        let mut nwritten: usize = 0;
        // SAFETY:
        // FD 1 always exists
        unsafe {
            fd_read(0, &iovec, 1, &mut nwritten);
        }
        if nwritten != 1 {
            panic!("Read failed! {nwritten}");
        }
        drop(iovec);
        curr_char <<= 8;
        curr_char |= buf[0] as u32;
        if let Some(converted) = char::from_u32(curr_char) {
            if curr_char == b'\n' as u32 {
                break;
            }
            res.push(converted);
            curr_char = 0;
        }
    }
    res
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
    use super::*;
    use alloc::boxed::Box;

    #[test]
    fn test() {
        assert_eq!(1, 1);
    }

    #[test]
    fn test_wasome_composite_initial_refc() {
        let mut refc = 42u32;
        let composite = WasomeComposite {
            refc: &mut refc as *mut u32,
        };
        assert_eq!(unsafe { *composite.refc }, 42);
    }

    #[test]
    fn test_wasome_composite_inc_rc() {
        let mut refc = 1u32;
        let composite = WasomeComposite {
            refc: &mut refc as *mut u32,
        };
        let ptr = &composite as *const WasomeComposite as *mut WasomeComposite;
        unsafe { WasomeComposite::inc_rc(ptr) };
        assert_eq!(refc, 2);
    }

    #[test]
    fn test_wasome_composite_inc_rc_multiple() {
        let mut refc = 0u32;
        let composite = WasomeComposite {
            refc: &mut refc as *mut u32,
        };
        let ptr = &composite as *const WasomeComposite as *mut WasomeComposite;
        unsafe { WasomeComposite::inc_rc(ptr) };
        unsafe { WasomeComposite::inc_rc(ptr) };
        unsafe { WasomeComposite::inc_rc(ptr) };
        assert_eq!(refc, 3);
    }

    #[test]
    fn test_wasome_composite_inc_rc_from_zero() {
        let mut refc = 0u32;
        let composite = WasomeComposite {
            refc: &mut refc as *mut u32,
        };
        let ptr = &composite as *const WasomeComposite as *mut WasomeComposite;
        unsafe { WasomeComposite::inc_rc(ptr) };
        assert_eq!(refc, 1);
    }

    #[test]
    fn test_wasome_composite_inc_rc_many() {
        let mut refc = 100u32;
        let composite = WasomeComposite {
            refc: &mut refc as *mut u32,
        };
        let ptr = &composite as *const WasomeComposite as *mut WasomeComposite;
        for _ in 0..50 {
            unsafe { WasomeComposite::inc_rc(ptr) };
        }
        assert_eq!(refc, 150);
    }

    #[test]
    fn test_wasome_composite_with_boxed() {
        let mut refc = 1u32;
        let boxed = Box::new(WasomeComposite {
            refc: &mut refc as *mut u32,
        });
        let ptr = Box::into_raw(boxed);
        unsafe { WasomeComposite::inc_rc(ptr) };
        assert_eq!(refc, 2);
        unsafe { drop(Box::from_raw(ptr)) };
    }

    #[test]
    fn test_wasome_composite_inc_rc_via_ptr() {
        let mut refc = 1u32;
        let mut composite = WasomeComposite {
            refc: &mut refc as *mut u32,
        };
        let ptr = &mut composite;
        unsafe { WasomeComposite::inc_rc(ptr) };
        assert_eq!(refc, 2);
    }

    #[test]
    fn test_wasome_composite_repr_c_layout() {
        assert_eq!(
            core::mem::size_of::<WasomeComposite>(),
            core::mem::size_of::<*mut u32>()
        );
    }
}
