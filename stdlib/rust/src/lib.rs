#![no_std]
extern crate alloc;
//extern crate core;

mod wasome_mem;
mod wasome_string;
mod wasome_vec;
mod plattform;

use crate::wasome_string::WasomeString;
use alloc::boxed::Box;
use alloc::string::String;
use core::mem::forget;
use core::panic::PanicInfo;
use crate::plattform::{exit, print_str, read_line_internal};

#[global_allocator]
static A: dlmalloc::GlobalDlmalloc = dlmalloc::GlobalDlmalloc;
/// # Safety
///
/// The passed char must be a valid char
#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_char(to_print: u32) {
    unsafe {
        print_str(String::from(char::from_u32_unchecked(to_print)).as_str());
    }
}

/// # Safety
///
/// The passed char must be a valid char
#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_string(to_print: *mut WasomeString) {
    let string = unsafe { WasomeString::as_string(to_print) };
    print_str(&string);
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

#[unsafe(no_mangle)]
pub extern "C" fn read_line() -> *mut WasomeString {
    let wasome_string = Box::into_raw(Box::new(WasomeString::new()));
    unsafe { WasomeString::update_from_string(wasome_string, read_line_internal()) }
    wasome_string
}

fn panic_internal(msg: Option<&str>) -> ! {
    print_str("\nPANIC!\n");
    if let Some(msg) = msg {
        print_str(msg);
        print_str("\n");
    }
    print_str("\n");
    exit(1);
    #[cfg(target_arch = "wasm32")]
    core::arch::wasm32::unreachable();
    // Infinite loop as final fallback if we for some reason don't compile to wasm32
    #[cfg(not(target_arch = "wasm32"))]
    #[allow(clippy::empty_loop)]
    loop {}
}

#[cfg(not(test))]
#[panic_handler]
fn rust_panic(info: &PanicInfo) -> ! {
    panic_internal(info.message().as_str())
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
