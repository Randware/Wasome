use alloc::boxed::Box;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::fmt::Display;
use core::mem::forget;

#[repr(C)]
pub struct WasomeString {
    pub refc: u32,
    prt: *mut u8,
    cnt: usize,
    cap: usize,
}

impl Default for WasomeString {
    fn default() -> Self {
        Self::new()
    }
}

impl WasomeString {
    #[must_use]
    pub fn new() -> Self {
        let mut vec: Vec<u8> = Vec::with_capacity(10);
        let cnt = vec.len();
        let cap = vec.capacity();
        let prt = vec.as_mut_ptr();
        // Don't actually dealloc the memory
        forget(vec);
        Self {
            refc: 1,
            prt,
            cnt,
            cap,
        }
    }

    /// # Safety
    ///
    /// `of` must be a valid pointer.
    ///
    /// Should the drop function of the resulting `String` be called and then this again, it is UB.
    /// In other words, executing the drop function of the string invalidates self.
    pub unsafe fn as_string(of: *mut Self) -> String {
        unsafe { String::from_utf8_unchecked(Vec::from_raw_parts((*of).prt, (*of).cnt, (*of).cap)) }
    }

    /// # Safety
    ///
    /// `of` must be a valid pointer.
    ///
    /// This does not call the drop function of string.
    pub unsafe fn update_from_string(of: *mut Self, string: String) {
        let this = unsafe { &mut *of };
        let mut vec = string.into_bytes();
        this.cap = vec.capacity();
        this.cnt = vec.len();
        this.prt = vec.as_mut_ptr();
        forget(vec);
    }

    /// # Safety
    ///
    /// `of` must be a valid pointer.
    pub unsafe fn push_char(of: *mut Self, elem: char) {
        // SAFETY:
        // We do not run the drop function
        let mut string = unsafe { Self::as_string(of) };
        string.push(elem);
        unsafe { Self::update_from_string(of, string) };
    }

    /// # Safety
    ///
    /// `of` must be a valid pointer.
    /// `other` must be a valid pointer
    pub unsafe fn push_string(of: *mut Self, other: *mut Self) {
        // SAFETY:
        // We do not run the drop functions
        let mut string = unsafe { Self::as_string(of) };
        let other_string = unsafe { Self::as_string(other) };

        string.push_str(&other_string);
        unsafe { Self::update_from_string(of, string) };
        forget(other_string);
        unsafe { Self::dec_refc(other) };
    }

    /// # Safety
    ///
    /// `of` must be a valid pointer.
    pub unsafe fn push_display(of: *mut Self, to_push: impl Display) {
        // SAFETY:
        // We do not run the drop function
        let mut string = unsafe { Self::as_string(of) };

        string.push_str(&to_push.to_string());
        unsafe { Self::update_from_string(of, string) };
    }

    /// # Safety
    ///
    /// `of` must be a valid pointer.
    pub unsafe fn pop(of: *mut Self) -> char {
        // SAFETY:
        // We do not run the drop function
        let mut string = unsafe { Self::as_string(of) };
        let elem = string.pop().expect("Pop on empty string!");
        unsafe { Self::update_from_string(of, string) };
        elem
    }

    /// # Safety
    ///
    /// `of` must be a valid pointer.
    pub unsafe fn dec_refc(to_handle: *mut Self) {
        let refc = unsafe { &mut *to_handle }.refc;
        if refc == 1 {
            unsafe { Self::drop(to_handle) }
        } else {
            unsafe { &mut *to_handle }.refc = refc - 1;
        }
    }

    /// # Safety
    ///
    /// `of` must be a valid pointer.
    pub unsafe fn drop(to_drop: *mut Self) {
        unsafe { Self::as_string(to_drop) };
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn string_new() -> *mut WasomeString {
    Box::into_raw(Box::new(WasomeString::new()))
}

/// # Safety
///
/// `mut_string` must be a valid pointer.
/// `elem` must be a valid UTF-8 char
#[unsafe(no_mangle)]
pub unsafe extern "C" fn string_push_char(mut_string: *mut WasomeString, elem: u32) {
    unsafe { WasomeString::push_char(mut_string, char::from_u32_unchecked(elem)) }
}

/// # Safety
///
/// `mut_string` must be a valid pointer.
/// `other` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn string_push_string(
    mut_string: *mut WasomeString,
    other: *mut WasomeString,
) {
    unsafe { WasomeString::push_string(mut_string, other) }
}

/// # Safety
///
/// `mut_string` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn string_push_s32(mut_string: *mut WasomeString, to_push: i32) {
    unsafe { WasomeString::push_display(mut_string, to_push) }
}

/// # Safety
///
/// `mut_string` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn string_push_f64(mut_string: *mut WasomeString, to_push: f64) {
    unsafe { WasomeString::push_display(mut_string, to_push) }
}

/// # Safety
///
/// `mut_string` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn string_push_s64(mut_string: *mut WasomeString, to_push: i64) {
    unsafe { WasomeString::push_display(mut_string, to_push) }
}

/// # Safety
///
/// `mut_string` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn string_push_u64(mut_string: *mut WasomeString, to_push: u64) {
    unsafe { WasomeString::push_display(mut_string, to_push) }
}

/// # Safety
///
/// `mut_string` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn string_pop(mut_string: *mut WasomeString) -> u32 {
    (unsafe { WasomeString::pop(mut_string) }) as u32
}

/// # Safety
///
/// `mut_string` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn string_drop(mut_string: *mut WasomeString) {
    unsafe { WasomeString::drop(mut_string) }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::print_string;

    #[test]
    fn new_wasome_string_is_valid() {
        let ws = string_new();
        assert_eq!(String::new(), unsafe { WasomeString::as_string(ws) })
    }

    #[test]
    fn wasome_string_push_char_contains_it() {
        let ws = string_new();
        unsafe {
            string_push_char(ws, b'W' as u32);
            string_push_char(ws, b'a' as u32);
            string_push_char(ws, b's' as u32);
            string_push_char(ws, b'o' as u32);
            string_push_char(ws, b'm' as u32);
            string_push_char(ws, b'e' as u32);
        }
        assert_eq!("Wasome".to_string(), unsafe { WasomeString::as_string(ws) })
    }

    #[test]
    fn wasome_string_push_string_contains_it() {
        let ws = string_new();
        unsafe {
            let to_push = string_new();
            WasomeString::update_from_string(to_push, "Wasome".to_string());
            string_push_string(ws, to_push);
        }
        assert_eq!("Wasome".to_string(), unsafe { WasomeString::as_string(ws) })
    }

    #[test]
    fn wasome_string_push_empty_string_stays_empty() {
        let ws = string_new();
        unsafe {
            let to_push = string_new();
            string_push_string(ws, to_push);
        }
        assert_eq!(String::new(), unsafe { WasomeString::as_string(ws) })
    }

    #[test]
    fn wasome_string_push_s32() {
        let ws = string_new();
        unsafe {
            string_push_s32(ws, -10);
        }
        assert_eq!("-10".to_string(), unsafe { WasomeString::as_string(ws) })
    }

    #[test]
    fn wasome_string_push_s64() {
        let ws = string_new();
        unsafe {
            string_push_s64(ws, -1000000000000000);
        }
        assert_eq!("-1000000000000000".to_string(), unsafe {
            WasomeString::as_string(ws)
        })
    }

    #[test]
    fn wasome_string_push_u64() {
        let ws = string_new();
        unsafe {
            string_push_u64(ws, 150);
        }
        assert_eq!("150".to_string(), unsafe { WasomeString::as_string(ws) })
    }

    #[test]
    fn wasome_string_push_f64() {
        let ws = string_new();
        unsafe {
            string_push_f64(ws, 17.3);
            string_push_f64(ws, 17.3);

            string_pop(ws);
            print_string(ws);
        }

        //assert_eq!("17.3".to_string(), unsafe { WasomeString::as_string(ws) })
    }

    #[test]
    fn wasome_string_push_pop() {
        let ws = string_new();
        unsafe {
            string_push_f64(ws, 17.3);
            assert_eq!(b'3' as u32, string_pop(ws))
        }
        assert_eq!("17.".to_string(), unsafe { WasomeString::as_string(ws) })
    }
}
