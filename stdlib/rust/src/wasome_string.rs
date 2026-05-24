use crate::wasome_vec::WasomeVec;
use alloc::boxed::Box;
use alloc::string::{String, ToString};
use core::fmt::Display;
use core::mem::forget;

#[repr(transparent)]
pub struct WasomeString {
    inner: WasomeVec<u8>,
}

impl Default for WasomeString {
    fn default() -> Self {
        Self::new()
    }
}

impl WasomeString {
    #[must_use]
    pub fn new() -> Self {
        Self {
            inner: WasomeVec::new(),
        }
    }

    /// # Safety
    ///
    /// `of` must be a valid pointer.
    ///
    /// Should the drop function of the resulting `String` be called and then this again, it is UB.
    /// In other words, executing the drop function of the string invalidates self.
    pub unsafe fn as_string(of: *mut Self) -> String {
        unsafe { String::from_utf8_unchecked(WasomeVec::as_vec(&raw mut (*of).inner)) }
    }

    /// # Safety
    ///
    /// `of` must be a valid pointer.
    ///
    /// This does not call the drop function of string.
    pub unsafe fn update_from_string(of: *mut Self, string: String) {
        let this = unsafe { &mut *of };
        let vec = string.into_bytes();
        this.inner = vec.into();
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
        let refc = unsafe { &mut *to_handle }.inner.refc;
        if refc == 1 {
            unsafe { Self::drop(to_handle) }
        } else {
            unsafe { &mut *to_handle }.inner.refc = refc - 1;
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
