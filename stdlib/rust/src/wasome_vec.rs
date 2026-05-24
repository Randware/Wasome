use crate::WasomeComposite;
use alloc::boxed::Box;
use alloc::vec::Vec;
use core::mem::forget;

#[repr(C)]
pub struct WasomeVec<T: Copy> {
    pub refc: u32,
    prt: *mut T,
    cnt: usize,
    cap: usize,
}

impl<T: Copy> WasomeVec<T> {
    #[must_use]
    pub fn new() -> Self {
        let mut vec: Vec<T> = Vec::with_capacity(10);
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
    /// Should the drop function of the resulting `Vec` be called and then this again, it is UB.
    /// In other words, executing the drop function of the vec invalidates self.
    pub unsafe fn as_vec(of: *mut Self) -> Vec<T> {
        let this = unsafe { &mut *of };
        unsafe { Vec::from_raw_parts(this.prt, this.cnt, this.cap) }
    }

    /// # Safety
    ///
    /// `of` must be a valid pointer.
    ///
    /// This does not call the drop function of vec.
    pub unsafe fn update_from_vec(of: *mut Self, mut vec: Vec<T>) {
        let this = unsafe { &mut *of };
        this.cnt = vec.len();
        this.cap = vec.capacity();
        this.prt = vec.as_mut_ptr();
        // Don't actually dealloc the memory
        forget(vec);
    }

    /// # Safety
    ///
    /// `of` must be a valid pointer.
    pub unsafe fn push(of: *mut Self, elem: T) {
        // SAFETY:
        // We do not run the drop function
        let mut vec = unsafe { Self::as_vec(of) };
        vec.push(elem);
        unsafe { Self::update_from_vec(of, vec) };
    }

    /// # Safety
    ///
    /// `of` must be a valid pointer.
    pub unsafe fn pop(of: *mut Self) -> T {
        // SAFETY:
        // We do not run the drop function
        let mut vec = unsafe { Self::as_vec(of) };
        let elem = vec.pop().expect("Pop on empty vec!");
        unsafe { Self::update_from_vec(of, vec) };
        elem
    }

    /// # Safety
    ///
    /// `of` must be a valid pointer.
    pub unsafe fn remove(of: *mut Self, index: usize) -> T {
        // SAFETY:
        // We do not run the drop function
        let mut vec = unsafe { Self::as_vec(of) };
        let elem = vec.remove(index);
        unsafe { Self::update_from_vec(of, vec) };
        elem
    }

    /// # Safety
    ///
    /// `of` must be a valid pointer.
    pub unsafe fn get(of: *mut Self, index: usize) -> T {
        // SAFETY:
        // We do not run the drop function
        let vec = unsafe { Self::as_vec(of) };
        let elem = vec[index];
        forget(vec);
        elem
    }

    /// # Safety
    ///
    /// `of` must be a valid pointer.
    pub unsafe fn set(of: *mut Self, index: usize, new: T) -> T {
        // SAFETY:
        // We do not run the drop function
        let mut vec = unsafe { Self::as_vec(of) };
        let elem = vec[index];
        vec[index] = new;
        // There is no need to update the vec as only the allocated area changed
        forget(vec);
        elem
    }
}

impl<T: Copy> From<Vec<T>> for WasomeVec<T> {
    fn from(mut value: Vec<T>) -> Self {
        let cnt = value.len();
        let cap = value.capacity();
        let wasome_vec = Self {
            refc: 1,
            prt: value.as_mut_ptr(),
            cnt,
            cap,
        };
        forget(value);
        wasome_vec
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn vec_new_1() -> *mut WasomeVec<u8> {
    Box::into_raw(Box::new(WasomeVec::new()))
}

#[unsafe(no_mangle)]
pub extern "C" fn vec_new_2() -> *mut WasomeVec<u16> {
    Box::into_raw(Box::new(WasomeVec::new()))
}

#[unsafe(no_mangle)]
pub extern "C" fn vec_new_4() -> *mut WasomeVec<u32> {
    Box::into_raw(Box::new(WasomeVec::new()))
}

#[unsafe(no_mangle)]
pub extern "C" fn vec_new_8() -> *mut WasomeVec<u64> {
    Box::into_raw(Box::new(WasomeVec::new()))
}

#[unsafe(no_mangle)]
pub extern "C" fn vec_new_ptr() -> *mut WasomeVec<*mut WasomeComposite> {
    Box::into_raw(Box::new(WasomeVec::new()))
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_push_1(mut_vec: *mut WasomeVec<u8>, elem: u8) {
    unsafe { WasomeVec::push(mut_vec, elem) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_push_2(mut_vec: *mut WasomeVec<u16>, elem: u16) {
    unsafe { WasomeVec::push(mut_vec, elem) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_push_4(mut_vec: *mut WasomeVec<u32>, elem: u32) {
    unsafe { WasomeVec::push(mut_vec, elem) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_push_8(mut_vec: *mut WasomeVec<u64>, elem: u64) {
    unsafe { WasomeVec::push(mut_vec, elem) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
/// `elem` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_push_ptr(
    mut_vec: *mut WasomeVec<*mut WasomeComposite>,
    elem: *mut WasomeComposite,
) {
    unsafe { WasomeVec::push(mut_vec, elem) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_pop_1(mut_vec: *mut WasomeVec<u8>) -> u8 {
    unsafe { WasomeVec::pop(mut_vec) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_pop_2(mut_vec: *mut WasomeVec<u16>) -> u16 {
    unsafe { WasomeVec::pop(mut_vec) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_pop_4(mut_vec: *mut WasomeVec<u32>) -> u32 {
    unsafe { WasomeVec::pop(mut_vec) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_pop_8(mut_vec: *mut WasomeVec<u64>) -> u64 {
    unsafe { WasomeVec::pop(mut_vec) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_pop_ptr(
    mut_vec: *mut WasomeVec<*mut WasomeComposite>,
) -> *mut WasomeComposite {
    unsafe { WasomeVec::pop(mut_vec) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_remove_1(mut_vec: *mut WasomeVec<u8>, index: usize) -> u8 {
    unsafe { WasomeVec::remove(mut_vec, index) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_remove_2(mut_vec: *mut WasomeVec<u16>, index: usize) -> u16 {
    unsafe { WasomeVec::remove(mut_vec, index) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_remove_4(mut_vec: *mut WasomeVec<u32>, index: usize) -> u32 {
    unsafe { WasomeVec::remove(mut_vec, index) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_remove_8(mut_vec: *mut WasomeVec<u64>, index: usize) -> u64 {
    unsafe { WasomeVec::remove(mut_vec, index) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_remove_ptr(
    mut_vec: *mut WasomeVec<*mut WasomeComposite>,
    index: usize,
) -> *mut WasomeComposite {
    unsafe { WasomeVec::remove(mut_vec, index) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_get_1(mut_vec: *mut WasomeVec<u8>, index: usize) -> u8 {
    unsafe { WasomeVec::get(mut_vec, index) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_get_2(mut_vec: *mut WasomeVec<u16>, index: usize) -> u16 {
    unsafe { WasomeVec::get(mut_vec, index) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_get_4(mut_vec: *mut WasomeVec<u32>, index: usize) -> u32 {
    unsafe { WasomeVec::get(mut_vec, index) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_get_8(mut_vec: *mut WasomeVec<u64>, index: usize) -> u64 {
    unsafe { WasomeVec::get(mut_vec, index) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_get_ptr(
    mut_vec: *mut WasomeVec<*mut WasomeComposite>,
    index: usize,
) -> *mut WasomeComposite {
    let val = unsafe { WasomeVec::get(mut_vec, index) };
    // SAFETY:
    //
    // WasomeVec always returns valid pointers
    unsafe {
        WasomeComposite::inc_rc(val);
    }
    val
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_set_1(mut_vec: *mut WasomeVec<u8>, index: usize, new: u8) -> u8 {
    unsafe { WasomeVec::set(mut_vec, index, new) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_set_2(mut_vec: *mut WasomeVec<u16>, index: usize, new: u16) -> u16 {
    unsafe { WasomeVec::set(mut_vec, index, new) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_set_4(mut_vec: *mut WasomeVec<u32>, index: usize, new: u32) -> u32 {
    unsafe { WasomeVec::set(mut_vec, index, new) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_set_8(mut_vec: *mut WasomeVec<u64>, index: usize, new: u64) -> u64 {
    unsafe { WasomeVec::set(mut_vec, index, new) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
/// `new` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_set_ptr(
    mut_vec: *mut WasomeVec<*mut WasomeComposite>,
    index: usize,
    new: *mut WasomeComposite,
) -> *mut WasomeComposite {
    unsafe { WasomeVec::set(mut_vec, index, new) }
}

/// # Safety
///
/// `mut_vec` must be a valid pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_drop(mut_vec: *mut WasomeVec<*mut WasomeComposite>) {
    unsafe {
        WasomeVec::as_vec(mut_vec);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ===================== vec_new tests =====================

    #[test]
    fn test_vec_new_1_returns_non_null() {
        let ptr = vec_new_1();
        assert!(!ptr.is_null());
        // SAFETY: vec_new_1 always returns a valid heap-allocated pointer
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 0);
            assert_eq!(vec.capacity(), 10);
        }
    }

    #[test]
    fn test_vec_new_2_returns_non_null() {
        let ptr = vec_new_2();
        assert!(!ptr.is_null());
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 0);
            assert_eq!(vec.capacity(), 10);
        }
    }

    #[test]
    fn test_vec_new_4_returns_non_null() {
        let ptr = vec_new_4();
        assert!(!ptr.is_null());
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 0);
            assert_eq!(vec.capacity(), 10);
        }
    }

    #[test]
    fn test_vec_new_8_returns_non_null() {
        let ptr = vec_new_8();
        assert!(!ptr.is_null());
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 0);
            assert_eq!(vec.capacity(), 10);
        }
    }

    #[test]
    fn test_vec_new_ptr_returns_non_null() {
        let ptr = vec_new_ptr();
        assert!(!ptr.is_null());
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 0);
            assert_eq!(vec.capacity(), 10);
        }
    }

    // ===================== vec_push tests =====================

    #[test]
    fn test_vec_push_1_single_element() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 42) };
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 1);
            assert_eq!(vec[0], 42);
        }
    }

    #[test]
    fn test_vec_push_1_multiple_elements() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 1) };
        unsafe { vec_push_1(ptr, 2) };
        unsafe { vec_push_1(ptr, 3) };
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 3);
            assert_eq!(vec[0], 1);
            assert_eq!(vec[1], 2);
            assert_eq!(vec[2], 3);
        }
    }

    #[test]
    fn test_vec_push_1_boundary_values() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 0) };
        unsafe { vec_push_1(ptr, 255) };
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec[0], 0);
            assert_eq!(vec[1], 255);
        }
    }

    #[test]
    fn test_vec_push_2_single_element() {
        let ptr = vec_new_2();
        unsafe { vec_push_2(ptr, 1000) };
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 1);
            assert_eq!(vec[0], 1000);
        }
    }

    #[test]
    fn test_vec_push_2_multiple_elements() {
        let ptr = vec_new_2();
        unsafe { vec_push_2(ptr, 1) };
        unsafe { vec_push_2(ptr, 2) };
        unsafe { vec_push_2(ptr, 3) };
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 3);
            assert_eq!(vec[0], 1);
            assert_eq!(vec[1], 2);
            assert_eq!(vec[2], 3);
        }
    }

    #[test]
    fn test_vec_push_2_boundary_values() {
        let ptr = vec_new_2();
        unsafe { vec_push_2(ptr, 0) };
        unsafe { vec_push_2(ptr, u16::MAX) };
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec[0], 0);
            assert_eq!(vec[1], u16::MAX);
        }
    }

    #[test]
    fn test_vec_push_4_single_element() {
        let ptr = vec_new_4();
        unsafe { vec_push_4(ptr, 100000) };
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 1);
            assert_eq!(vec[0], 100000);
        }
    }

    #[test]
    fn test_vec_push_4_multiple_elements() {
        let ptr = vec_new_4();
        unsafe { vec_push_4(ptr, 1) };
        unsafe { vec_push_4(ptr, 2) };
        unsafe { vec_push_4(ptr, 3) };
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 3);
            assert_eq!(vec[0], 1);
            assert_eq!(vec[1], 2);
            assert_eq!(vec[2], 3);
        }
    }

    #[test]
    fn test_vec_push_4_boundary_values() {
        let ptr = vec_new_4();
        unsafe { vec_push_4(ptr, 0) };
        unsafe { vec_push_4(ptr, u32::MAX) };
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec[0], 0);
            assert_eq!(vec[1], u32::MAX);
        }
    }

    #[test]
    fn test_vec_push_8_single_element() {
        let ptr = vec_new_8();
        unsafe { vec_push_8(ptr, 10000000000) };
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 1);
            assert_eq!(vec[0], 10000000000);
        }
    }

    #[test]
    fn test_vec_push_8_multiple_elements() {
        let ptr = vec_new_8();
        unsafe { vec_push_8(ptr, 1) };
        unsafe { vec_push_8(ptr, 2) };
        unsafe { vec_push_8(ptr, 3) };
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 3);
            assert_eq!(vec[0], 1);
            assert_eq!(vec[1], 2);
            assert_eq!(vec[2], 3);
        }
    }

    #[test]
    fn test_vec_push_8_boundary_values() {
        let ptr = vec_new_8();
        unsafe { vec_push_8(ptr, 0) };
        unsafe { vec_push_8(ptr, u64::MAX) };
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec[0], 0);
            assert_eq!(vec[1], u64::MAX);
        }
    }

    #[test]
    fn test_vec_push_ptr() {
        let mut refc = 1u32;
        let composite = Box::into_raw(Box::new(WasomeComposite {
            refc: &mut refc as *mut u32,
        }));
        let ptr = vec_new_ptr();
        unsafe { vec_push_ptr(ptr, composite) };
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 1);
            assert_eq!(vec[0], composite);
        }
    }

    // ===================== vec_pop tests =====================

    #[test]
    fn test_vec_pop_1_single_element() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 42) };
        let result = unsafe { vec_pop_1(ptr) };
        assert_eq!(result, 42);
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 0);
        }
    }

    #[test]
    fn test_vec_pop_1_multiple_elements_fifo() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 1) };
        unsafe { vec_push_1(ptr, 2) };
        unsafe { vec_push_1(ptr, 3) };
        assert_eq!(unsafe { vec_pop_1(ptr) }, 3);
        assert_eq!(unsafe { vec_pop_1(ptr) }, 2);
        assert_eq!(unsafe { vec_pop_1(ptr) }, 1);
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 0);
        }
    }

    #[test]
    fn test_vec_pop_1_boundary_values() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 0) };
        unsafe { vec_push_1(ptr, 255) };
        assert_eq!(unsafe { vec_pop_1(ptr) }, 255);
        assert_eq!(unsafe { vec_pop_1(ptr) }, 0);
    }

    #[test]
    fn test_vec_pop_2_single_element() {
        let ptr = vec_new_2();
        unsafe { vec_push_2(ptr, 1000) };
        assert_eq!(unsafe { vec_pop_2(ptr) }, 1000);
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 0);
        }
    }

    #[test]
    fn test_vec_pop_2_multiple_elements() {
        let ptr = vec_new_2();
        unsafe { vec_push_2(ptr, 10) };
        unsafe { vec_push_2(ptr, 20) };
        unsafe { vec_push_2(ptr, 30) };
        assert_eq!(unsafe { vec_pop_2(ptr) }, 30);
        assert_eq!(unsafe { vec_pop_2(ptr) }, 20);
        assert_eq!(unsafe { vec_pop_2(ptr) }, 10);
    }

    #[test]
    fn test_vec_pop_4_single_element() {
        let ptr = vec_new_4();
        unsafe { vec_push_4(ptr, 100000) };
        assert_eq!(unsafe { vec_pop_4(ptr) }, 100000);
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 0);
        }
    }

    #[test]
    fn test_vec_pop_4_multiple_elements() {
        let ptr = vec_new_4();
        unsafe { vec_push_4(ptr, 100) };
        unsafe { vec_push_4(ptr, 200) };
        unsafe { vec_push_4(ptr, 300) };
        assert_eq!(unsafe { vec_pop_4(ptr) }, 300);
        assert_eq!(unsafe { vec_pop_4(ptr) }, 200);
        assert_eq!(unsafe { vec_pop_4(ptr) }, 100);
    }

    #[test]
    fn test_vec_pop_8_single_element() {
        let ptr = vec_new_8();
        unsafe { vec_push_8(ptr, 10000000000) };
        assert_eq!(unsafe { vec_pop_8(ptr) }, 10000000000);
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 0);
        }
    }

    #[test]
    fn test_vec_pop_8_multiple_elements() {
        let ptr = vec_new_8();
        unsafe { vec_push_8(ptr, 100) };
        unsafe { vec_push_8(ptr, 200) };
        unsafe { vec_push_8(ptr, 300) };
        assert_eq!(unsafe { vec_pop_8(ptr) }, 300);
        assert_eq!(unsafe { vec_pop_8(ptr) }, 200);
        assert_eq!(unsafe { vec_pop_8(ptr) }, 100);
    }

    #[test]
    fn test_vec_pop_ptr() {
        let mut refc = 1u32;
        let composite = Box::into_raw(Box::new(WasomeComposite {
            refc: &mut refc as *mut u32,
        }));
        let ptr = vec_new_ptr();
        unsafe { vec_push_ptr(ptr, composite) };
        let result = unsafe { vec_pop_ptr(ptr) };
        assert_eq!(result, composite);
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 0);
        }
    }

    // ===================== vec_remove tests =====================

    #[test]
    fn test_vec_remove_1_first_element() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 1) };
        unsafe { vec_push_1(ptr, 2) };
        unsafe { vec_push_1(ptr, 3) };
        let result = unsafe { vec_remove_1(ptr, 0) };
        assert_eq!(result, 1);
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 2);
            assert_eq!(vec[0], 2);
            assert_eq!(vec[1], 3);
        }
    }

    #[test]
    fn test_vec_remove_1_middle_element() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 1) };
        unsafe { vec_push_1(ptr, 2) };
        unsafe { vec_push_1(ptr, 3) };
        let result = unsafe { vec_remove_1(ptr, 1) };
        assert_eq!(result, 2);
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 2);
            assert_eq!(vec[0], 1);
            assert_eq!(vec[1], 3);
        }
    }

    #[test]
    fn test_vec_remove_1_last_element() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 1) };
        unsafe { vec_push_1(ptr, 2) };
        unsafe { vec_push_1(ptr, 3) };
        let result = unsafe { vec_remove_1(ptr, 2) };
        assert_eq!(result, 3);
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 2);
            assert_eq!(vec[0], 1);
            assert_eq!(vec[1], 2);
        }
    }

    #[test]
    fn test_vec_remove_2() {
        let ptr = vec_new_2();
        unsafe { vec_push_2(ptr, 10) };
        unsafe { vec_push_2(ptr, 20) };
        unsafe { vec_push_2(ptr, 30) };
        let result = unsafe { vec_remove_2(ptr, 1) };
        assert_eq!(result, 20);
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 2);
            assert_eq!(vec[0], 10);
            assert_eq!(vec[1], 30);
        }
    }

    #[test]
    fn test_vec_remove_4() {
        let ptr = vec_new_4();
        unsafe { vec_push_4(ptr, 100) };
        unsafe { vec_push_4(ptr, 200) };
        unsafe { vec_push_4(ptr, 300) };
        let result = unsafe { vec_remove_4(ptr, 0) };
        assert_eq!(result, 100);
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 2);
            assert_eq!(vec[0], 200);
            assert_eq!(vec[1], 300);
        }
    }

    #[test]
    fn test_vec_remove_8() {
        let ptr = vec_new_8();
        unsafe { vec_push_8(ptr, 1000) };
        unsafe { vec_push_8(ptr, 2000) };
        unsafe { vec_push_8(ptr, 3000) };
        let result = unsafe { vec_remove_8(ptr, 2) };
        assert_eq!(result, 3000);
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 2);
            assert_eq!(vec[0], 1000);
            assert_eq!(vec[1], 2000);
        }
    }

    #[test]
    fn test_vec_remove_ptr() {
        let mut refc = 1u32;
        let composite = Box::into_raw(Box::new(WasomeComposite {
            refc: &mut refc as *mut u32,
        }));
        let ptr = vec_new_ptr();
        unsafe { vec_push_ptr(ptr, composite) };
        unsafe { vec_push_ptr(ptr, composite) };
        let result = unsafe { vec_remove_ptr(ptr, 0) };
        assert_eq!(result, composite);
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 1);
        }
    }

    // ===================== vec_get tests =====================

    #[test]
    fn test_vec_get_1_first_element() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 42) };
        unsafe { vec_push_1(ptr, 99) };
        assert_eq!(unsafe { vec_get_1(ptr, 0) }, 42);
    }

    #[test]
    fn test_vec_get_1_last_element() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 42) };
        unsafe { vec_push_1(ptr, 99) };
        assert_eq!(unsafe { vec_get_1(ptr, 1) }, 99);
    }

    #[test]
    fn test_vec_get_1_single_element() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 42) };
        assert_eq!(unsafe { vec_get_1(ptr, 0) }, 42);
    }

    #[test]
    fn test_vec_get_1_many_elements() {
        let ptr = vec_new_1();
        for i in 0..20u8 {
            unsafe { vec_push_1(ptr, i) };
        }
        for i in 0..20u8 {
            assert_eq!(unsafe { vec_get_1(ptr, i as usize) }, i);
        }
    }

    #[test]
    fn test_vec_get_2() {
        let ptr = vec_new_2();
        unsafe { vec_push_2(ptr, 100) };
        unsafe { vec_push_2(ptr, 200) };
        unsafe { vec_push_2(ptr, 300) };
        assert_eq!(unsafe { vec_get_2(ptr, 0) }, 100);
        assert_eq!(unsafe { vec_get_2(ptr, 1) }, 200);
        assert_eq!(unsafe { vec_get_2(ptr, 2) }, 300);
    }

    #[test]
    fn test_vec_get_4() {
        let ptr = vec_new_4();
        unsafe { vec_push_4(ptr, 1000) };
        unsafe { vec_push_4(ptr, 2000) };
        assert_eq!(unsafe { vec_get_4(ptr, 0) }, 1000);
        assert_eq!(unsafe { vec_get_4(ptr, 1) }, 2000);
    }

    #[test]
    fn test_vec_get_8() {
        let ptr = vec_new_8();
        unsafe { vec_push_8(ptr, 1000000) };
        unsafe { vec_push_8(ptr, 2000000) };
        assert_eq!(unsafe { vec_get_8(ptr, 0) }, 1000000);
        assert_eq!(unsafe { vec_get_8(ptr, 1) }, 2000000);
    }

    #[test]
    fn test_vec_get_ptr() {
        let mut refc = 1u32;
        let composite = Box::into_raw(Box::new(WasomeComposite {
            refc: &mut refc as *mut u32,
        }));
        let ptr = vec_new_ptr();
        unsafe { vec_push_ptr(ptr, composite) };
        let result = unsafe { vec_get_ptr(ptr, 0) };
        assert_eq!(result, composite);
    }

    // ===================== vec_set tests =====================

    #[test]
    fn test_vec_set_1_first_element() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 10) };
        unsafe { vec_push_1(ptr, 20) };
        let old = unsafe { vec_set_1(ptr, 0, 99) };
        assert_eq!(old, 10);
        assert_eq!(unsafe { vec_get_1(ptr, 0) }, 99);
    }

    #[test]
    fn test_vec_set_1_last_element() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 10) };
        unsafe { vec_push_1(ptr, 20) };
        let old = unsafe { vec_set_1(ptr, 1, 99) };
        assert_eq!(old, 20);
        assert_eq!(unsafe { vec_get_1(ptr, 1) }, 99);
    }

    #[test]
    fn test_vec_set_1_single_element() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 42) };
        let old = unsafe { vec_set_1(ptr, 0, 100) };
        assert_eq!(old, 42);
        assert_eq!(unsafe { vec_get_1(ptr, 0) }, 100);
    }

    #[test]
    fn test_vec_set_1_multiple_replacements() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 1) };
        unsafe { vec_push_1(ptr, 2) };
        unsafe { vec_push_1(ptr, 3) };
        assert_eq!(unsafe { vec_set_1(ptr, 0, 10) }, 1);
        assert_eq!(unsafe { vec_set_1(ptr, 1, 20) }, 2);
        assert_eq!(unsafe { vec_set_1(ptr, 2, 30) }, 3);
        assert_eq!(unsafe { vec_get_1(ptr, 0) }, 10);
        assert_eq!(unsafe { vec_get_1(ptr, 1) }, 20);
        assert_eq!(unsafe { vec_get_1(ptr, 2) }, 30);
    }

    #[test]
    fn test_vec_set_2() {
        let ptr = vec_new_2();
        unsafe { vec_push_2(ptr, 100) };
        unsafe { vec_push_2(ptr, 200) };
        let old = unsafe { vec_set_2(ptr, 0, 999) };
        assert_eq!(old, 100);
        assert_eq!(unsafe { vec_get_2(ptr, 0) }, 999);
    }

    #[test]
    fn test_vec_set_4() {
        let ptr = vec_new_4();
        unsafe { vec_push_4(ptr, 1000) };
        unsafe { vec_push_4(ptr, 2000) };
        let old = unsafe { vec_set_4(ptr, 1, 9999) };
        assert_eq!(old, 2000);
        assert_eq!(unsafe { vec_get_4(ptr, 1) }, 9999);
    }

    #[test]
    fn test_vec_set_8() {
        let ptr = vec_new_8();
        unsafe { vec_push_8(ptr, 100000) };
        unsafe { vec_push_8(ptr, 200000) };
        let old = unsafe { vec_set_8(ptr, 0, 999999) };
        assert_eq!(old, 100000);
        assert_eq!(unsafe { vec_get_8(ptr, 0) }, 999999);
    }

    #[test]
    fn test_vec_set_ptr() {
        let mut refc1 = 1u32;
        let composite1 = Box::into_raw(Box::new(WasomeComposite {
            refc: &mut refc1 as *mut u32,
        }));
        let mut refc2 = 1u32;
        let composite2 = Box::into_raw(Box::new(WasomeComposite {
            refc: &mut refc2 as *mut u32,
        }));
        let ptr = vec_new_ptr();
        unsafe { vec_push_ptr(ptr, composite1) };
        let old = unsafe { vec_set_ptr(ptr, 0, composite2) };
        assert_eq!(old, composite1);
        assert_eq!(unsafe { vec_get_ptr(ptr, 0) }, composite2);
    }

    // ===================== vec_drop tests =====================

    #[test]
    fn test_vec_drop() {
        let mut refc = 1u32;
        let composite = Box::into_raw(Box::new(WasomeComposite {
            refc: &mut refc as *mut u32,
        }));
        let ptr = vec_new_ptr();
        unsafe { vec_push_ptr(ptr, composite) };
        unsafe { vec_push_ptr(ptr, composite) };
        unsafe { vec_drop(ptr) };
    }

    // ===================== Combined / stress tests =====================

    #[test]
    fn test_push_pop_all_elements_1() {
        let ptr = vec_new_1();
        for i in 0..100u8 {
            unsafe { vec_push_1(ptr, i) };
        }
        for i in (0..100).rev() {
            assert_eq!(unsafe { vec_pop_1(ptr) }, i);
        }
    }

    #[test]
    fn test_push_pop_all_elements_2() {
        let ptr = vec_new_2();
        for i in 0..100u16 {
            unsafe { vec_push_2(ptr, i) };
        }
        for i in (0..100).rev() {
            assert_eq!(unsafe { vec_pop_2(ptr) }, i);
        }
    }

    #[test]
    fn test_push_pop_all_elements_4() {
        let ptr = vec_new_4();
        for i in 0..100u32 {
            unsafe { vec_push_4(ptr, i) };
        }
        for i in (0..100).rev() {
            assert_eq!(unsafe { vec_pop_4(ptr) }, i);
        }
    }

    #[test]
    fn test_push_pop_all_elements_8() {
        let ptr = vec_new_8();
        for i in 0..100u64 {
            unsafe { vec_push_8(ptr, i) };
        }
        for i in (0..100).rev() {
            assert_eq!(unsafe { vec_pop_8(ptr) }, i);
        }
    }

    #[test]
    fn test_push_get_set_loop_1() {
        let ptr = vec_new_1();
        for i in 0..50u8 {
            unsafe { vec_push_1(ptr, i) };
        }
        for i in 0..50u8 {
            assert_eq!(unsafe { vec_get_1(ptr, i as usize) }, i);
            unsafe { vec_set_1(ptr, i as usize, 255 - i) };
            assert_eq!(unsafe { vec_get_1(ptr, i as usize) }, 255 - i);
        }
    }

    #[test]
    fn test_push_remove_get_loop_1() {
        let ptr = vec_new_1();
        for i in 0..20u8 {
            unsafe { vec_push_1(ptr, i) };
        }
        for i in 0..10u8 {
            assert_eq!(unsafe { vec_remove_1(ptr, 0) }, i);
        }
        for i in 0..10u8 {
            assert_eq!(unsafe { vec_get_1(ptr, i as usize) }, i + 10);
        }
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 10);
        }
    }

    #[test]
    fn test_alternate_push_pop_1() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 1) };
        unsafe { vec_push_1(ptr, 2) };
        assert_eq!(unsafe { vec_pop_1(ptr) }, 2);
        unsafe { vec_push_1(ptr, 3) };
        assert_eq!(unsafe { vec_pop_1(ptr) }, 3);
        assert_eq!(unsafe { vec_pop_1(ptr) }, 1);
    }

    #[test]
    fn test_many_pushes_then_all_pops_4() {
        let ptr = vec_new_4();
        for i in 0..200u32 {
            unsafe { vec_push_4(ptr, i * 4) };
        }
        for i in (0..200).rev() {
            assert_eq!(unsafe { vec_pop_4(ptr) }, i * 4);
        }
    }

    #[test]
    fn test_many_pushes_then_all_pops_8() {
        let ptr = vec_new_8();
        for i in 0..200u64 {
            unsafe { vec_push_8(ptr, i * 8) };
        }
        for i in (0..200).rev() {
            assert_eq!(unsafe { vec_pop_8(ptr) }, i * 8);
        }
    }

    // ===================== Edge case tests =====================

    #[test]
    fn test_push_empty_capacity() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 1) };
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert!(vec.capacity() >= 10);
        }
    }

    #[test]
    fn test_push_pop_preserves_order() {
        let ptr = vec_new_1();
        let values = [7u8, 3, 9, 1, 5];
        for &v in &values {
            unsafe { vec_push_1(ptr, v) };
        }
        for &v in values.iter().rev() {
            assert_eq!(unsafe { vec_pop_1(ptr) }, v);
        }
    }

    #[test]
    fn test_remove_preserves_adjacent_order_1() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 10) };
        unsafe { vec_push_1(ptr, 20) };
        unsafe { vec_push_1(ptr, 30) };
        unsafe { vec_push_1(ptr, 40) };
        unsafe { vec_push_1(ptr, 50) };
        unsafe { vec_remove_1(ptr, 2) };
        unsafe {
            let vec = WasomeVec::as_vec(ptr);
            assert_eq!(vec.len(), 4);
            assert_eq!(vec[0], 10);
            assert_eq!(vec[1], 20);
            assert_eq!(vec[2], 40);
            assert_eq!(vec[3], 50);
        }
    }

    #[test]
    fn test_set_get_same_index_multiple_times() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 5) };
        unsafe { vec_push_1(ptr, 10) };
        unsafe { vec_push_1(ptr, 15) };
        for _ in 0..10 {
            let old = unsafe { vec_set_1(ptr, 1, 99) };
            assert_eq!(old, 10);
            assert_eq!(unsafe { vec_get_1(ptr, 1) }, 99);
            let old = unsafe { vec_set_1(ptr, 1, 10) };
            assert_eq!(old, 99);
            assert_eq!(unsafe { vec_get_1(ptr, 1) }, 10);
        }
    }

    #[test]
    fn test_push_pop_1_zero_value() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 0) };
        assert_eq!(unsafe { vec_pop_1(ptr) }, 0);
    }

    #[test]
    fn test_push_pop_2_zero_value() {
        let ptr = vec_new_2();
        unsafe { vec_push_2(ptr, 0) };
        assert_eq!(unsafe { vec_pop_2(ptr) }, 0);
    }

    #[test]
    fn test_push_pop_4_zero_value() {
        let ptr = vec_new_4();
        unsafe { vec_push_4(ptr, 0) };
        assert_eq!(unsafe { vec_pop_4(ptr) }, 0);
    }

    #[test]
    fn test_push_pop_8_zero_value() {
        let ptr = vec_new_8();
        unsafe { vec_push_8(ptr, 0) };
        assert_eq!(unsafe { vec_pop_8(ptr) }, 0);
    }

    #[test]
    fn test_get_set_zero_index() {
        let ptr = vec_new_1();
        unsafe { vec_push_1(ptr, 55) };
        assert_eq!(unsafe { vec_set_1(ptr, 0, 66) }, 55);
        assert_eq!(unsafe { vec_get_1(ptr, 0) }, 66);
    }
}
