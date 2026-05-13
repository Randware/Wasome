use crate::WasomeComposite;
use alloc::boxed::Box;
use alloc::vec::Vec;
use core::mem::forget;

#[repr(C)]
pub struct WasomeVec<T: Copy> {
    refc: u32,
    prt: *mut T,
    cnt: usize,
    cap: usize,
}

impl<T: Copy> WasomeVec<T> {
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

#[unsafe(no_mangle)]
pub extern "C" fn vec_new_1() -> *mut WasomeVec<u8> {
    Box::<WasomeVec<_>>::into_raw(Box::new(WasomeVec::new()))
}

#[unsafe(no_mangle)]
pub extern "C" fn vec_new_2() -> *mut WasomeVec<u16> {
    Box::<WasomeVec<_>>::into_raw(Box::new(WasomeVec::new()))
}

#[unsafe(no_mangle)]
pub extern "C" fn vec_new_4() -> *mut WasomeVec<u32> {
    Box::<WasomeVec<_>>::into_raw(Box::new(WasomeVec::new()))
}

#[unsafe(no_mangle)]
pub extern "C" fn vec_new_8() -> *mut WasomeVec<u64> {
    Box::<WasomeVec<_>>::into_raw(Box::new(WasomeVec::new()))
}

#[unsafe(no_mangle)]
pub extern "C" fn vec_new_ptr() -> *mut WasomeVec<*mut WasomeComposite> {
    Box::<WasomeVec<_>>::into_raw(Box::new(WasomeVec::new()))
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
