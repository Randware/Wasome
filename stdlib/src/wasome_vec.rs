use alloc::boxed::Box;
use alloc::vec::Vec;
use core::mem::forget;
use crate::WasomeComposite;

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

    unsafe fn as_vec(&mut self) -> Vec<T> {
        unsafe { Vec::from_raw_parts(self.prt, self.cnt, self.cap) }
    }

    fn update_from_vec(&mut self, mut vec: Vec<T>) {
        self.cnt = vec.len();
        self.cap = vec.capacity();
        self.prt = vec.as_mut_ptr();
        // Don't actually dealloc the memory
        forget(vec);
    }
    pub fn push(&mut self, elem: T) {
        let mut vec = unsafe { self.as_vec() };
        vec.push(elem);
        self.update_from_vec(vec);
    }

    pub fn pop(&mut self) -> T {
        let mut vec = unsafe { self.as_vec() };
        let elem = vec.pop().expect("Pop on empty vec!");
        self.update_from_vec(vec);
        elem
    }

    pub fn remove(&mut self, index: usize) -> T {
        let mut vec = unsafe { self.as_vec() };
        let elem = vec.remove(index);
        self.update_from_vec(vec);
        elem
    }

    pub fn get(&mut self, index: usize) -> T {
        let vec = unsafe { self.as_vec() };
        let elem = vec[index];
        forget(vec);
        elem
    }

    pub fn set(&mut self, index: usize, new: T) -> T {
        let mut vec = unsafe { self.as_vec() };
        let elem = vec[index];
        vec[index] = new;
        // There is no need to update the vec as only the allocated area changed
        forget(vec);
        elem
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_new_1() -> *mut WasomeVec<u8> {
    Box::<WasomeVec<_>>::into_raw(Box::new(WasomeVec::new()))
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_new_2() -> *mut WasomeVec<u16> {
    Box::<WasomeVec<_>>::into_raw(Box::new(WasomeVec::new()))
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_new_4() -> *mut WasomeVec<u32> {
    Box::<WasomeVec<_>>::into_raw(Box::new(WasomeVec::new()))
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_new_8() -> *mut WasomeVec<u64> {
    Box::<WasomeVec<_>>::into_raw(Box::new(WasomeVec::new()))
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_new_ptr() -> *mut WasomeVec<*mut WasomeComposite> {
    Box::<WasomeVec<_>>::into_raw(Box::new(WasomeVec::new()))
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_push_1(mut_vec: *mut WasomeVec<u8>, elem: u8) {
    unsafe { (*mut_vec).push(elem) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_push_2(mut_vec: *mut WasomeVec<u16>, elem: u16) {
    unsafe { (*mut_vec).push(elem) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_push_4(mut_vec: *mut WasomeVec<u32>, elem: u32) {
    unsafe { (*mut_vec).push(elem) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_push_8(mut_vec: *mut WasomeVec<u64>, elem: u64) {
    unsafe { (*mut_vec).push(elem) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_push_ptr(
    mut_vec: *mut WasomeVec<*mut WasomeComposite>,
    elem: *mut WasomeComposite,
) {
    unsafe { (*mut_vec).push(elem) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_pop_1(mut_vec: *mut WasomeVec<u8>) -> u8 {
    unsafe { (*mut_vec).pop() }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_pop_2(mut_vec: *mut WasomeVec<u16>) -> u16 {
    unsafe { (*mut_vec).pop() }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_pop_4(mut_vec: *mut WasomeVec<u32>) -> u32 {
    unsafe { (*mut_vec).pop() }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_pop_8(mut_vec: *mut WasomeVec<u64>) -> u64 {
    unsafe { (*mut_vec).pop() }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_pop_ptr(
    mut_vec: *mut WasomeVec<*mut WasomeComposite>,
) -> *mut WasomeComposite {
    unsafe { (*mut_vec).pop() }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_remove_1(mut_vec: *mut WasomeVec<u8>, index: usize) -> u8 {
    unsafe { (*mut_vec).remove(index) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_remove_2(mut_vec: *mut WasomeVec<u16>, index: usize) -> u16 {
    unsafe { (*mut_vec).remove(index) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_remove_4(mut_vec: *mut WasomeVec<u32>, index: usize) -> u32 {
    unsafe { (*mut_vec).remove(index) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_remove_8(mut_vec: *mut WasomeVec<u64>, index: usize) -> u64 {
    unsafe { (*mut_vec).remove(index) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_remove_ptr(
    mut_vec: *mut WasomeVec<*mut WasomeComposite>,
    index: usize,
) -> *mut WasomeComposite {
    unsafe { (*mut_vec).remove(index) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_get_1(mut_vec: *mut WasomeVec<u8>, index: usize) -> u8 {
    unsafe { (*mut_vec).get(index) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_get_2(mut_vec: *mut WasomeVec<u16>, index: usize) -> u16 {
    unsafe { (*mut_vec).get(index) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_get_4(mut_vec: *mut WasomeVec<u32>, index: usize) -> u32 {
    unsafe { (*mut_vec).get(index) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_get_8(mut_vec: *mut WasomeVec<u64>, index: usize) -> u64 {
    unsafe { (*mut_vec).get(index) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_get_ptr(
    mut_vec: *mut WasomeVec<*mut WasomeComposite>,
    index: usize,
) -> *mut WasomeComposite {
    let val = unsafe { (*mut_vec).get(index) };
    WasomeComposite::inc_rc(val);
    val
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_set_1(mut_vec: *mut WasomeVec<u8>, index: usize, new: u8) -> u8 {
    unsafe { (*mut_vec).set(index, new) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_set_2(mut_vec: *mut WasomeVec<u16>, index: usize, new: u16) -> u16 {
    unsafe { (*mut_vec).set(index, new) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_set_4(mut_vec: *mut WasomeVec<u32>, index: usize, new: u32) -> u32 {
    unsafe { (*mut_vec).set(index, new) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_set_8(mut_vec: *mut WasomeVec<u64>, index: usize, new: u64) -> u64 {
    unsafe { (*mut_vec).set(index, new) }
}

#[unsafe(no_mangle)]
pub extern "C" fn wasome_vec_set_ptr(
    mut_vec: *mut WasomeVec<*mut WasomeComposite>,
    index: usize,
    new: *mut WasomeComposite,
) -> *mut WasomeComposite {
    unsafe { (*mut_vec).set(index, new) }
}

impl<T: Copy> Drop for WasomeVec<T> {
    fn drop(&mut self) {
        // Drops the allocation
        unsafe {
            self.as_vec();
        }
    }
}