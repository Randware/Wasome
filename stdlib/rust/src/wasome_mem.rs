use alloc::alloc::{alloc, dealloc};
use core::alloc::Layout;

// Free and malloc are also functions in libc.
// So we can't have them in the tests (as they require libc)

/// # Safety
///
/// This is UB if `to_alloc == 0`
#[cfg(not(test))]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn malloc(to_alloc: usize) -> *mut u8 {
    let alloc = unsafe {
        alloc(Layout::from_size_align(to_alloc, 8).unwrap_or_else(|_| {
            panic!(
                "Memory allocation of {} bytes may not be over 2^31-1",
                to_alloc
            )
        }))
    };
    if alloc.is_null() {
        panic!("Unable to allocate {} bytes of memory!", to_alloc);
    }
    alloc
}

/// # Safety
///
/// This is UB if either:
/// 1. `to_free` was not allocated by [`malloc`]
/// 2. `size` is different than the one passed to `malloc`
#[cfg(not(test))]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn free(to_free: *mut u8, size: usize) {
    unsafe { dealloc(to_free, Layout::from_size_align(size, 8).unwrap()) }
}
