#![no_std]
use alloc::string::String;
use core::panic::PanicInfo;
use crate::panic_internal;

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

pub fn print(s: &str) {
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

pub fn read_line_internal() -> String {
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
        // FD 0 always exists
        unsafe {
            fd_read(0, &iovec, 1, &mut nwritten);
        }
        if nwritten != 1 {
            panic!("Read failed! {nwritten}");
        }
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

pub fn exit(code: i32) {
    proc_exit(code)
}

// Prevent errors
#[cfg(not(test))]
#[panic_handler]
fn rust_panic(info: &PanicInfo) -> ! {
    panic_internal(info.message().as_str())
}
