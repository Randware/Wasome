use alloc::boxed::Box;

#[repr(C)]
pub struct WasomeOptionNone {
    refc: u32,
    tag: u32,
}

impl WasomeOptionNone {
    pub fn new() -> Self {
        WasomeOptionNone { refc: 1, tag: 0 }
    }
}

impl Default for WasomeOptionNone {
    fn default() -> Self {
        Self::new()
    }
}

#[repr(C)]
pub struct WasomeOptionSome<T> {
    refc: u32,
    tag: u32,
    val: T,
}

impl<T> WasomeOptionSome<T> {
    pub fn new(val: T) -> Self {
        WasomeOptionSome {
            refc: 1,
            tag: 1,
            val,
        }
    }

    pub fn val(&self) -> &T {
        &self.val
    }
}

impl<T> From<T> for WasomeOptionSome<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

#[repr(C)]
pub union WasomeOption<T> {
    none: *mut WasomeOptionNone,
    some: *mut WasomeOptionSome<T>,
}

impl<T> From<Option<T>> for WasomeOption<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            None => Self {
                none: Box::into_raw(WasomeOptionNone::default().into()),
            },
            Some(val) => Self {
                some: Box::into_raw(WasomeOptionSome::from(val).into()),
            },
        }
    }
}
