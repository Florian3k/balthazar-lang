use std::fmt::Debug;

use crate::gc::GcObj;

pub struct ObjHeader {
    pub next: Option<GcObj>,
}

pub struct ObjString(pub String);

impl From<String> for ObjString {
    fn from(s: String) -> Self {
        ObjString(s)
    }
}
pub enum ObjInner {
    String(ObjString),
}

pub struct Obj {
    pub header: ObjHeader,
    pub inner: ObjInner,
}

impl Obj {}

#[derive(Copy, Clone)]
pub union Value {
    pub float: f64,
    pub int: i64,
    pub uint: u64,
    pub obj: GcObj,
}

#[allow(dead_code)]
impl Value {
    // casts to number types are safe
    pub fn as_int(self) -> i64 {
        unsafe { self.int }
    }
    pub fn as_uint(self) -> u64 {
        unsafe { self.uint }
    }
    pub fn as_float(self) -> f64 {
        unsafe { self.float }
    }
    pub unsafe fn as_obj(self) -> GcObj {
        self.obj
    }

    /// # Safety
    /// reference only usable while GC is alive, no idea how to encode it properly
    pub unsafe fn as_obj_string_ref<'a>(self) -> &'a ObjString {
        let obj: &Obj = self.obj.0.as_ref();
        match &obj.inner {
            ObjInner::String(s) => s,

            #[allow(unreachable_patterns)]
            _ => panic!("bad unsafe as_obj_* coersion"),
        }
    }
}

impl Debug for Value {
    // for now just print hexadecimal value
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:x}", self.as_uint())
    }
}
