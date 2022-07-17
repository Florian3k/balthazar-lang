use std::fmt::Debug;
use std::cell::RefCell;
use std::marker::PhantomData;
use std::ptr::NonNull;

use crate::gc::GcObj;


pub struct ObjHeader {
    pub next: Option<GcObj>,
}


pub struct ObjString(pub String);
pub enum ObjInner {
    String(ObjString),
}


pub struct Obj {
    pub header: ObjHeader,
    pub inner: ObjInner,
}

impl Obj {
}


#[derive(Copy, Clone)]
pub union Value<'gc> {
    pub float: f64,
    pub int:   i64,
    pub uint:  u64,
    pub obj: GcObj,
    _marker: PhantomData<&'gc ()>,
}

#[allow(dead_code)]
impl<'gc> Value<'gc> {
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

    pub unsafe fn as_obj_string_ref(self) -> &'gc ObjString {
        let obj: &'gc Obj = self.obj.0.as_ref();
        match &obj.inner { 
            ObjInner::String(s) => s,
            _ => panic!("bad unsafe as_obj_* coersion")
        }
    }
}

impl<'gc> Debug for Value<'gc> {
    // for now just print hexadecimal value 
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:x}", self.as_uint())
    }
}