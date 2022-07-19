use crate::value::{Obj, ObjHeader, ObjInner, Value};
use std::{
    cell::RefCell,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

#[derive(Default)]
struct GcState {
    head: Option<GcObj>,
}

#[derive(Clone, Copy)]
pub struct GcObj(pub NonNull<Obj>);

impl Deref for GcObj {
    type Target = Obj;

    fn deref(&self) -> &Self::Target {
        // this should be safe, because only live GcObjs should be accessible
        unsafe { &*self.0.as_ptr() }
    }
}

impl DerefMut for GcObj {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // this should be safe, because only live GcObjs should be accessible
        unsafe { &mut *self.0.as_ptr() }
    }
}

thread_local! { static GC_STATE: RefCell<GcState> = Default::default() }

pub fn allocate_obj(inner: ObjInner) -> Value {
    GC_STATE.with(|gc| {
        let mut gc = gc.borrow_mut();
        let prev = gc.head;
        let res = Box::new(Obj {
            header: ObjHeader { next: prev },
            inner,
        });
        let res = unsafe { NonNull::new_unchecked(Box::into_raw(res)) };
        gc.head = Some(GcObj(res));
        Value { obj: GcObj(res) }
    })
}

pub unsafe fn free_all_objs() {
    GC_STATE.with(|gc| {
        let mut next = gc.borrow().head;
        let mut count = 0;
        while let Some(p) = next {
            next = p.header.next;
            count += 1;
            let _ = Box::from_raw(p.0.as_ptr());
        }
    })
}
