mod bytecode;
mod chunk;
mod value;
mod gc;

use gc::allocate_obj;
use value::{Value, ObjInner, ObjString};
use bytecode::Op::{self, *};

use std::{io, error::Error};
use chunk::Chunk;
use std::fs::{read_to_string};


const STACK_SIZE: usize = 8196;
struct VM<'gc> {
    chunk: Chunk<'gc>,
    stack: [Value<'gc>; STACK_SIZE],
    sp: usize,
    ip: usize,
}

/// Generates Op arm for arithmetic expressions
/// Takes `self`, `Value` variant and arithmetic operator
macro_rules! arith_arm {
    ($s:ident, $variant:ident, $operator:tt) => {
        {
            // coersing Value variants is safe for now
            unsafe {
                let rhs = $s.pop_val().$variant;
                let lhs = $s.pop_val().$variant;
                $s.push_val($crate::value::Value{$variant: lhs $operator rhs})
            }
        }
    };
}

macro_rules! cmp_arm {
    ($self:ident, $variant:ident, $operator:tt) => {
        unsafe {
            let rhs = $self.pop_val().$variant;
            let lhs = $self.pop_val().$variant;
            let v = $crate::value::Value { uint: u64::from(lhs $operator rhs) };
            $self.push_val(v);
        }
    };
}

impl<'gc> VM<'gc> {
    pub fn new(chunk: Chunk<'gc>) -> Self {
        Self { chunk, stack: [Value {int: 0}; STACK_SIZE], sp: 0, ip: 0 }
    }

    fn push_val(&mut self, v: Value<'gc>) {
        self.stack[self.sp] = v;
        self.sp += 1;
    }

    fn pop_val(&mut self) -> Value {
        self.sp -= 1;
        self.stack[self.sp]
    }

    /// Consumes litle-endian u16 from under `ip`
    fn read_u16(&mut self) -> u16 {
        let res : u16 = ((self.chunk.code[self.ip + 1] as u16) << 8) | self.chunk.code[self.ip] as u16;
        self.ip += 2;
        res
    }

    pub fn run(&mut self) -> Result<(), Box<dyn Error>> {
        loop {
            let op: Op = self.chunk.code[self.ip].try_into()?;
            self.ip += 1;
            match op {
                OpRet   => break,
                OpConst => {
                    let const_index = self.read_u16();
                    let v = self.chunk.constants[const_index as usize];
                    self.push_val(v);
                },
                OpAddI64 => arith_arm!(self, int, +),
                OpSubI64 => arith_arm!(self, int, -),
                OpMulI64 => arith_arm!(self, int, *),
                OpDivI64 => arith_arm!(self, int, /),

                OpLeqI64 => cmp_arm!(self, int, <=),
                OpLtI64  => cmp_arm!(self, int, <),
                OpGeqI64 => cmp_arm!(self, int, >=),
                OpGtI64  => cmp_arm!(self, int, >),

                OpAddF64 => arith_arm!(self, float, +),
                OpSubF64 => arith_arm!(self, float, -),
                OpMulF64 => arith_arm!(self, float, *),
                OpDivF64 => arith_arm!(self, float, /),

                OpLeqF64 => cmp_arm!(self, float, <=),
                OpLtF64  => cmp_arm!(self, float, <),
                OpGeqF64 => cmp_arm!(self, float, >=),
                OpGtF64  => cmp_arm!(self, float, >),

                OpEq     => {
                    let rhs = self.pop_val().as_uint();
                    let lhs = self.pop_val().as_uint();

                    // naive equality works for ints (obviously), almost works for floats 
                    // (not IEEE compliant but w/e) and will work for strings, when we starn interning them
                    let res = rhs == lhs;
                    self.push_val(Value{ uint: u64::from(res) })
                }

                OpPrint => {
                    let v = self.pop_val();
                    println!("{:?}", v);
                },
                OpConcat => {
                    let rhs = unsafe { self.pop_val().as_obj_string_ref() };
                    // clone rhs string so we can borrow vm for another pop
                    let rhs_str = rhs.0.clone();
                    let lhs = unsafe { self.pop_val().as_obj_string_ref() };
                    let mut res_str = lhs.0.clone();
                    res_str.push_str(&rhs_str);
                    let res = allocate_obj(ObjInner::String(ObjString(res_str)));
                    self.push_val(res); 
                },
                OpPrintStr => {
                    let s = unsafe { self.pop_val().as_obj_string_ref() };
                    println!("{}", s.0);
                }
            }
        }
        Ok(())
    }

}


fn main() -> io::Result<()> {
    let s = read_to_string("test.json").unwrap();
    let c = Chunk::from_str(&s).unwrap();
    dbg!(&c);
    let mut vm = VM::new(c);
    vm.run().unwrap();
    unsafe { gc::free_all_objs() };
  
    Ok(())
}
