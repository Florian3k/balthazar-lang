mod bytecode;
mod chunk;
mod gc;
mod value;

use bytecode::Op::{self, *};
use gc::allocate_obj;
use value::{ObjInner, Value};

use chunk::Chunk;
use std::collections::HashMap;
use std::fs::read_to_string;
use std::{error::Error, io};

const STACK_SIZE: usize = 8196;
struct VM {
    chunk: Chunk,
    stack: [Value; STACK_SIZE],
    sp: usize,
    ip: usize,
    strings: HashMap<String, Value>,
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

impl VM {
    pub fn new(chunk: Chunk, string_vec: Vec<(String, Value)>) -> Self {
        Self {
            chunk,
            stack: [Value { int: 0 }; STACK_SIZE],
            sp: 0,
            ip: 0,
            strings: string_vec.into_iter().collect(),
        }
    }

    fn push_val(&mut self, v: Value) {
        self.stack[self.sp] = v;
        self.sp += 1;
    }

    fn pop_val(&mut self) -> Value {
        self.sp -= 1;
        self.stack[self.sp]
    }

    /// Consumes u8 from under `ip`
    fn read_u8(&mut self) -> u8 {
        let res: u8 = self.chunk.code[self.ip] as u8;
        self.ip += 1;
        res
    }

    /// Consumes litle-endian u16 from under `ip`
    fn read_u16(&mut self) -> u16 {
        let res: u16 =
            ((self.chunk.code[self.ip + 1] as u16) << 8) | self.chunk.code[self.ip] as u16;
        self.ip += 2;
        res
    }

    /// Consumes litle-endian i16 from under `ip`
    fn read_i16(&mut self) -> i16 {
        self.read_u16() as i16
    }

    /// Allocates or returns already allocated `Obj` containing given String
    /// All `ObjString`s should be allocated using this function
    fn allocate_string(&mut self, s: String) -> Value {
        let obj = self
            .strings
            .entry(s.clone())
            .or_insert_with(|| allocate_obj(ObjInner::String(s.into())));
        *obj
    }

    fn jump(&mut self, offset: i16) {
        self.ip = ((self.ip as i64) + (offset as i64)) as usize;
    }

    pub fn run(&mut self) -> Result<(), Box<dyn Error>> {
        loop {
            let op: Op = self.chunk.code[self.ip].try_into()?;
            self.ip += 1;
            match op {
                OpRet => break,
                OpConst => {
                    let const_index = self.read_u16();
                    let v = self.chunk.constants[const_index as usize];
                    self.push_val(v);
                }
                OpNull => self.push_val(Value { uint: 0 }),

                OpPop => {
                    self.pop_val();
                }
                OpLoad => {
                    let idx = self.read_u8();
                    self.push_val(self.stack[self.sp - idx as usize]);
                }
                OpStore => {
                    let idx = self.read_u8();
                    let val = self.pop_val();
                    self.stack[self.sp + 1 - idx as usize] = val;
                }

                OpAddI64 => arith_arm!(self, int, +),
                OpSubI64 => arith_arm!(self, int, -),
                OpMulI64 => arith_arm!(self, int, *),
                OpDivI64 => arith_arm!(self, int, /),

                OpLeqI64 => cmp_arm!(self, int, <=),
                OpLtI64 => cmp_arm!(self, int, <),
                OpGeqI64 => cmp_arm!(self, int, >=),
                OpGtI64 => cmp_arm!(self, int, >),

                OpNegI64 => {
                    let rhs = self.pop_val().as_int();
                    self.push_val(Value { int: -rhs })
                }

                OpAddF64 => arith_arm!(self, float, +),
                OpSubF64 => arith_arm!(self, float, -),
                OpMulF64 => arith_arm!(self, float, *),
                OpDivF64 => arith_arm!(self, float, /),

                OpLeqF64 => cmp_arm!(self, float, <=),
                OpLtF64 => cmp_arm!(self, float, <),
                OpGeqF64 => cmp_arm!(self, float, >=),
                OpGtF64 => cmp_arm!(self, float, >),

                OpJump => {
                    let offset = self.read_i16();
                    self.jump(offset);
                }
                OpJumpTrue => {
                    let offset = self.read_i16();
                    if self.pop_val().as_uint() != 0 {
                        self.jump(offset);
                    }
                }
                OpJumpFalse => {
                    let offset = self.read_i16();
                    if self.pop_val().as_uint() == 0 {
                        self.jump(offset);
                    }
                }

                OpEq => {
                    let rhs = self.pop_val().as_uint();
                    let lhs = self.pop_val().as_uint();

                    // naive equality works for ints (obviously), almost works for floats
                    // (not IEEE compliant but w/e) and will work for strings, when we starn interning them
                    let res = rhs == lhs;
                    self.push_val(Value {
                        uint: u64::from(res),
                    })
                }

                OpPrint => {
                    let v = self.pop_val();
                    println!("{:?}", v);
                }
                OpConcat => {
                    let rhs = unsafe { self.pop_val().as_obj_string_ref() };
                    let lhs = unsafe { self.pop_val().as_obj_string_ref() };
                    let mut res_str = lhs.0.clone();
                    res_str.push_str(&rhs.0);
                    let res = self.allocate_string(res_str);
                    self.push_val(res);
                }
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
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Expected 1 argument");
        return Ok(());
    }

    let s = read_to_string(args[1].to_string()).unwrap();
    let (c, sv) = Chunk::from_str(&s).unwrap();
    // dbg!(&c);
    let mut vm = VM::new(c, sv);
    vm.run().unwrap();
    unsafe { gc::free_all_objs() };

    Ok(())
}
