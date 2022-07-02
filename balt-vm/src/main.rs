#[repr(u8)]
#[derive(Debug, Eq, PartialEq)]
enum Opcode {
  OpPush8 = 1,
  // OpPush32 = 2,
  OpAddInt8 = 3,
  // OpAddInt32 = 4,
}

use Opcode::*;

struct VM {
  consts: Vec<u8>,
  code: Vec<u8>,
  stack: [u8; 8192],
  sp: usize,
  // bp: usize,
  ip: usize,
}

impl TryFrom<u8> for Opcode {
  type Error = ();
  fn try_from(value: u8) -> Result<Self, ()> {
    match value {
      1 => Ok(OpPush8),
      // 2 => Ok(OpPush32),
      3 => Ok(OpAddInt8),
      // 4 => Ok(OpAddInt32),
      _ => Err(()),
    }
  }
}

impl VM {
  fn new(code: Vec<u8>, consts: Vec<u8>) -> VM {
    VM {
      consts,
      code,
      stack: [0].repeat(8192).try_into().unwrap(),
      sp: 0,
      // bp: 0,
      ip: 0,
    }
  }

  fn debug(self: &VM) {
    println!();
    println!("--- VM debug start");
    println!(" Constants ({}): ", self.consts.len());
    for (idx, val) in self.consts.iter().enumerate() {
      println!("  {} | {}", idx, val);
    }
    println!(" Code ({}): ", self.code.len());
    let mut i = 0;
    while i < self.code.len() {
      let op = Opcode::try_from(self.code[i]).unwrap();
      print!("  {} | op  {:?}", i, op);
      if i == self.ip {
        println!(" <-- ip");
      } else {
        println!("");
      }
      if op == OpPush8 {
        i += 1;
        println!("  {} | val {}", i, self.code[i]);
      }
      i += 1;
    }
    println!(" Stack ({}): ", self.sp);
    for i in 0..self.sp {
      println!("  {} | {}", i, self.stack[i])
    }
    println!("--- VM debug end ---");
    println!();
  }

  fn push(self: &mut VM, byte: u8) {
    self.stack[self.sp] = byte;
    self.sp += 1;
  }

  fn pop(self: &mut VM) -> u8 {
    self.sp -= 1;
    self.stack[self.sp]
  }

  fn exec(self: &mut VM) {
    let code: u8 = self.code[self.ip];
    self.ip += 1;
    match Opcode::try_from(code).ok() {
      Some(op) => match op {
        OpPush8 => {
          println!("Executing {:?}", op);
          self.push(self.consts[self.code[self.ip] as usize]);
          self.ip += 1;
        }
        // OpPush32 => panic!("TODO OpPush32"),
        OpAddInt8 => {
          println!("Executing {:?}", op);
          let val = self.pop();
          self.stack[self.sp - 1] += val;
        }
        // OpAddInt32 => panic!("TODO OpAddInt32"),
      },
      None => panic!("Error: unexpected opcode {}", code),
    }
  }
}

fn main() {
  let consts: Vec<u8> = vec![2, 5];
  /*
  coś
  coś
  coś
  exit
  
  coś
  coś
  ret
  
  */
  let code: Vec<u8> = vec![OpPush8 as u8, 0, OpPush8 as u8, 1, OpAddInt8 as u8];
  let mut vm: VM = VM::new(code, consts);

  vm.debug();
  vm.exec();
  vm.debug();
  vm.exec();
  vm.debug();
  vm.exec();
  vm.debug();
  println!("Hello, world!");
}
