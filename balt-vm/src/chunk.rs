use crate::value::{ObjInner, ObjString, Value};
use crate::{bytecode::Op, gc::allocate_obj};
use serde::{Deserialize, Serialize};
use serde_json::Result as DeserResult;

#[derive(Debug)]
pub struct Chunk {
  pub code: Vec<u8>,
  pub constants: Vec<Value>,
  pub lines: Vec<usize>,
}

impl Chunk {
  pub fn from_str(s: &str) -> DeserResult<Self> {
    let SerChunk {
      code,
      constants,
      lines,
    } = serde_json::from_str(s)?;
    Ok(Self {
      code: code.iter().map(Into::into).collect(),
      constants: constants
        .iter()
        // TODO: graceful error handling
        .map(Into::into)
        .collect(),
      lines: lines.iter().map(|n| *n as usize).collect(),
    })
  }
}

/// Helper enum for deserialization
/// allows user to write either opcode names (e.g. "OpConst") or bare bytes in input JSON
#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum SerOp {
  Op(Op),
  Byte(u8),
}

impl From<&SerOp> for u8 {
  fn from(s: &SerOp) -> Self {
    match s {
      SerOp::Op(op) => *op as u8,
      SerOp::Byte(b) => *b,
    }
  }
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum SerConstant {
  Num(String),
  Str { str: String },
}

impl From<&SerConstant> for Value {
  fn from(sc: &SerConstant) -> Self {
    match sc {
      SerConstant::Num(s) => Value {
        uint: s.parse().unwrap(),
      },
      SerConstant::Str { str } => allocate_obj(ObjInner::String(ObjString(str.clone()))),
    }
  }
}

/// Helper struct for deserialization
#[derive(Serialize, Deserialize)]
struct SerChunk {
  code: Vec<SerOp>,
  // strings should encode 64-bit integers
  constants: Vec<SerConstant>,
  // u32 to ensure it can be encoded in JS number
  lines: Vec<u32>,
}
