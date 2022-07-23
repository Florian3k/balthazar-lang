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
    /// Builds `Chunk` from serialised JSON. Returns `Chunk` and a `Vec` of produced strings
    pub fn from_str(s: &str) -> DeserResult<(Self, Vec<(String, Value)>)> {
        let SerChunk {
            code,
            constants,
            lines,
        } = serde_json::from_str(s)?;
        let mut strings = Vec::new();
        let into_constant = |sc: &SerConstant| match sc {
            SerConstant::Num(s) => Value {
                uint: s.parse().unwrap(),
            },
            SerConstant::Str { str } => {
                // no deduplication - we assume that bytecode is correct, i.e. every constant string is unique
                // that assumption probably won't hold when we introduce functions
                let v = allocate_obj(ObjInner::String(ObjString(str.clone())));
                strings.push((str.clone(), v));
                v
            }
        };
        let c = Self {
            code: code.iter().map(Into::into).collect(),
            constants: constants
                .iter()
                // TODO: graceful error handling
                .map(into_constant)
                .collect(),
            lines: lines.iter().map(|n| *n as usize).collect(),
        };
        Ok((c, strings))
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

/// Helper struct for deserialization
#[derive(Serialize, Deserialize)]
struct SerChunk {
    code: Vec<SerOp>,
    // strings should encode 64-bit integers
    constants: Vec<SerConstant>,
    // u32 to ensure it can be encoded in JS number
    lines: Vec<u32>,
}
