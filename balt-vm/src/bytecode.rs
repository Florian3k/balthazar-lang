use serde::{Deserialize, Serialize};
use std::error::Error;

#[repr(u8)]
#[derive(Debug, Eq, PartialEq, Serialize, Deserialize, Copy, Clone)]
pub enum Op {
    OpRet = 0,
    OpConst = 1,

    OpAddI64 = 2,
    OpSubI64 = 3,
    OpMulI64 = 4,
    OpDivI64 = 5,

    OpLeqI64 = 6,
    OpLtI64 = 7,
    OpGeqI64 = 8,
    OpGtI64 = 9,

    OpAddF64 = 10,
    OpSubF64 = 11,
    OpMulF64 = 12,
    OpDivF64 = 13,

    OpLeqF64 = 14,
    OpLtF64 = 15,
    OpGeqF64 = 16,
    OpGtF64 = 17,

    OpPrint = 18,

    OpConcat = 19,
    OpPrintStr = 20,

    OpEq = 21,
}
use Op::*;

impl TryFrom<u8> for Op {
    type Error = Box<dyn Error>;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0 => OpRet,
            1 => OpConst,
            2 => OpAddI64,
            3 => OpSubI64,
            4 => OpMulI64,
            5 => OpDivI64,
            6 => OpLeqI64,
            7 => OpLtI64,
            8 => OpGeqI64,
            9 => OpGtI64,
            10 => OpAddF64,
            11 => OpSubF64,
            12 => OpMulF64,
            13 => OpDivF64,
            14 => OpLeqF64,
            15 => OpLtF64,
            16 => OpGeqF64,
            17 => OpGtF64,
            18 => OpPrint,
            19 => OpConcat,
            20 => OpPrintStr,
            21 => OpEq,
            x => return Err(format!("Unknown opcode: {}", x).into()),
        })
    }
}
