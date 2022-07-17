
use std::error::Error;
use serde::{Serialize, Deserialize};

#[repr(u8)]
#[derive(Debug, Eq, PartialEq, Serialize, Deserialize, Copy, Clone)]
pub enum Op {
    
    OpRet     = 0,
    OpConst   = 1,

    OpAddI64  = 2,
    OpSubI64  = 3,
    OpMulI64  = 4,
    OpDivI64  = 5,

    OpAddF64  = 6,
    OpSubF64  = 7,
    OpMulF64  = 8,
    OpDivF64  = 9,

    OpPrint   = 10,

    OpConcat  = 11,
    OpPrintStr = 12,
}
use Op::*;

impl TryFrom<u8> for Op {
    type Error = Box<dyn Error>;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(
            match value {
                0  => OpRet,
                1  => OpConst,
                2  => OpAddI64,
                3  => OpSubI64,
                4  => OpMulI64,
                5  => OpDivI64,
                6  => OpAddF64,
                7  => OpSubF64,
                8  => OpMulF64,
                9  => OpDivF64,
                10 => OpPrint,
                11 => OpConcat,
                12 => OpPrintStr,
                x  => return Err(format!("Unknown opcode: {}", x).into())
            }
        )
    }
}