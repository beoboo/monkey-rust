use std::collections::HashMap;
use num_derive::FromPrimitive;

pub type Byte = u8;

pub type Instructions = Vec<Byte>;

#[repr(u8)]
#[derive(Debug, Clone, Copy, FromPrimitive, PartialEq)]
pub enum OpCode {
    OpConstant = 0,
    OpPop,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpTrue,
    OpFalse,
    OpEqual,
    OpNotEqual,
    OpGreaterThan,
    OpMinus,
    OpBang,
    OpJump,
    OpJumpNotTruthy,
    OpNull,
    OpGetGlobal,
    OpSetGlobal,

    Invalid = 255,
}

pub struct Definition {
    pub name: String,
    pub operand_widths: Vec<u8>,
}

impl Definition {
    pub fn new(name: String, operand_widths: Vec<u8>) -> Self {
        Self{name, operand_widths}
    }
}

pub struct OpCodes {
    definitions: HashMap<Byte, Definition>,
}

impl OpCodes {
    pub fn new() -> Self {
        let definitions: HashMap<Byte, Definition> = vec![
            (OpCode::OpConstant as Byte, Definition::new("OpConstant".into(), vec![2])),
            (OpCode::OpPop as Byte, Definition::new("OpPop".into(), vec![])),
            (OpCode::OpAdd as Byte, Definition::new("OpAdd".into(), vec![])),
            (OpCode::OpSub as Byte, Definition::new("OpSub".into(), vec![])),
            (OpCode::OpMul as Byte, Definition::new("OpMul".into(), vec![])),
            (OpCode::OpDiv as Byte, Definition::new("OpDiv".into(), vec![])),
            (OpCode::OpTrue as Byte, Definition::new("OpTrue".into(), vec![])),
            (OpCode::OpFalse as Byte, Definition::new("OpFalse".into(), vec![])),
            (OpCode::OpEqual as Byte, Definition::new("OpEqual".into(), vec![])),
            (OpCode::OpNotEqual as Byte, Definition::new("OpNotEqual".into(), vec![])),
            (OpCode::OpGreaterThan as Byte, Definition::new("OpGreaterThan".into(), vec![])),
            (OpCode::OpMinus as Byte, Definition::new("OpMinus".into(), vec![])),
            (OpCode::OpBang as Byte, Definition::new("OpBang".into(), vec![])),
            (OpCode::OpJump as Byte, Definition::new("OpJump".into(), vec![2])),
            (OpCode::OpJumpNotTruthy as Byte, Definition::new("OpJumpNotTruthy".into(), vec![2])),
            (OpCode::OpNull as Byte, Definition::new("OpNull".into(), vec![])),
            (OpCode::OpGetGlobal as Byte, Definition::new("OpGetGlobal".into(), vec![2])),
            (OpCode::OpSetGlobal as Byte, Definition::new("OpSetGlobal".into(), vec![2])),
        ].into_iter().collect();

        Self {
            definitions
        }
    }

    pub fn get(&self, op_code: &Byte) -> Option<&Definition> {
        self.definitions.get(op_code)
    }
}