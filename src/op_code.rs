use std::collections::HashMap;

pub type Byte = u8;

pub type Instructions = Vec<Byte>;

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    OpConstant = 0,
    OpAdd,
    Invalid = 255,
}

impl OpCode {
    pub fn from_byte(src: Byte) -> Self {
        match src {
            0 => Self::OpConstant,
            1 => Self::OpAdd,
            _ => panic!("Cannot convert from {}", src)
        }
    }
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
            (OpCode::OpAdd as Byte, Definition::new("OpAdd".into(), vec![])),
        ].into_iter().collect();

        Self {
            definitions
        }
    }

    pub fn get(&self, op_code: &Byte) -> Option<&Definition> {
        self.definitions.get(op_code)
    }
}