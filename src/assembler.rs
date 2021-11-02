use crate::op_code::{Byte, OpCode, Instructions, OpCodes};

pub struct Assembler {
    definitions: OpCodes,
}

impl Assembler {
    pub fn new(op_codes: OpCodes) -> Self {
        Self {
            definitions: op_codes
        }
    }

    pub fn assemble(&self, op_code: OpCode, operands: Vec<u32>) -> Instructions {
        let byte = op_code as Byte;
        let definition = match self.definitions.get(&byte) {
            Some(definition) => definition,
            None => return vec![]
        };

        let mut width = 1;
        for w in &definition.operand_widths {
            width += w;
        }

        let mut instructions = vec![
            byte.clone(),
        ];

        for (i, o) in operands.iter().enumerate() {
            let w = definition.operand_widths[i];

            match w {
                2 => {
                    instructions.push((*o >> 8) as Byte);
                    instructions.push(*o as Byte);
                },
                _ => panic!("Invalid operation width")
            }
        }

        instructions
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assemble() {
        struct Test {
            op: OpCode,
            operands: Vec<u32>,
            expected: Vec<u8>,
        }

        let tests = vec![
            Test {
                op: OpCode::OpConstant,
                operands: vec![65534],
                expected: vec![OpCode::OpConstant as Byte, 255, 254],
            },
            Test {
                op: OpCode::OpAdd,
                operands: vec![],
                expected: vec![OpCode::OpAdd as Byte],
            },
        ];

        for t in tests {
            let assembler = Assembler::new(OpCodes::new());
            let instruction = assembler.assemble(t.op, t.operands);

            assert_eq!(instruction.len(), t.expected.len());
            for (i, byte) in t.expected.iter().enumerate() {
                assert_eq!(instruction[i], *byte);
            }
        }
    }
}