use crate::op_code::{Instructions, Definition, Byte, OpCodes};
use crate::utils::from_u16;

pub struct Disassembler {
    definitions: OpCodes,
}

impl Disassembler {
    pub fn new(op_codes: OpCodes) -> Self {
        Self {
            definitions: op_codes
        }
    }

    pub fn disassemble(&self, instructions: Instructions) -> String {
        let mut result = String::new();
        let mut offset = 0;

        loop {
            if offset >= instructions.len() {
                break
            }

            let definition = self.lookup(&(instructions[offset]))
                .unwrap_or_else(|| panic!("Definition not found for {}", instructions[offset]));

            let (operands, read) = self.read_operands(definition, &instructions[(offset + 1)..]);
            // println!("offset: {}, read: {}", offset, read);

            result.push_str(format!("{:04} {}", offset, self.format_instruction(definition, operands)).as_str());
            offset += 1 + read;
        }

        result
    }

    fn format_instruction(&self, definition: &Definition, operands: Vec<u32>) -> String {
        let operands_width = definition.operand_widths.len();
        if operands_width != operands.len() {
            return format!("ERROR: operand length ({}) does not match definition ({})\n", operands.len(), operands_width);
        }

        match operands_width {
            0 => format!("{}\n", definition.name),
            1 => format!("{} {}\n", definition.name, operands[0]),
            _ => format!("ERROR: unhandled operand count for {}\n", definition.name)
        }
    }

    pub fn lookup(&self, op_code: &Byte) -> Option<&Definition> {
        self.definitions.get(op_code)
    }

    fn read_operands(&self, definition: &Definition, instructions: &[Byte]) -> (Vec<u32>, usize) {
        let mut operands = vec![];
        let mut offset = 0;
        for width in &definition.operand_widths {
            match width {
                2 => operands.push(from_u16(&instructions[offset..]) as u32),
                _ => panic!("Invalid width: {}", width)
            }

            offset += *width as usize;
        }

        (operands, offset)
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::assembler::Assembler;
    use crate::op_code::{OpCode, OpCodes};
    use crate::op_code::OpCode::OpConstant;

    #[test]
    fn test_string() {
        let assembler = Assembler::new(OpCodes::new());

        let instructions = vec![
            assembler.assemble(OpCode::OpAdd, vec![]),
            assembler.assemble(OpCode::OpConstant, vec![2]),
            assembler.assemble(OpCode::OpConstant, vec![65534]),
        ].concat();

        let expected = "0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65534
";

        let disassembler = Disassembler::new(OpCodes::new());

        assert_eq!(disassembler.disassemble(instructions), expected);
    }

    #[test]
    fn test_read_operands() {
        struct Test {
            op: OpCode,
            operands: Vec<u32>,
            bytes_read: usize,
        }

        let tests = vec![
            Test {
                op: OpConstant,
                operands: vec![65534],
                bytes_read: 2,
            }
        ];

        let op_codes = OpCodes::new();
        let assembler = Assembler::new(OpCodes::new());
        let disassembler = Disassembler::new(op_codes);

        for t in tests {
            let instruction = assembler.assemble(t.op, t.operands.clone());
            let definition = disassembler.lookup(&(t.op as Byte)).unwrap_or_else(|| panic!("No definition found for {:?}", t.op));

            let (operands_read, n) = disassembler.read_operands(definition, &instruction[1..]);
            assert_eq!(n, t.bytes_read);

            for (i, operand) in t.operands.iter().enumerate() {
                assert_eq!(operands_read[i], *operand);
            }
        }
    }
}