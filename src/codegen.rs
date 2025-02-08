use std::fmt::format;

use crate::asm::{Function, Instruction, Operand, Program, UnaryOperator};

#[derive(Debug)]
pub struct CodegenError {
    s: String,
}

pub fn generate(p: Program) -> Result<String, CodegenError> {
    generate_program(p)
}

fn generate_program(p: Program) -> Result<String, CodegenError> {
    let Program::Program(f) = p;

    let code = format!(
        "{}\n\t.section\t.note.GNU-stack,\"\",@progbits\n",
        generate_function(f)?
    );

    Ok(code)
}

fn generate_function(f: Function) -> Result<String, CodegenError> {
    let Function::Function {
        identifier: id,
        instructions: insts,
    } = f;

    let code = format!(
        "\t.global {name}\n{name}:\n{push}\n{insts}",
        name = id.s,
        push = "\tpushq\t%rbp\n\tmovq\t%rsp, %rbp",
        insts = generate_instruction(insts)?
    );

    Ok(code)
}

fn generate_instruction(insts: Vec<Instruction>) -> Result<String, CodegenError> {
    let mut code = String::new();

    for inst in insts {
        let op = match inst {
            Instruction::Mov { src, dst } => format!(
                "\tmovl\t{src}, {dst}\n",
                src = generate_operand(src)?,
                dst = generate_operand(dst)?
            ),
            Instruction::Ret => format!("\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret\n"),
            Instruction::Unary(uop, o) => {
                format!(
                    "\t{operator}\t{operand}\n",
                    operator = generate_uop(uop)?,
                    operand = generate_operand(o)?
                )
            }
            Instruction::AllocateStack(n) => format!("\tsubq\t${}, %rsp\n", n),
        };

        code += &op;
    }

    Ok(code)
}

fn generate_uop(op: UnaryOperator) -> Result<String, CodegenError> {
    match op {
        UnaryOperator::Neg => Ok("negl".to_string()),
        UnaryOperator::Not => Ok("notl".to_string()),
    }
}

fn generate_operand(o: Operand) -> Result<String, CodegenError> {
    match o {
        Operand::Immediate(n) => Ok(format!("${}", n)),
        Operand::Reg(r) => {
            let r = match r {
                crate::asm::Register::AX => "%eax".to_string(),
                crate::asm::Register::R10 => "%r10d".to_string(),
            };

            Ok(format!("{}", r))
        }
        Operand::Pseudo(_identifier) => todo!(),
        Operand::Stack(n) => Ok(format!("{}(%rbp)", n)),
    }
}

#[cfg(test)]
mod tests {
    use crate::{asm, codegen::generate, parse, tacky, token};

    #[test]
    fn valid_asm() {
        let mut result = token::tokenize(" int main(void) { return 100; } ".into()).unwrap();
        let result = parse::parse(&mut result).unwrap();
        let result = tacky::convert(result).unwrap();
        let result = asm::convert(result).unwrap();
        let result = generate(result).unwrap();

        assert_eq!(result, "");
    }
}
