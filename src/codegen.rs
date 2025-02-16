use crate::asm::{BinaryOperator, Function, Instruction, Operand, Program, UnaryOperator};

#[derive(Debug)]
pub struct CodegenError(String);

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for CodegenError {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl std::error::Error for CodegenError {}

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
        name = id.0,
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
            Instruction::Binary(op, o1, o2) => {
                format!(
                    "\t{op}\t{o1},{o2}\n",
                    op = generate_binop(op)?,
                    o1 = generate_operand(o1)?,
                    o2 = generate_operand(o2)?,
                )
            }
            Instruction::Idiv(o) => format!("\tidivl\t{o}\n", o = generate_operand(o)?),
            Instruction::Cdq => format!("\tcdq\n"),
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

fn generate_binop(op: BinaryOperator) -> Result<String, CodegenError> {
    match op {
        BinaryOperator::Add => Ok("addl".to_string()),
        BinaryOperator::Sub => Ok("subl".to_string()),
        BinaryOperator::Mult => Ok("imull".to_string()),
        BinaryOperator::And => Ok("andl".to_string()),
        BinaryOperator::Or => Ok("orl".to_string()),
        BinaryOperator::Xor => Ok("xorl".to_string()),
        BinaryOperator::Shl => Ok("sall".to_string()),
        BinaryOperator::Shr => Ok("sarl".to_string()),
    }
}

fn generate_operand(o: Operand) -> Result<String, CodegenError> {
    match o {
        Operand::Immediate(n) => Ok(format!("${}", n)),
        Operand::Reg(r) => {
            let r = match r {
                crate::asm::Register::AX => "%eax".to_string(),
                crate::asm::Register::DX => "%edx".to_string(),
                crate::asm::Register::CX => "%ecx".to_string(),
                crate::asm::Register::R10 => "%r10d".to_string(),
                crate::asm::Register::R11 => "%r11d".to_string(),
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
    fn immediate() {
        let mut result = token::tokenize(" int main(void) { return 100; } ".into()).unwrap();
        let result = parse::parse(&mut result).unwrap();
        let result = tacky::convert(result).unwrap();
        let result = asm::convert(result).unwrap();
        let result = generate(result).unwrap();

        let expect = "\t.global main\nmain:\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp\n\tsubq\t$0, %rsp\n\tmovl\t$100, %eax\n\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret\n\n\t.section\t.note.GNU-stack,\"\",@progbits\n";

        assert_eq!(result, expect, "{result}")
    }

    #[test]
    fn binop() {
        let mut result =
            token::tokenize(" int main(void) { return 2 * (3 + 4); } ".into()).unwrap();
        let result = parse::parse(&mut result).unwrap();
        let result = tacky::convert(result).unwrap();
        let result = asm::convert(result).unwrap();
        let result = generate(result).unwrap();

        let expect = "\t.global main\nmain:\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp\n\tsubq\t$8, %rsp\n\tmovl\t$3, -4(%rbp)\n\taddl\t$4,-4(%rbp)\n\tmovl\t$2, -8(%rbp)\n\tmovl\t-8(%rbp), %r11d\n\timull\t-4(%rbp),%r11d\n\tmovl\t%r11d, -8(%rbp)\n\tmovl\t-8(%rbp), %eax\n\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret\n\n\t.section\t.note.GNU-stack,\"\",@progbits\n";

        assert_eq!(result, expect, "{result}")
    }
}
