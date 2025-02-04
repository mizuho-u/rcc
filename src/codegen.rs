use crate::asm::{Function, Instruction, Operand, Program};

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
        "\t.global {name}\n{name}:\n{insts}",
        name = id.s,
        insts = generate_instruction(insts)?
    );

    Ok(code)
}

fn generate_instruction(insts: Vec<Instruction>) -> Result<String, CodegenError> {
    let mut code = String::new();

    for inst in insts {
        let op = match inst {
            Instruction::Mov { src, dst } => format!(
                "\tmovl {src}, {dst}\n",
                src = generate_operand(src)?,
                dst = generate_operand(dst)?
            ),
            Instruction::Ret => format!("ret\n"),
        };

        code += &op;
    }

    Ok(code)
}

fn generate_operand(o: Operand) -> Result<String, CodegenError> {
    match o {
        Operand::Immediate(n) => Ok(format!("${}", n)),
        Operand::Register => Ok(format!("{}", "%eax")),
    }
}

#[cfg(test)]
mod tests {
    use crate::{asm, codegen::generate, parse, token};

    #[test]
    fn valid_asm() {
        let mut result = token::tokenize(" int main(void) { return 1; } ".into()).unwrap();
        let result = parse::parse(&mut result).unwrap();
        let result = asm::convert(result).unwrap();
        let result = generate(result).unwrap();

        assert_eq!(result, "");
    }
}
