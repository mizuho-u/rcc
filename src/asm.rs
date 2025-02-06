use crate::ast;

#[derive(PartialEq, Debug)]
pub enum Program {
    Program(Function),
}

#[derive(PartialEq, Debug)]
pub enum Function {
    Function {
        identifier: Identifier,
        instructions: Vec<Instruction>,
    },
}

#[derive(PartialEq, Debug)]
pub enum Instruction {
    Mov { src: Operand, dst: Operand },
    Ret,
}

#[derive(PartialEq, Debug)]
pub enum Operand {
    Immediate(i32),
    Register,
}

#[derive(PartialEq, Debug)]
pub struct Identifier {
    pub s: String,
}

#[derive(Debug)]
pub struct AssemblyError {
    s: String,
}

pub fn convert(p: ast::Program) -> Result<Program, AssemblyError> {
    Ok(convert_program(p)?)
}

fn convert_program(p: ast::Program) -> Result<Program, AssemblyError> {
    let ast::Program::Program(func) = p;

    Ok(Program::Program(convert_function(func)?))
}

fn convert_function(f: ast::Function) -> Result<Function, AssemblyError> {
    let ast::Function::Function(id, body) = f;

    Ok(Function::Function {
        identifier: Identifier { s: id.s },
        instructions: convert_statement(body)?,
    })
}

fn convert_statement(s: ast::Statement) -> Result<Vec<Instruction>, AssemblyError> {
    let insts = match s {
        ast::Statement::Return(expression) => {
            vec![
                Instruction::Mov {
                    src: convert_exp(expression)?,
                    dst: Operand::Register,
                },
                Instruction::Ret,
            ]
        }
    };

    Ok(insts)
}

fn convert_exp(e: ast::Expression) -> Result<Operand, AssemblyError> {
    let op = match e {
        ast::Expression::Constant(n) => Operand::Immediate(n),
        ast::Expression::Unary(unary_operator, expression) => todo!(),
    };

    Ok(op)
}

#[cfg(test)]
mod tests {
    use crate::asm::{Function, Identifier, Instruction, Operand, Program};
    use crate::parse::parse;
    use crate::token;

    use super::convert;

    #[test]
    fn valid_asm() {
        let mut result = token::tokenize(" int main(void) { return 1; } ".into()).unwrap();
        let result = parse(&mut result).unwrap();
        let result = convert(result).unwrap();

        assert_eq!(
            result,
            Program::Program(Function::Function {
                identifier: Identifier {
                    s: "main".to_string()
                },
                instructions: vec![
                    Instruction::Mov {
                        src: Operand::Immediate(1),
                        dst: Operand::Register
                    },
                    Instruction::Ret
                ]
            })
        );
    }
}
