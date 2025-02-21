use crate::ast;
use std::cell::RefCell;

#[derive(PartialEq, Debug)]
pub enum Program {
    Program(Function),
}

#[derive(PartialEq, Debug)]
pub enum Function {
    Function(Identifier, Vec<Instruction>),
}

#[derive(PartialEq, Debug)]
pub enum Instruction {
    Return(Val),
    Unary(UnaryOperator, Val, Val),
    Binary(BinaryOperator, Val, Val, Val),
    Copy(Val, Val),
    Jump(Identifier),
    JumpIfZero(Val, Identifier),
    JumpIfNotZero(Val, Identifier),
    Label(Identifier),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Val {
    Constant(i32),
    Var(Identifier),
}

#[derive(PartialEq, Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

#[derive(PartialEq, Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Devide,
    Remainder,
    And,
    Or,
    Xor,
    LeftShift,
    RightShift,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub struct TackeyError(String);

impl std::fmt::Display for TackeyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for TackeyError {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl std::error::Error for TackeyError {}

pub fn convert(p: ast::Program) -> Result<Program, TackeyError> {
    convert_program(p)
}

fn convert_program(p: ast::Program) -> Result<Program, TackeyError> {
    let ast::Program::Program(f) = p;

    Ok(Program::Program(convert_function(f)?))
}

fn convert_function(f: ast::Function) -> Result<Function, TackeyError> {
    let ast::Function::Function(id, stmt) = f;

    Ok(Function::Function(
        Identifier(id.0),
        convert_statement(stmt)?,
    ))
}

fn convert_statement(s: ast::Statement) -> Result<Vec<Instruction>, TackeyError> {
    let mut instructions = Vec::new();

    match s {
        ast::Statement::Return(e) => {
            let val = convert_exp(e, &mut instructions)?;
            instructions.push(Instruction::Return(val));
        }
    }

    Ok(instructions)
}

fn convert_exp(
    e: ast::Expression,
    instructions: &mut Vec<Instruction>,
) -> Result<Val, TackeyError> {
    match e {
        ast::Expression::Constant(n) => Ok(Val::Constant(n)),
        ast::Expression::Unary(op, e) => {
            let src = convert_exp(*e, instructions)?;
            let dst = Val::Var(Identifier(make_temporary()));

            let op = match op {
                ast::UnaryOperator::Complement => UnaryOperator::Complement,
                ast::UnaryOperator::Negate => UnaryOperator::Negate,
                ast::UnaryOperator::Not => UnaryOperator::Not,
            };

            instructions.push(Instruction::Unary(op, src, dst.clone()));

            Ok(dst)
        }
        ast::Expression::Binary(op, left, right) => match op {
            ast::BinaryOperator::LogicalAnd => {
                let false_label = Identifier(make_label("false".to_string()));
                let end_label = Identifier(make_label("end".to_string()));

                let dst = Val::Var(Identifier(make_temporary()));

                let left = convert_exp(*left, instructions)?;
                instructions.push(Instruction::JumpIfZero(left, false_label.clone()));

                let right = convert_exp(*right, instructions)?;
                instructions.push(Instruction::JumpIfZero(right, false_label.clone()));

                instructions.push(Instruction::Copy(Val::Constant(1), dst.clone()));

                instructions.push(Instruction::Jump(end_label.clone()));

                instructions.push(Instruction::Label(false_label));
                instructions.push(Instruction::Copy(Val::Constant(0), dst.clone()));

                instructions.push(Instruction::Label(end_label));

                Ok(dst)
            }
            ast::BinaryOperator::LogicalOr => {
                let true_label = Identifier(make_label("true".to_string()));
                let end_label = Identifier(make_label("end".to_string()));

                let dst = Val::Var(Identifier(make_temporary()));

                let left = convert_exp(*left, instructions)?;
                instructions.push(Instruction::JumpIfNotZero(left, true_label.clone()));

                let right = convert_exp(*right, instructions)?;
                instructions.push(Instruction::JumpIfNotZero(right, true_label.clone()));

                instructions.push(Instruction::Copy(Val::Constant(0), dst.clone()));
                instructions.push(Instruction::Jump(end_label.clone()));

                instructions.push(Instruction::Label(true_label));
                instructions.push(Instruction::Copy(Val::Constant(1), dst.clone()));

                instructions.push(Instruction::Label(end_label));

                Ok(dst)
            }
            _ => {
                let left = convert_exp(*left, instructions)?;
                let right = convert_exp(*right, instructions)?;

                let dst = Val::Var(Identifier(make_temporary()));

                let op = convert_binop(&op)?;

                instructions.push(Instruction::Binary(op, left, right, dst.clone()));
                Ok(dst)
            }
        },
    }
}

fn convert_binop(op: &ast::BinaryOperator) -> Result<BinaryOperator, TackeyError> {
    let op = match op {
        ast::BinaryOperator::Subtract => BinaryOperator::Subtract,
        ast::BinaryOperator::Add => BinaryOperator::Add,
        ast::BinaryOperator::Multiply => BinaryOperator::Multiply,
        ast::BinaryOperator::Divide => BinaryOperator::Devide,
        ast::BinaryOperator::Remainder => BinaryOperator::Remainder,
        ast::BinaryOperator::And => BinaryOperator::And,
        ast::BinaryOperator::Or => BinaryOperator::Or,
        ast::BinaryOperator::Xor => BinaryOperator::Xor,
        ast::BinaryOperator::LeftShit => BinaryOperator::LeftShift,
        ast::BinaryOperator::RightShift => BinaryOperator::RightShift,
        ast::BinaryOperator::EqualTo => BinaryOperator::Equal,
        ast::BinaryOperator::NotEqualTo => BinaryOperator::NotEqual,
        ast::BinaryOperator::LessThan => BinaryOperator::LessThan,
        ast::BinaryOperator::LessOrEqual => BinaryOperator::LessOrEqual,
        ast::BinaryOperator::GreaterThan => BinaryOperator::GreaterThan,
        ast::BinaryOperator::GreaterOrEqual => BinaryOperator::GreaterOrEqual,
        _ => return Err(TackeyError(format!("cannot convert binop"))),
    };
    Ok(op)
}

fn make_temporary() -> String {
    thread_local!(
        pub static TMP_COUNT: RefCell<i32> = RefCell::new(0)
    );

    let count = TMP_COUNT.with(|c| {
        let mut c = c.borrow_mut();
        *c += 1;

        *c
    });

    format!("tmp.{}", count)
}

fn make_label(prefix: String) -> String {
    thread_local!(
        pub static LABEL_COUNT: RefCell<i32> = RefCell::new(0)
    );

    let count = LABEL_COUNT.with(|c| {
        let mut c = c.borrow_mut();
        *c += 1;

        *c
    });

    // 文字、数字、ピリオド、アンダースコアだけ使える
    format!("{}.{}", prefix, count)
}

#[cfg(test)]
mod tests {
    use crate::parse::parse;
    use crate::tacky::{
        self, convert, BinaryOperator, Identifier, Instruction, UnaryOperator, Val,
    };
    use crate::token;

    #[test]
    fn valid_tacky() {
        let mut result = token::tokenize(" int main(void) { return 1; } ".into()).unwrap();
        let result = parse(&mut result).unwrap();
        let result = convert(result).unwrap();

        assert_eq!(
            result,
            tacky::Program::Program(tacky::Function::Function(
                Identifier("main".to_string()),
                vec![tacky::Instruction::Return(tacky::Val::Constant(1))]
            ))
        )
    }

    #[test]
    fn valid_tacky_unary() {
        let mut result = token::tokenize(" int main(void) { return ~(-1); } ".into()).unwrap();
        let result = parse(&mut result).unwrap();
        let result = convert(result).unwrap();

        assert_eq!(
            result,
            tacky::Program::Program(tacky::Function::Function(
                Identifier("main".to_string()),
                vec![
                    tacky::Instruction::Unary(
                        tacky::UnaryOperator::Negate,
                        tacky::Val::Constant(1),
                        tacky::Val::Var(Identifier("tmp.1".to_string())),
                    ),
                    tacky::Instruction::Unary(
                        tacky::UnaryOperator::Complement,
                        tacky::Val::Var(Identifier("tmp.1".to_string())),
                        tacky::Val::Var(Identifier("tmp.2".to_string())),
                    ),
                    tacky::Instruction::Return(tacky::Val::Var(Identifier("tmp.2".to_string()))),
                ]
            ))
        )
    }

    #[test]
    fn valid_tacky_binary() {
        let mut result =
            token::tokenize(" int main(void) { return (1+2)*3-4/5; } ".into()).unwrap();
        let result = parse(&mut result).unwrap();
        let result = convert(result).unwrap();

        assert_eq!(
            result,
            tacky::Program::Program(tacky::Function::Function(
                Identifier("main".to_string()),
                vec![
                    tacky::Instruction::Binary(
                        tacky::BinaryOperator::Add,
                        tacky::Val::Constant(1),
                        tacky::Val::Constant(2),
                        tacky::Val::Var(Identifier("tmp.1".to_string())),
                    ),
                    tacky::Instruction::Binary(
                        tacky::BinaryOperator::Multiply,
                        tacky::Val::Var(Identifier("tmp.1".to_string())),
                        tacky::Val::Constant(3),
                        tacky::Val::Var(Identifier("tmp.2".to_string())),
                    ),
                    tacky::Instruction::Binary(
                        tacky::BinaryOperator::Devide,
                        tacky::Val::Constant(4),
                        tacky::Val::Constant(5),
                        tacky::Val::Var(Identifier("tmp.3".to_string())),
                    ),
                    tacky::Instruction::Binary(
                        tacky::BinaryOperator::Subtract,
                        tacky::Val::Var(Identifier("tmp.2".to_string())),
                        tacky::Val::Var(Identifier("tmp.3".to_string())),
                        tacky::Val::Var(Identifier("tmp.4".to_string())),
                    ),
                    tacky::Instruction::Return(tacky::Val::Var(Identifier("tmp.4".to_string()))),
                ]
            ))
        )
    }

    #[test]
    fn bitwise_binary_operator() {
        let mut result =
            token::tokenize(" int main(void) { return 1 | 2 ^ 3 & 4 << 5 >> 6; } ".into()).unwrap();
        let result = parse(&mut result).unwrap();
        let result = convert(result).unwrap();

        assert_eq!(
            result,
            tacky::Program::Program(tacky::Function::Function(
                Identifier("main".to_string()),
                vec![
                    tacky::Instruction::Binary(
                        tacky::BinaryOperator::LeftShift,
                        tacky::Val::Constant(4),
                        tacky::Val::Constant(5),
                        tacky::Val::Var(Identifier("tmp.1".to_string())),
                    ),
                    tacky::Instruction::Binary(
                        tacky::BinaryOperator::RightShift,
                        tacky::Val::Var(Identifier("tmp.1".to_string())),
                        tacky::Val::Constant(6),
                        tacky::Val::Var(Identifier("tmp.2".to_string())),
                    ),
                    tacky::Instruction::Binary(
                        tacky::BinaryOperator::And,
                        tacky::Val::Constant(3),
                        tacky::Val::Var(Identifier("tmp.2".to_string())),
                        tacky::Val::Var(Identifier("tmp.3".to_string())),
                    ),
                    tacky::Instruction::Binary(
                        tacky::BinaryOperator::Xor,
                        tacky::Val::Constant(2),
                        tacky::Val::Var(Identifier("tmp.3".to_string())),
                        tacky::Val::Var(Identifier("tmp.4".to_string())),
                    ),
                    tacky::Instruction::Binary(
                        tacky::BinaryOperator::Or,
                        tacky::Val::Constant(1),
                        tacky::Val::Var(Identifier("tmp.4".to_string())),
                        tacky::Val::Var(Identifier("tmp.5".to_string())),
                    ),
                    tacky::Instruction::Return(tacky::Val::Var(Identifier("tmp.5".to_string()))),
                ]
            ))
        )
    }

    #[test]
    fn logical_operator() {
        let mut result =
            token::tokenize(" int main(void) { return !1 && 2 || 3; } ".into()).unwrap();
        let result = parse(&mut result).unwrap();
        let result = convert(result).unwrap();

        assert_eq!(
            result,
            tacky::Program::Program(tacky::Function::Function(
                Identifier("main".to_string()),
                vec![
                    Instruction::Unary(
                        UnaryOperator::Not,
                        Val::Constant(1),
                        Val::Var(Identifier("tmp.3".to_string())),
                    ),
                    Instruction::JumpIfZero(
                        Val::Var(Identifier("tmp.3".to_string())),
                        Identifier("false.3".to_string())
                    ),
                    Instruction::JumpIfZero(Val::Constant(2), Identifier("false.3".to_string())),
                    Instruction::Copy(Val::Constant(1), Val::Var(Identifier("tmp.2".to_string()))),
                    Instruction::Jump(Identifier("end.4".to_string())),
                    Instruction::Label(Identifier("false.3".to_string())),
                    Instruction::Copy(Val::Constant(0), Val::Var(Identifier("tmp.2".to_string()))),
                    Instruction::Label(Identifier("end.4".to_string())),
                    Instruction::JumpIfNotZero(
                        Val::Var(Identifier("tmp.2".to_string())),
                        Identifier("true.1".to_string())
                    ),
                    Instruction::JumpIfNotZero(Val::Constant(3), Identifier("true.1".to_string())),
                    Instruction::Copy(Val::Constant(0), Val::Var(Identifier("tmp.1".to_string()))),
                    Instruction::Jump(Identifier("end.2".to_string())),
                    Instruction::Label(Identifier("true.1".to_string())),
                    Instruction::Copy(Val::Constant(1), Val::Var(Identifier("tmp.1".to_string()))),
                    Instruction::Label(Identifier("end.2".to_string())),
                    tacky::Instruction::Return(tacky::Val::Var(Identifier("tmp.1".to_string()))),
                ]
            ))
        )
    }

    #[test]
    fn relational_operator() {
        // (1 == 2) != ((((3 < 4) > 5) <= 6) >= 7)
        let mut result =
            token::tokenize(" int main(void) { return 1 == 2 != 3 < 4 > 5 <= 6 >= 7; } ".into())
                .unwrap();
        let result = parse(&mut result).unwrap();
        let result = convert(result).unwrap();

        assert_eq!(
            result,
            tacky::Program::Program(tacky::Function::Function(
                Identifier("main".to_string()),
                vec![
                    Instruction::Binary(
                        BinaryOperator::Equal,
                        Val::Constant(1),
                        Val::Constant(2),
                        Val::Var(Identifier("tmp.1".to_string())),
                    ),
                    Instruction::Binary(
                        BinaryOperator::LessThan,
                        Val::Constant(3),
                        Val::Constant(4),
                        Val::Var(Identifier("tmp.2".to_string())),
                    ),
                    Instruction::Binary(
                        BinaryOperator::GreaterThan,
                        Val::Var(Identifier("tmp.2".to_string())),
                        Val::Constant(5),
                        Val::Var(Identifier("tmp.3".to_string())),
                    ),
                    Instruction::Binary(
                        BinaryOperator::LessOrEqual,
                        Val::Var(Identifier("tmp.3".to_string())),
                        Val::Constant(6),
                        Val::Var(Identifier("tmp.4".to_string())),
                    ),
                    Instruction::Binary(
                        BinaryOperator::GreaterOrEqual,
                        Val::Var(Identifier("tmp.4".to_string())),
                        Val::Constant(7),
                        Val::Var(Identifier("tmp.5".to_string())),
                    ),
                    Instruction::Binary(
                        BinaryOperator::NotEqual,
                        Val::Var(Identifier("tmp.1".to_string())),
                        Val::Var(Identifier("tmp.5".to_string())),
                        Val::Var(Identifier("tmp.6".to_string())),
                    ),
                    tacky::Instruction::Return(tacky::Val::Var(Identifier("tmp.6".to_string()))),
                ]
            ))
        )
    }
}
