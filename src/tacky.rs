use crate::parse::{self, AssignmentOperator, Block, BlockItem, Expression};
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
    Divide,
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

pub fn convert(p: parse::Program) -> Result<Program, TackeyError> {
    convert_program(p)
}

fn convert_program(p: parse::Program) -> Result<Program, TackeyError> {
    let parse::Program::Program(f) = p;

    Ok(Program::Program(convert_function(f)?))
}

fn convert_function(f: parse::Function) -> Result<Function, TackeyError> {
    let parse::Function::Function(id, body) = f;

    let mut insts = convert_block(body)?;

    // これはok
    // int main(void) {
    //     int a = 4;
    //     a = 0;
    // }
    //
    // これもok
    // #include <stdio.h>
    // int foo(void) {
    //     printf("I'm living on the edge, baby!");
    //     // no return statement
    // }

    // int main(void) {
    //     foo();
    //     return 0;
    // }
    //
    // らしい。
    // ので、その場合のコールスタックの復帰とかその辺の処理をするためReturnを入れる。
    //
    // @todo 最終的に全部に入れて最適化で取り除く感じにしたい
    if let Some(Instruction::Return(_)) = insts.last() {
    } else {
        insts.push(Instruction::Return(Val::Constant(0)));
    }

    Ok(Function::Function(Identifier(id.0), insts))
}

fn convert_block(b: Block) -> Result<Vec<Instruction>, TackeyError> {
    let mut insts = Vec::new();

    match b {
        Block::Block(items) => {
            for b in items {
                insts.append(&mut convert_block_item(b)?);
            }
        }
    }

    Ok(insts)
}

fn convert_block_item(b: BlockItem) -> Result<Vec<Instruction>, TackeyError> {
    match b {
        BlockItem::Statement(s) => convert_statement(s),
        BlockItem::Declaration(d) => convert_declaration(d),
    }
}

fn convert_declaration(d: parse::Declaration) -> Result<Vec<Instruction>, TackeyError> {
    let mut instructions = Vec::new();

    match d {
        parse::Declaration::Declaration(id, Some(exp)) => {
            let r = convert_exp(exp, &mut instructions)?;
            instructions.push(Instruction::Copy(r, Val::Var(Identifier(id.0))));
        }
        _ => {}
    };

    Ok(instructions)
}

fn convert_statement(s: parse::Statement) -> Result<Vec<Instruction>, TackeyError> {
    let mut instructions = Vec::new();

    match s {
        parse::Statement::Return(e) => {
            let val = convert_exp(e, &mut instructions)?;
            instructions.push(Instruction::Return(val));
        }
        parse::Statement::Expression(e) => {
            convert_exp(e, &mut instructions)?;
        }
        parse::Statement::Null => {}
        parse::Statement::If(cond, then, el) => {
            let endif_label = Identifier(make_label("endif".to_string()));
            let val = convert_exp(cond, &mut instructions)?;

            if let Some(el) = el {
                let else_label = Identifier(make_label("else".to_string()));

                instructions.push(Instruction::JumpIfZero(val, else_label.clone()));

                instructions.append(&mut convert_statement(*then)?);
                instructions.push(Instruction::Jump(endif_label.clone()));

                instructions.push(Instruction::Label(else_label));
                instructions.append(&mut convert_statement(*el)?);

                instructions.push(Instruction::Label(endif_label));
            } else {
                instructions.push(Instruction::JumpIfZero(val, endif_label.clone()));
                instructions.append(&mut convert_statement(*then)?);
                instructions.push(Instruction::Label(endif_label));
            }
        }
        parse::Statement::Goto(id) => instructions.push(Instruction::Jump(Identifier(id.0))),
        parse::Statement::Label(id, s) => {
            instructions.push(Instruction::Label(Identifier(id.0)));
            instructions.append(&mut convert_statement(*s)?);
        }
        parse::Statement::Compound(b) => instructions.append(&mut convert_block(b)?),
        parse::Statement::Break(parse::Identifier(s)) => {
            instructions.push(Instruction::Jump(Identifier(format!("break.{}", s))))
        }
        parse::Statement::Continue(parse::Identifier(s)) => {
            instructions.push(Instruction::Jump(Identifier(format!("continue.{}", s))))
        }
        parse::Statement::While(cond, body, parse::Identifier(s)) => {
            let continue_label = Identifier(format!("continue.{}", s));
            let break_label = Identifier(format!("break.{}", s));

            instructions.push(Instruction::Label(continue_label.clone()));
            let v = convert_exp(cond, &mut instructions)?;
            instructions.push(Instruction::JumpIfZero(v, break_label.clone()));
            instructions.append(&mut convert_statement(*body)?);
            instructions.push(Instruction::Jump(continue_label));
            instructions.push(Instruction::Label(break_label));
        }
        parse::Statement::DoWhile(body, cond, parse::Identifier(s)) => {
            let start_label = Identifier(format!("start.{}", s));
            let continue_label = Identifier(format!("continue.{}", s));
            let break_label = Identifier(format!("break.{}", s));

            instructions.push(Instruction::Label(start_label.clone()));
            instructions.append(&mut convert_statement(*body)?);
            instructions.push(Instruction::Label(continue_label.clone()));
            let v = convert_exp(cond, &mut instructions)?;
            instructions.push(Instruction::JumpIfNotZero(v, start_label));
            instructions.push(Instruction::Label(break_label.clone()));
        }
        parse::Statement::For(init, cond, post, body, parse::Identifier(s)) => {
            let start_label = Identifier(format!("start.{}", s));
            let continue_label = Identifier(format!("continue.{}", s));
            let break_label = Identifier(format!("break.{}", s));

            match init {
                parse::ForInit::Declaration(d) => {
                    instructions.append(&mut convert_declaration(d)?);
                }
                parse::ForInit::Expression(Some(e)) => {
                    convert_exp(e, &mut instructions)?;
                }
                _ => {}
            };

            instructions.push(Instruction::Label(start_label.clone()));

            let cond_val = if let Some(cond) = cond {
                convert_exp(cond, &mut instructions)?
            } else {
                Val::Constant(1)
            };
            instructions.push(Instruction::JumpIfZero(cond_val, break_label.clone()));

            instructions.append(&mut convert_statement(*body)?);

            instructions.push(Instruction::Label(continue_label.clone()));

            if let Some(post) = post {
                convert_exp(post, &mut instructions)?;
            }

            instructions.push(Instruction::Jump(start_label));
            instructions.push(Instruction::Label(break_label.clone()));
        }
        parse::Statement::Switch(expression, stmt, identifier) => todo!(),
        parse::Statement::Case(expression, stmt, identifier) => todo!(),
        parse::Statement::Default(stmt, identifier) => todo!(),
    }

    Ok(instructions)
}

fn convert_exp(
    e: parse::Expression,
    instructions: &mut Vec<Instruction>,
) -> Result<Val, TackeyError> {
    match e {
        parse::Expression::Constant(n) => Ok(Val::Constant(n)),
        parse::Expression::Unary(parse::UnaryOperator::IncrementPrefix, e) => {
            let src = convert_exp(*e, instructions)?;
            let dst = Val::Var(Identifier(make_temporary()));

            instructions.push(Instruction::Binary(
                BinaryOperator::Add,
                src.clone(),
                Val::Constant(1),
                dst.clone(),
            ));
            instructions.push(Instruction::Copy(dst.clone(), src.clone()));

            Ok(dst)
        }
        parse::Expression::Unary(parse::UnaryOperator::IncrementPostfix, e) => {
            let src = convert_exp(*e, instructions)?;
            let dst = Val::Var(Identifier(make_temporary()));

            instructions.push(Instruction::Copy(src.clone(), dst.clone()));
            instructions.push(Instruction::Binary(
                BinaryOperator::Add,
                src.clone(),
                Val::Constant(1),
                src.clone(),
            ));

            Ok(dst)
        }
        parse::Expression::Unary(parse::UnaryOperator::DecrementPrefix, e) => {
            let src = convert_exp(*e, instructions)?;
            let dst = Val::Var(Identifier(make_temporary()));

            instructions.push(Instruction::Binary(
                BinaryOperator::Subtract,
                src.clone(),
                Val::Constant(1),
                dst.clone(),
            ));
            instructions.push(Instruction::Copy(dst.clone(), src.clone()));

            Ok(dst)
        }
        parse::Expression::Unary(parse::UnaryOperator::DecrementPostfix, e) => {
            let src = convert_exp(*e, instructions)?;
            let dst = Val::Var(Identifier(make_temporary()));

            instructions.push(Instruction::Copy(src.clone(), dst.clone()));
            instructions.push(Instruction::Binary(
                BinaryOperator::Subtract,
                src.clone(),
                Val::Constant(1),
                src.clone(),
            ));

            Ok(dst)
        }
        parse::Expression::Unary(op, e) => {
            let src = convert_exp(*e, instructions)?;
            let dst = Val::Var(Identifier(make_temporary()));

            let op = match op {
                parse::UnaryOperator::Complement => UnaryOperator::Complement,
                parse::UnaryOperator::Negate => UnaryOperator::Negate,
                parse::UnaryOperator::Not => UnaryOperator::Not,
                _ => return Err(TackeyError("Unexpected unary operator.".to_string())),
            };

            instructions.push(Instruction::Unary(op, src, dst.clone()));

            Ok(dst)
        }
        parse::Expression::Binary(op, left, right) => match op {
            parse::BinaryOperator::LogicalAnd => {
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
            parse::BinaryOperator::LogicalOr => {
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
        parse::Expression::Var(id) => Ok(Val::Var(Identifier(id.0))),
        parse::Expression::Assignment(op, e1, e2) => {
            let v = convert_assignment(op, *e1, *e2, instructions)?;
            Ok(v)
        }
        parse::Expression::Conditional(cond, e1, e2) => {
            let dst = Val::Var(Identifier(make_temporary()));
            let end_label = Identifier(make_label("endcond".to_string()));
            let e2_label = Identifier(make_label("e2".to_string()));

            let cond = convert_exp(*cond, instructions)?;
            instructions.push(Instruction::JumpIfZero(cond, e2_label.clone()));

            let e1 = convert_exp(*e1, instructions)?;
            instructions.push(Instruction::Copy(e1, dst.clone()));
            instructions.push(Instruction::Jump(end_label.clone()));

            instructions.push(Instruction::Label(e2_label.clone()));
            let e2 = convert_exp(*e2, instructions)?;
            instructions.push(Instruction::Copy(e2, dst.clone()));

            instructions.push(Instruction::Label(end_label.clone()));

            Ok(dst)
        }
    }
}

fn convert_assignment(
    op: AssignmentOperator,
    lvalue: Expression,
    rvalue: Expression,
    instructions: &mut Vec<Instruction>,
) -> Result<Val, TackeyError> {
    let l = convert_exp(lvalue.clone(), instructions)?;

    let binop = match op {
        AssignmentOperator::Simple => {
            let r = convert_exp(rvalue, instructions)?;

            instructions.push(Instruction::Copy(r, l.clone()));

            return Ok(l);
        }
        AssignmentOperator::Addition => parse::BinaryOperator::Add,
        AssignmentOperator::Subtract => parse::BinaryOperator::Subtract,
        AssignmentOperator::Multiplication => parse::BinaryOperator::Multiply,
        AssignmentOperator::Division => parse::BinaryOperator::Divide,
        AssignmentOperator::Remainder => parse::BinaryOperator::Remainder,
        AssignmentOperator::And => parse::BinaryOperator::And,
        AssignmentOperator::Or => parse::BinaryOperator::Or,
        AssignmentOperator::Xor => parse::BinaryOperator::Xor,
        AssignmentOperator::LeftShift => parse::BinaryOperator::LeftShift,
        AssignmentOperator::RightShift => parse::BinaryOperator::RightShift,
    };

    let dst = convert_exp(
        parse::Expression::Binary(binop, Box::new(lvalue), Box::new(rvalue)),
        instructions,
    )?;

    instructions.push(Instruction::Copy(dst, l.clone()));

    Ok(l)
}

fn convert_binop(op: &parse::BinaryOperator) -> Result<BinaryOperator, TackeyError> {
    let op = match op {
        parse::BinaryOperator::Subtract => BinaryOperator::Subtract,
        parse::BinaryOperator::Add => BinaryOperator::Add,
        parse::BinaryOperator::Multiply => BinaryOperator::Multiply,
        parse::BinaryOperator::Divide => BinaryOperator::Divide,
        parse::BinaryOperator::Remainder => BinaryOperator::Remainder,
        parse::BinaryOperator::And => BinaryOperator::And,
        parse::BinaryOperator::Or => BinaryOperator::Or,
        parse::BinaryOperator::Xor => BinaryOperator::Xor,
        parse::BinaryOperator::LeftShift => BinaryOperator::LeftShift,
        parse::BinaryOperator::RightShift => BinaryOperator::RightShift,
        parse::BinaryOperator::EqualTo => BinaryOperator::Equal,
        parse::BinaryOperator::NotEqualTo => BinaryOperator::NotEqual,
        parse::BinaryOperator::LessThan => BinaryOperator::LessThan,
        parse::BinaryOperator::LessOrEqual => BinaryOperator::LessOrEqual,
        parse::BinaryOperator::GreaterThan => BinaryOperator::GreaterThan,
        parse::BinaryOperator::GreaterOrEqual => BinaryOperator::GreaterOrEqual,
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
