#[derive(PartialEq, Debug)]
pub enum Program {
    Program(Function),
}

#[derive(PartialEq, Debug)]
pub enum Function {
    Function(Identifier, Statement),
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Constant(i32),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
}

#[derive(PartialEq, Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
}

#[derive(PartialEq, Debug)]
pub enum BinaryOperator {
    Subtract,
    Add,
    Multiply,
    Divide,
    Remainder,
    And,
    Or,
    Xor,
    LeftShit,
    RightShift,
}

#[derive(PartialEq, Debug)]
pub struct Identifier {
    pub s: String,
}
