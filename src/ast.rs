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
}

#[derive(PartialEq, Debug)]
pub struct Identifier {
    pub s: String,
}
