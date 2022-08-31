
#[derive(Debug, PartialEq, Clone)]
pub enum AST {
    Id(Id),
    Prim(Prim),
    ConstantTerm(Constant),
    If(Box<AST>, Box<AST>, Box<AST>),
    Let(Vec<Def>, Box<AST>),
    Map(Vec<Id>, Box<AST>),
    App(Box<AST>, Vec<AST>),
    UnopApp(Unop, Box<AST>),
    BinopApp(Binop, Box<AST>, Box<AST>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    Empty,
    Int(i64),
    Bool(bool),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Unop {
    Plus, Minus, 
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Binop {
    Add, Sub, Mul, Div,
    Eq, Neq,
    Lt, Gt, Le, Ge,
    And, Or,
}

#[derive(Debug, Copy, PartialEq, Clone)]
pub enum Prim {
    NumberQ, FunctionQ, ListQ, EmptyQ, ConsQ,
    Cons, First, Rest,
    Arity,
}

/**
Definitions

Def ::= Id := AST ;
**/

#[derive(Debug, PartialEq, Clone)]
pub struct Def {
    pub id: Id,
    pub ast: AST,
}

/**
Identifiers
**/

#[derive(Debug, PartialEq, Clone)]
pub enum Id {
    Id(String),
}
