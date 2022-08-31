use crate::ast::*;
extern crate nom;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1, take_while},
    character::complete::*,
    combinator::{map, map_res},
    error::{context},
    sequence::{pair},
    multi::*,
    IResult,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    PrimTk(Prim),
    ConstantTk(Constant),
    BinopTk(Binop),
    OtherOpTk(OtherOp),
    IdTk(Id),
    KeywordTk(Keyword),
    PunctuationTk(Punctuation),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword{
    Let, LetCC, LetRec, In,
    Map, To,
    If, Then, Else,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Punctuation{
    LParen, RParen,
    LBrace, RBrace,
    LBracket, RBracket,
    SemiColon, Comma,
    Bind
}

#[derive(Debug, PartialEq, Clone)]
pub enum OtherOp{
    Plus, Minus,
    Not
}

/**
 * Punctuation ::= '(' | ')' | '{' | '}' | '[' | ']' | ';' | ',' | ':='
 */

pub fn punctuation(input: &str) -> IResult<&str, Punctuation> {
    context(
        "punctuation",
            alt((
                map(tag("("), |_| Punctuation::LParen),
                map(tag(")"), |_| Punctuation::RParen),
                map(tag("{"), |_| Punctuation::LBrace),
                map(tag("}"), |_| Punctuation::RBrace),
                map(tag("["), |_| Punctuation::LBracket),
                map(tag("]"), |_| Punctuation::RBracket),
                map(tag(";"), |_| Punctuation::SemiColon),
                map(tag(","), |_| Punctuation::Comma),
                map(tag(":="), |_| Punctuation::Bind),
            ))
    )(input)
}

/**
 * Keyword ::= let | letcc | letrec | in | map | to | if | then | else
 */

pub fn keyword(input: &str) -> IResult<&str, Keyword> {
    context(
        "Keyword",
        alt((
                map(tag("let"), |_| Keyword::Let),
                map(tag("letcc"), |_| Keyword::LetCC),
                map(tag("letrec"), |_| Keyword::LetRec),
                map(tag("in"), |_| Keyword::In),
                map(tag("map"), |_| Keyword::Map),
                map(tag("to"), |_| Keyword::To),
                map(tag("if"), |_| Keyword::If),
                map(tag("then"), |_| Keyword::Then),
                map(tag("else"), |_| Keyword::Else),
            ))
        )(input)
}

/**
 * Prim  ::= number? | function? | list? | empty? | cons? | cons | first | rest | arity
 */

 pub fn prim(input: &str) -> IResult<&str, Prim> {
    context(
        "prim",
        alt((
                map(tag("number?"), |_| Prim::NumberQ),
                map(tag("function?"), |_| Prim::FunctionQ),
                map(tag("list?"), |_| Prim::ListQ),
                map(tag("empty?"), |_| Prim::EmptyQ),
                map(tag("cons?"), |_| Prim::ConsQ),
                map(tag("cons"), |_| Prim::Cons),
                map(tag("first"), |_| Prim::First),
                map(tag("rest"), |_| Prim::Rest),
                map(tag("arity"), |_| Prim::Arity),
            ))
    )(input)
}

/**
 * Bool ::= true | false
 */

 pub fn bool(input: &str) -> IResult<&str, Constant> {
    context(
        "bool",
        
            alt((
                map(tag("true"), |_| Constant::Bool(true)),
                map(tag("false"), |_| Constant::Bool(false)),
            )),
    )(input)
}

/**
 * Int ::= [0-9]+
 */
 pub fn int(input: &str) -> IResult<&str, Constant> {
    context(
        "int",
        map(
            map_res(
                take_while1(|c: char| c.is_digit(10)),
                |x: &str| x.parse::<i64>(),
            ),
            Constant::Int
        ),
    )(input)
}

/**
 * Binop ::= Sign | '*' | / | = | != | < | > | <= | >= | & | '|'
 */

pub fn binop(input: &str) -> IResult<&str, Binop> {
    context(
        "binop",
        alt((
                map(tag("*"), |_| Binop::Mul),
                map(tag("/"), |_| Binop::Div),
                map(tag("="), |_| Binop::Eq),
                map(tag("!="), |_| Binop::Neq),
                map(tag("<"), |_| Binop::Lt),
                map(tag(">"), |_| Binop::Gt),
                map(tag("<="), |_| Binop::Le),
                map(tag(">="), |_| Binop::Ge),
                map(tag("&"), |_| Binop::And),
                map(tag("|"), |_| Binop::Or),
            ))
    )(input)
}

/**
 * otherop ::= + | - | ~
 */
pub fn otherop(input: &str) -> IResult<&str, OtherOp> {
    context(
        "otherop",
            alt((
                map(tag("+"), |_| OtherOp::Plus),
                map(tag("-"), |_| OtherOp::Minus),
                map(tag("~"), |_| OtherOp::Not),
            ))
        )(input)
}

/**
 * Id ::= AlphaOther {AlphaOther | Digit}*
 */
 
pub fn id(input: &str) -> IResult<&str, Id> {
    context(
        "id",
        map(
            pair(
                take_while1(|c : char| c.is_alphabetic()),
                take_while(|c: char| c.is_alphanumeric() || c == '_' || c == '?'),
            ),
            |(x, y) : (&str, _)| { Id::Id(vec![x.to_string(), y.to_string()].concat()) },
        ),
    )(input)
}

/**
 * Empty ::= 'empty'
 */

pub fn empty(input: &str) -> IResult<&str, Constant> {
    context(
        "empty",
        map(tag("empty"), |_| Constant::Empty),
    )(input)
}

/**
 * Token ::= Punctuation | KeyWord | Prim | Empty | Bool | Int | Binop | Unop | Id
 */

pub fn token(input: &str) -> IResult<&str, Token> {
    context(
        "token",
            alt((
                map(punctuation, Token::PunctuationTk),
                map(keyword, Token::KeywordTk),
                map(prim, Token::PrimTk),
                map(empty, Token::ConstantTk),
                map(bool, Token::ConstantTk),
                map(int, Token::ConstantTk),
                map(binop, Token::BinopTk),
                map(otherop, Token::OtherOpTk),
                map(id, Token::IdTk),
            ))
        )(input)
}

/**
 * The tokenizer creates a stream of tokens from a string
 * by ignoring whitespace and separating tokens by whitespace.
 */


pub fn tokenizer(input: &str) -> IResult<&str, Vec<Token>> {
    context(
        "tokenizer",
        map(
            many0(
                alt((
                    map(token, Some),
                    map(multispace1, |_| None),
                ))
            )
            , |x| x.into_iter().filter(|x| x.is_some()).map(|x| x.unwrap()).collect()
        )
    )(input)
}

