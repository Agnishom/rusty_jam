use crate::ast::*;
use crate::lexer::*;
extern crate nom;

use nom::*;



/**
 * IdList      ::= { PropIdList }
 * PropIdList  ::= Id { , Id}*
 */

pub fn idlist(input: Tokens) -> IResult<Tokens, Vec<Id>> {
    error::context(
        "idlist",
        multi::separated_list0(
            tokensat(& |tk| match tk {
                Token::PunctuationTk(Punctuation::Comma) => true,
                _ => false,
            }),
            combinator::map(tokensat(& |tk| match tk {
                    Token::IdTk(_) => true,
                    _ => false,
                }),
                & |tk| match tk {
                    Token::IdTk(id) => id,
                    _ => panic!("Expected Id"),
                }
            )
        ),
    )(input)
}

/**
 * Term        ::= Unop Term
 *               | Factor { ( ExpList ) }
 *               | Null
 *               | Int
 *               | Bool
 */

pub fn term(input: Tokens) -> IResult<Tokens, AST> {
    error::context(
     "term",
     branch::alt((
        unopterm,
        factorapp,
        combinator::map(
            tokensat(& |tk| match tk {
                Token::ConstantTk(_) => true,
                _   => false,
                }),
            |tk| match tk {
                Token::ConstantTk(c) => AST::ConstantTerm(c),
                _ => panic!("Expected Constant"),
            }
        ),

     ))
    )(input)
}

pub fn unopterm(input: Tokens) -> IResult<Tokens, AST>{
    error::context(
        "Unop Term",
        combinator::map(
            sequence::tuple((
                tokensat(& |tk| match tk {
                    Token::OtherOpTk(_) => true,
                    _ => false,
                }),
                term
            )),
            |(tk, term)| match tk {
                Token::OtherOpTk(OtherOp::Plus) => AST::UnopApp(Unop::Plus, Box::new(term)),
                Token::OtherOpTk(OtherOp::Minus) => AST::UnopApp(Unop::Minus, Box::new(term)),
                Token::OtherOpTk(OtherOp::Not) => AST::UnopApp(Unop::Not, Box::new(term)),
                _ => panic!("Expected Unop"),
            }
        )
    )(input)
}

/**
  * factorapp ::= factor { ( expList ) }
  */
pub fn factorapp(input: Tokens) -> IResult<Tokens, AST> {
    let factor = factor(input);
    match factor{
        Err(e) => Err(e),
        Ok((rest, factor)) => {
            let factorapp = combinator::map(
                sequence::tuple((
                    tokensat(& |tk| match tk {
                        Token::PunctuationTk(Punctuation::LParen) => true,
                        _ => false,
                    }),
                    explist,
                    tokensat(& |tk| match tk {
                        Token::PunctuationTk(Punctuation::RParen) => true,
                        _ => false,
                    }),
                )),
                |(_, explist, _)| AST::App(Box::new(factor.clone()), explist)
            )(rest.clone());
            match factorapp {
                Err(_) => Ok((rest, factor)),
                Ok((rest, factorapp)) => Ok((rest, factorapp)),
            }
        }
    }
}

/** 
 * factor ::= ( exp ) | prim | id
 */

pub fn factor(input: Tokens) -> IResult<Tokens, AST> {
    error::context(
        "factor",
        branch::alt((
            combinator::map(
                sequence::tuple((
                    tokensat(& |tk| match tk {
                        Token::PunctuationTk(Punctuation::LParen) => true,
                        _ => false,
                    }),
                    exp,
                    tokensat(& |tk| match tk {
                        Token::PunctuationTk(Punctuation::RParen) => true,
                        _ => false,
                    }),
                )),
                |(_, exp, _)| exp
            ),
            combinator::map(
                tokensat(& |tk| match tk {
                    Token::IdTk(_) => true,
                    _ => false,
                }),
                |tk| match tk {
                    Token::IdTk(id) => AST::Id(id),
                    _ => panic!("Expected Id"),
                }
            ),
            combinator::map(
                tokensat(& |tk| match tk {
                    Token::PrimTk(_) => true,
                    _ => false,
                }),
                |tk| match tk {
                    Token::PrimTk(prim) => AST::Prim(prim),
                    _ => panic!("Expected Prim"),
                }
            ),
        ))
    )(input)
}

/**
 * ExpList     ::= { PropExpList }
 * PropExpList ::= Exp { , Exp }*
 */

pub fn explist(input: Tokens) -> IResult<Tokens, Vec<AST>> {
    error::context(
        "ExpList",
        multi::separated_list0(
            tokensat(& |tk| match tk {
                Token::PunctuationTk(Punctuation::Comma) => true,
                _ => false,
            }),
            exp
        ),
    )(input)
}

/**
 * Exp      ::= Term { Binop Exp }
 *            | if Exp then Exp else Exp
 *            | let Def+ in Exp
 *            | map IdList to Exp
 */

pub fn exp(input: Tokens) -> IResult<Tokens, AST> {
    error::context(
        "Exp",
        branch::alt((
            exp1,
            ifexp,
            letexp,
            mapexp,
        ))
    )(input)}

pub fn exp1(input: Tokens) -> IResult<Tokens, AST> {
    let term1 = term(input);
    match term1 {
        Ok((rest, term)) => {
            let binop = combinator::map(
                tokensat(& |tk| match tk {
                    Token::BinopTk(_) => true,
                    Token::OtherOpTk(OtherOp::Plus) => true,
                    Token::OtherOpTk(OtherOp::Minus) => true,
                    _   => false,
                    }),
                |tk| match tk {
                    Token::BinopTk(binop) => binop,
                    Token::OtherOpTk(OtherOp::Plus) => Binop::Add,
                    Token::OtherOpTk(OtherOp::Minus) => Binop::Sub,
                    _ => panic!("Expected Binop"),
                }
            // this is probably a bad idea
            // we should pass around the tokens using a reference
            )(rest.clone());
            match binop {
                Ok((rest, binop)) => {
                    let exp2 = exp(rest);
                    match exp2 {
                        Ok((rest2, exp)) => Ok((rest2, AST::BinopApp(binop, Box::new(term), Box::new(exp)))),
                        Err(e) => Err(e),
                    }
                }
                Err(_) => Ok((rest, term)),
            }
        }
        Err(e) => Err(e),
    }
}

#[test]
fn exp1_test1() {
    let input = "2 + 2";
    let tokens = tokenizer(input).unwrap().1;
    let ast = exp1(tokens);
    assert!(ast.is_ok());
    match ast{
        Ok((rest, ast)) => {
            assert!(rest.tokens.is_empty());
            assert_eq!(ast, AST::BinopApp(Binop::Add,
                 Box::new(AST::ConstantTerm(Constant::Int(2))),
                 Box::new(AST::ConstantTerm(Constant::Int(2)))));
        }
        Err(e) => assert!(false, format!("{:?}", e)),
    }
}

#[test]
fn exp1_test2() {
    let input = "2 + ( 3 + 4 )";
    let tokens = tokenizer(input).unwrap().1;
    let ast = exp1(tokens);
    assert!(ast.is_ok());
    match ast{
        Ok((rest, ast)) => {
            assert!(rest.tokens.is_empty());
            assert_eq!(ast, AST::BinopApp(Binop::Add,
                 Box::new(AST::ConstantTerm(Constant::Int(2))),
                 Box::new(AST::BinopApp(Binop::Add,
                    Box::new(AST::ConstantTerm(Constant::Int(3))),
                    Box::new(AST::ConstantTerm(Constant::Int(4)))
                 ))
                ));
        }
        Err(e) => assert!(false, format!("{:?}", e)),
    }
}

pub fn ifexp(input: Tokens) -> IResult<Tokens, AST> {
    error::context(
        "ifexp",
        combinator::map(
            sequence::tuple((
                tokensat(& |tk| match tk {
                    Token::KeywordTk(Keyword::If) => true,
                    _ => false,
                }),
                exp,
                tokensat(& |tk| match tk {
                    Token::KeywordTk(Keyword::Then) => true,
                    _ => false,
                }),
                exp,
                tokensat(& |tk| match tk {
                    Token::KeywordTk(Keyword::Else) => true,
                    _ => false,
                }),
                exp,
            )),
            |(_, exp1, _, exp2, _, exp3)| AST::If(Box::new(exp1), Box::new(exp2), Box::new(exp3))
        )
    )(input)
}

pub fn letexp(input: Tokens) -> IResult<Tokens, AST> {
    error::context(
        "letexp",
        combinator::map(
            sequence::tuple((
                tokensat(& |tk| match tk {
                    Token::KeywordTk(Keyword::Let) => true,
                    _ => false,
                }),
                multi::many1(def),
                tokensat(& |tk| match tk {
                    Token::KeywordTk(Keyword::In) => true,
                    _ => false,
                }),
                exp,
            )),
            |(_, defs, _, exp)| AST::Let(defs, Box::new(exp))
        )
    )(input)
}

pub fn mapexp(input: Tokens) -> IResult<Tokens, AST> {
    error::context(
        "mapexp",
        combinator::map(
            sequence::tuple((
                tokensat(& |tk| match tk {
                    Token::KeywordTk(Keyword::Map) => true,
                    _ => false,
                }),
                idlist,
                tokensat(& |tk| match tk {
                    Token::KeywordTk(Keyword::To) => true,
                    _ => false,
                }),
                exp,
            )),
            |(_, ids, _, exp)| AST::Map(ids, Box::new(exp))
        )
    )(input)
}

/**
 * Def ::= Id := Exp ;
 */

pub fn def(input: Tokens) -> IResult<Tokens, Def> {
    error::context(
        "def",
        combinator::map(
            sequence::tuple((
                tokensat(& |tk| match tk {
                    Token::IdTk(_) => true,
                    _ => false,
                }),
                tokensat(& |tk| match tk {
                    Token::PunctuationTk(Punctuation::Bind) => true,
                    _ => false,
                }),
                exp,
                tokensat(& |tk| match tk {
                    Token::PunctuationTk(Punctuation::SemiColon) => true,
                    _ => false,
                }),
            )),
            |(id, _, exp, _)| match id {
                Token::IdTk(id) => Def { id, ast: exp } ,
                _ => panic!("Expected Id"),
            }
        )
    )(input)
}

pub fn parse(input: &str) -> AST{
    match tokenizer(input){
        Err(e) => panic!("Tokenization Failure {:?}", e),
        Ok((rest, tokens)) => {
            if rest.len() > 0 {
                panic!("Unexpected tokens {:?}", rest);
            }
            match exp(tokens) {
                Err(e) => panic!("Parsing Failure {:?}", e),
                Ok((rest, ast)) => {
                    if rest.tokens.len() > 0 {
                        panic!("Unexpected tokens {:?}", rest);
                    }
                    ast
                }
            }
        }
    }
}