pub mod ast;
pub mod parser;
pub mod lexer;

fn main() {
    let input = "2 + 2";
    let tokens = lexer::tokenizer(input).unwrap().1;
    let ast = parser::exp1(tokens);
    println!("{:?}", ast);
}
