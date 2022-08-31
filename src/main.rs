pub mod ast;
pub mod parser;
pub mod lexer;

fn main() {
    let input = "let x := 1 ; in x";
    println!("{:?}", parser::parse(input));
}
