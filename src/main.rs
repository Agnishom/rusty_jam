pub mod ast;
pub mod parser;
pub mod lexer;
pub mod evaluator; 

fn main() {
    let input = "(map x to x)(1)";
    let ast = parser::parse(input);
    println!("{:?}", ast);
    let ans = evaluator::eval(&ast);
    println!("{:?}", ans);
}
