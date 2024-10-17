use codegen::Codegen;
use inkwell::context::Context;

pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod parser;

const SOURCE: &str = "
a = 1337;
b = 42;
";

fn main() {
    let mut l = lexer::Lexer::new(SOURCE.to_string());
    let tokens = l.lex().unwrap();
    //println!("{:?}", tokens);
    let mut p = parser::Parser::new(tokens.clone());
    match p.parse() {
        Ok(_) => (),
        Err(e) => {
            println!("{}", e);
            return;
        }
    }
    //println!("{:?}", p.program);
    let ctx = Context::create();
    let mut codegen = Codegen::new(&ctx);
    codegen.compile(p.program);
}
