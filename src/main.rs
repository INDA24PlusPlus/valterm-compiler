use compiler::Codebuilder;
use inkwell::context::Context;

pub mod ast;
pub mod compiler;
pub mod lexer;
pub mod parser;

const SOURCE: &str = "
a = 1337;
print(a);
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
    let mut codegen = Codebuilder::new(&ctx);
    let mut compiler = compiler::Compiler::new();
    codegen.compile(&mut compiler, p.program).unwrap();
}
