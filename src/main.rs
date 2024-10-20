use compiler::Codebuilder;
use inkwell::context::Context;

pub mod ast;
pub mod compiler;
pub mod lexer;
pub mod parser;

// Fibonacci sequence
const SOURCE: &str = "
i = 0;
a = 0;
b = 1;
while (i != 20) {
    print(a);
    tmp = a;
    a = b;
    b = tmp + b;
    i = i + 1;
}
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
