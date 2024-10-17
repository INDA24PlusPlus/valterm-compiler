pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod parser;

const SOURCE: &str = "
a = 1337;b = 42;
if (a == b) {
    print(a);
} else b = 0;
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
    println!("{:?}", p.program);
}
