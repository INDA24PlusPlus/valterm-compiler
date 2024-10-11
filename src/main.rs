pub mod ast;
pub mod lexer;
pub mod parser;

fn main() {
    let mut l = lexer::Lexer::new("int skibidi=1337*a".to_string());
    let tokens = l.lex().unwrap();
    println!("{:?}", tokens);
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
