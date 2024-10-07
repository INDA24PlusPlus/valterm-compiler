pub mod lexer;

fn main() {
    let mut l = lexer::Lexer::new("if int skibidi=1337*3;".to_string());
    println!("{:?}", l.lex());
}
