#[derive(Debug, PartialEq)]
pub enum TokenType {
    Integer(i64),
    Identifier(String),
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    Equal,
    Semicolon,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    token: TokenType,
    index: usize,
}

#[derive(Debug, PartialEq)]
pub enum LexerState {
    Start,
    Integer,
    Identifier,
}

pub struct Lexer {
    buffer: String,
    tokens: Vec<Token>,
    state: LexerState,
    size: usize,
    index: usize,
}

#[derive(Debug)]
pub enum LexerError {
    InvalidCharacter(usize),
    OutOfBounds,
}

impl Lexer {
    pub fn new(buffer: String) -> Lexer {
        Lexer {
            size: buffer.len(),
            state: LexerState::Start,
            tokens: Vec::new(),
            buffer,
            index: 0,
        }
    }

    fn get_current(&self) -> Result<char, LexerError> {
        match self.buffer.chars().nth(self.index) {
            Some(c) => Ok(c),
            None => Err(LexerError::OutOfBounds),
        }
    }

    pub fn lex(&mut self) -> Result<&Vec<Token>, LexerError> {
        while self.index < self.size {
            let c = self.get_current()?;
            if c.is_whitespace() {
                self.index += 1;
                self.state = LexerState::Start;
                continue;
            }
            match self.state {
                LexerState::Start => {
                    match c {
                        '0'..='9' => {
                            self.state = LexerState::Integer;
                        }
                        'a'..='z' | 'A'..='Z' | '_' => {
                            self.state = LexerState::Identifier;
                        }
                        '+' => {
                            self.tokens.push(Token {
                                token: TokenType::Plus,
                                index: self.index,
                            });
                        }
                        '-' => {
                            self.tokens.push(Token {
                                token: TokenType::Minus,
                                index: self.index,
                            });
                        }
                        '*' => {
                            self.tokens.push(Token {
                                token: TokenType::Star,
                                index: self.index,
                            });
                        }
                        '/' => {
                            self.tokens.push(Token {
                                token: TokenType::Slash,
                                index: self.index,
                            });
                        }
                        '(' => {
                            self.tokens.push(Token {
                                token: TokenType::LParen,
                                index: self.index,
                            });
                        }
                        ')' => {
                            self.tokens.push(Token {
                                token: TokenType::RParen,
                                index: self.index,
                            });
                        }
                        '=' => {
                            self.tokens.push(Token {
                                token: TokenType::Equal,
                                index: self.index,
                            });
                        }
                        ';' => {
                            self.tokens.push(Token {
                                token: TokenType::Semicolon,
                                index: self.index,
                            });
                        }
                        _ => {
                            return Err(LexerError::InvalidCharacter(self.index));
                        }
                    }
                    if self.state == LexerState::Start {
                        self.index += 1;
                    }
                }
                LexerState::Integer => {
                    let mut digits = String::new();
                    loop {
                        let c = self.get_current()?;
                        if !c.is_ascii_digit() {
                            break;
                        }
                        digits.push(c);
                        self.index += 1;
                        if self.index >= self.size {
                            break;
                        }
                    }
                    let parsed = match digits.parse::<i64>() {
                        Ok(n) => n,
                        Err(_) => return Err(LexerError::InvalidCharacter(self.index)),
                    };
                    self.tokens.push(Token {
                        index: self.index - digits.len(),
                        token: TokenType::Integer(parsed),
                    });
                    self.state = LexerState::Start;
                }
                LexerState::Identifier => {
                    let mut identifier = String::new();
                    loop {
                        let c = self.get_current()?;
                        if !c.is_ascii_alphanumeric() && c != '_' {
                            break;
                        }
                        identifier.push(c);
                        self.index += 1;
                        if self.index >= self.size {
                            break;
                        }
                    }
                    self.tokens.push(Token {
                        index: self.index - identifier.len(),
                        token: TokenType::Identifier(identifier),
                    });
                    self.state = LexerState::Start;
                }
            }
        }

        Ok(&self.tokens)
    }
}
