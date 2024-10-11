#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Integer(i64),
    Identifier(String),
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    Assignment,
    Semicolon,
    If,
    Else,
    Equals, // Binary comparator
    True,
    False,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token: TokenType,
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

    fn peek(&self, n: usize) -> Option<char> {
        self.buffer.chars().nth(self.index + n)
    }

    fn identifier_or_keyword(&self, identifier: String) -> TokenType {
        match identifier.as_str() {
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "true" => TokenType::True,
            "false" => TokenType::False,
            _ => TokenType::Identifier(identifier),
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
                    let token_type = match c {
                        '0'..='9' => {
                            self.state = LexerState::Integer;
                            None
                        }
                        'a'..='z' | 'A'..='Z' | '_' => {
                            self.state = LexerState::Identifier;
                            None
                        }
                        '+' => Some(TokenType::Plus),
                        '-' => Some(TokenType::Minus),
                        '*' => Some(TokenType::Star),
                        '/' => Some(TokenType::Slash),
                        '(' => Some(TokenType::LParen),
                        ')' => Some(TokenType::RParen),
                        '=' => {
                            if self.peek(1) == Some('=') {
                                self.index += 1;
                                Some(TokenType::Equals)
                            } else {
                                Some(TokenType::Assignment)
                            }
                        }
                        ';' => Some(TokenType::Semicolon),
                        _ => {
                            return Err(LexerError::InvalidCharacter(self.index));
                        }
                    };
                    if let Some(t) = token_type {
                        self.tokens.push(Token {
                            index: self.index,
                            token: t,
                        });
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
                        token: self.identifier_or_keyword(identifier),
                    });
                    self.state = LexerState::Start;
                }
            }
        }

        Ok(&self.tokens)
    }
}
