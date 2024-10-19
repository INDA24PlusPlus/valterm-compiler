use crate::{
    ast::{Expr, Operator, Stmt},
    lexer::{Token, TokenType},
};

#[derive(Debug)]
pub enum ParserError {
    ExpectedToken {
        expected: TokenType,
        got: Option<Token>,
        index: usize,
    },
}

impl ParserError {
    fn expected_token(expected: TokenType, got: Option<Token>, index: usize) -> ParserError {
        ParserError::ExpectedToken {
            expected,
            got,
            index,
        }
    }
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ParserError::ExpectedToken {
                expected,
                got,
                index,
            } => match got {
                Some(g) => write!(
                    f,
                    "Expected token {:?} at index {}, got {:?}",
                    expected, index, g
                ),
                None => write!(
                    f,
                    "Expected token {:?} at index {}, got None",
                    expected, index
                ),
            },
        }
    }
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    pub program: Vec<Stmt>,
    index: usize,
}

/*
Goal: recursive descent parser
Example grammar without left recursion:
<expr> ::= <term> (("+" <term>) | ("-" <term>))*
<term> ::= <factor> (("*" <factor>) | ("/" <factor>))*
<factor> ::= <integer> | "(" <expr> ")"
<integer> ::= <number> (<number>)*
<number> ::= [0-9]
*/

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            program: vec![],
            index: 0,
        }
    }

    /// peek(0) returns the current token
    fn peek(&self, n: usize) -> Option<Token> {
        self.tokens.get(self.index + n).cloned()
    }

    fn previous(&self) -> Option<Token> {
        self.tokens.get(self.index - 1).cloned()
    }

    // Does NOT consume the token
    fn expect(&self, token_type: TokenType) -> Result<Token, ParserError> {
        match self.peek(0) {
            Some(t) if t.token == token_type => Ok(t),
            _ => Err(ParserError::expected_token(token_type, None, self.index)),
        }
    }

    fn expect_type(&self, token_type: TokenType) -> Result<Token, ParserError> {
        match self.peek(0) {
            Some(t) if t.token == token_type || matches!(token_type, TokenType::Identifier(_)) => {
                Ok(t)
            }
            _ => Err(ParserError::expected_token(token_type, None, self.index)),
        }
    }

    fn consume(&mut self) -> Option<Token> {
        let token = self.peek(0);
        // println!("Consuming {:?}", token);
        self.index += 1;
        token
    }

    pub fn parse(&mut self) -> Result<(), ParserError> {
        while self.index < self.tokens.len() {
            let statement = self.parse_statement()?;
            self.program.push(statement);
        }
        Ok(())
    }

    /// Highest level of the grammar, handles arbitrary statements
    fn parse_statement(&mut self) -> Result<Stmt, ParserError> {
        if let Some(token) = self.peek(0) {
            match token.token {
                TokenType::Identifier(_) => {
                    if self.peek(1).is_some_and(|t| t.token == TokenType::LParen) {
                        // Function call
                        self.parse_call()
                    } else {
                        // Assignment
                        self.parse_assignment()
                    }
                }
                TokenType::If => self.parse_if_statement(),
                TokenType::While => self.parse_while_statement(),
                TokenType::LBrace => self.parse_block(),
                _ => Ok(Stmt::Expr(self.parse_expr(true)?)),
            }
        } else {
            Err(ParserError::expected_token(
                TokenType::Identifier("".to_string()),
                None,
                self.index,
            ))
        }
    }

    fn parse_call(&mut self) -> Result<Stmt, ParserError> {
        let identifier = self.expect_type(TokenType::Identifier("".to_string()))?;
        self.consume();
        self.expect(TokenType::LParen)?;
        self.consume();

        let mut args = vec![];
        while let Some(token) = self.peek(0) {
            if token.token == TokenType::RParen {
                self.consume();
                break;
            }
            let expr = self.parse_expr(false)?;
            args.push(expr);
            if let Some(token) = self.peek(0) {
                if token.token == TokenType::Comma {
                    self.consume();
                }
            }
        }

        self.expect(TokenType::Semicolon)?;
        self.consume();

        Ok(Stmt::Call {
            function: match identifier.token {
                TokenType::Identifier(s) => s,
                _ => unreachable!(),
            },
            args,
        })
    }

    fn parse_while_statement(&mut self) -> Result<Stmt, ParserError> {
        self.expect(TokenType::While)?;
        self.consume();
        self.expect(TokenType::LParen)?;
        self.consume();

        let condition = self.parse_expr(false)?;

        self.expect(TokenType::RParen)?;
        self.consume();

        let body = self.parse_statement()?;
        Ok(Stmt::While {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    fn parse_if_statement(&mut self) -> Result<Stmt, ParserError> {
        self.expect(TokenType::If)?;
        self.consume();
        self.expect(TokenType::LParen)?;
        self.consume();

        let condition = self.parse_expr(false)?;

        self.expect(TokenType::RParen)?;
        self.consume();

        let if_body = self.parse_statement()?;

        if let Some(token) = self.peek(0) {
            if token.token == TokenType::Else {
                self.consume();
                let else_body = self.parse_statement()?;
                return Ok(Stmt::If {
                    condition: Box::new(condition),
                    if_body: Box::new(if_body),
                    else_body: Some(Box::new(else_body)),
                });
            }
        }

        Ok(Stmt::If {
            condition: Box::new(condition),
            if_body: Box::new(if_body),
            else_body: None,
        })
    }

    fn parse_block(&mut self) -> Result<Stmt, ParserError> {
        self.expect(TokenType::LBrace)?;
        self.consume();
        let mut block = vec![];
        while let Some(token) = self.peek(0) {
            if token.token == TokenType::RBrace {
                self.consume();
                break;
            }
            let statement = self.parse_statement()?;
            block.push(statement);
        }

        Ok(Stmt::Block { body: block })
    }

    /// Handle assignment statements
    fn parse_assignment(&mut self) -> Result<Stmt, ParserError> {
        //let _type_identifier = self.consume().unwrap(); // Idk about types tbh
        let identifier = self.expect_type(TokenType::Identifier("".to_string()))?;
        self.consume();
        self.expect(TokenType::Assignment)?;
        self.consume();

        let expr = self.parse_expr(true)?;

        Ok(Stmt::Expr(Expr::Assignment {
            symbol: match identifier.token {
                TokenType::Identifier(s) => s,
                _ => unreachable!(),
            },
            value: Box::new(expr),
        }))
    }

    /// Handle binary expressions
    fn parse_expr(&mut self, semicolon: bool) -> Result<Expr, ParserError> {
        let term = self.parse_term()?;
        if semicolon {
            self.expect(TokenType::Semicolon)?;
            self.consume();
        }
        Ok(term)
    }

    /// Handle addition and subtraction of terms
    fn parse_term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_factor()?;
        while let Some(tok) = self.peek(0) {
            if tok.token == TokenType::Plus || tok.token == TokenType::Minus {
                self.consume();
                let op = match tok.token {
                    TokenType::Plus => Operator::Add,
                    TokenType::Minus => Operator::Subtract,
                    _ => unreachable!(),
                };
                let right = self.parse_factor()?;
                expr = Expr::BinaryExpr {
                    op,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
            } else if tok.token == TokenType::Equals {
                self.consume();
                let left = expr;
                let right = self.parse_expr(false)?;
                expr = Expr::BinaryExpr {
                    op: Operator::Equals,
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else if tok.token == TokenType::NotEquals {
                self.consume();
                let left = expr;
                let right = self.parse_expr(false)?;
                expr = Expr::BinaryExpr {
                    op: Operator::NotEquals,
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    /// Handle multiplication and division of factors
    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_integer()?;
        while let Some(tok) = self.peek(0) {
            if tok.token == TokenType::Star || tok.token == TokenType::Slash {
                self.consume();
                let op = match tok.token {
                    TokenType::Star => Operator::Multiply,
                    TokenType::Slash => Operator::Divide,
                    _ => unreachable!(),
                };
                let right = self.parse_integer()?;
                expr = Expr::BinaryExpr {
                    op,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    /// Handle raw integers as well as parenthesized expressions and identifiers
    fn parse_integer(&mut self) -> Result<Expr, ParserError> {
        match self.consume() {
            Some(token) => match token.token {
                TokenType::Integer(n) => Ok(Expr::Integer(n)),
                TokenType::LParen => {
                    let expr = self.parse_expr(false)?;
                    self.expect(TokenType::RParen)?; // Must be followed by a closing parenthesis
                    self.consume(); // Consume the closing parenthesis
                    Ok(expr)
                }
                TokenType::True => Ok(Expr::Boolean(true)),
                TokenType::False => Ok(Expr::Boolean(false)),
                TokenType::Identifier(ident) => Ok(Expr::Reference(ident)),
                _ => Err(ParserError::expected_token(
                    TokenType::Expression,
                    self.previous(),
                    self.index - 1,
                )),
            },
            None => Err(ParserError::expected_token(
                TokenType::Expression,
                None,
                self.index - 1,
            )),
        }
    }
}
