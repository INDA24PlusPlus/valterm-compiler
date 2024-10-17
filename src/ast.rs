#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equals,
}

#[derive(Debug)]
pub enum Expr {
    Integer(i64),      // Integer literal
    Boolean(bool),     // Boolean literal
    Reference(String), // Identifier reference
    BinaryExpr {
        op: Operator,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    UnaryExpr {
        op: Operator,
        expr: Box<Expr>,
    },
    Assignment {
        symbol: String,
        value: Box<Expr>,
    },
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Block {
        body: Vec<Stmt>,
    },
    If {
        condition: Box<Expr>,
        if_body: Box<Stmt>,
        else_body: Option<Box<Stmt>>,
    },
    While {
        condition: Box<Expr>,
        body: Box<Stmt>,
    },
    Call {
        function: String,
        args: Vec<Expr>,
    },
}
