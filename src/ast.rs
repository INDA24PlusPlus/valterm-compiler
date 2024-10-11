#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
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
    If {
        condition: Box<Expr>,
        if_body: Vec<Expr>,
        else_body: Vec<Expr>,
    },
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    If {
        condition: Box<Expr>,
        if_body: Vec<Stmt>,
        else_body: Vec<Stmt>,
    },
}
