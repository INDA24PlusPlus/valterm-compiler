use inkwell::{builder::Builder, context::Context, module::Module};

use crate::ast::Stmt;

pub struct Codegen {
    context: Context,
    module: Module,
    builder: Builder,
}

impl Codegen {
    pub fn new() -> Codegen {
        let context = Context::create();
        let module = context.create_module("main");
        let builder = context.create_builder();

        Codegen {
            context,
            module,
            builder,
        }
    }

    pub fn compile_statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => todo!(),
            Stmt::Block { body } => todo!(),
            Stmt::If {
                condition,
                if_body,
                else_body,
            } => todo!(),
            Stmt::While { condition, body } => todo!(),
            Stmt::Call { function, args } => todo!(),
        }
    }
}
