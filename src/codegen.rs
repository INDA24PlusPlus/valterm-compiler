use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValueEnum, IntValue, PointerValue},
};

use crate::ast::{Expr, Stmt};

pub struct Codegen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,

    variables: HashMap<String, PointerValue<'ctx>>,
}

impl From<inkwell::builder::BuilderError> for CodegenError {
    fn from(e: inkwell::builder::BuilderError) -> Self {
        CodegenError::BuilderError(e)
    }
}

pub enum CodegenError {
    InvalidExpr,
    BuilderError(inkwell::builder::BuilderError),
    UndefinedSymbol(String),
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context) -> Codegen<'ctx> {
        let module = context.create_module("main");
        let builder = context.create_builder();

        Codegen {
            context,
            module,
            builder,

            variables: HashMap::new(),
        }
    }

    pub fn compile(&mut self, program: Vec<Stmt>) -> Result<(), CodegenError> {
        // Create main function
        let main_type = self.context.i32_type().fn_type(&[], false);
        let main = self.module.add_function("main", main_type, None);

        // Create entry basic block
        let basic_block = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(basic_block);

        // Compile each statement of program
        for stmt in program {
            self.compile_statement(&stmt)?;
        }

        println!("{:?}", self.module.print_to_string().to_string());

        Ok(())
    }

    fn compile_statement(&mut self, stmt: &Stmt) -> Result<(), CodegenError> {
        match stmt {
            Stmt::Expr(expr) => self.compile_expr(expr)?,
            Stmt::Block { body } => todo!(),
            Stmt::If {
                condition,
                if_body,
                else_body,
            } => todo!(),
            Stmt::While { condition, body } => todo!(),
            Stmt::Call { function, args } => todo!(),
        };

        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<BasicValueEnum, CodegenError> {
        match expr {
            Expr::Integer(i) => {
                let i64_type = self.context.i64_type();
                let i64_value = i64_type.const_int(*i as u64, false);
                Ok(BasicValueEnum::IntValue(i64_value))
            }
            Expr::Boolean(b) => Ok(BasicValueEnum::IntValue(
                self.context
                    .i64_type()
                    .const_int(if *b { 1 } else { 0 }, false),
            )),
            Expr::Reference(s) => {
                let ptr = match self.variables.get(s) {
                    Some(ptr) => *ptr,
                    None => return Err(CodegenError::UndefinedSymbol(s.clone())),
                };
                let i64_type = self.context.i64_type();
                let value = self.builder.build_load(i64_type, ptr, s)?;
                Ok(value)
            }
            Expr::BinaryExpr { op, left, right } => todo!(),
            Expr::UnaryExpr { op, expr } => todo!(),
            Expr::Assignment { symbol, value } => {
                let i64_type = self.context.i64_type();
                let ptr = match self.variables.get(symbol) {
                    Some(ptr) => *ptr,
                    None => {
                        let ptr = self.builder.build_alloca(i64_type, symbol)?;
                        self.variables.insert(symbol.clone(), ptr);
                        ptr
                    }
                };
                let compiled_value = self.compile_expr(value)?;

                self.builder.build_store(ptr, compiled_value)?;
                Ok(compiled_value)
            }
        }
    }
}
