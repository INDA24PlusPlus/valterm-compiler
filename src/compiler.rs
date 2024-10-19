use std::{collections::HashMap, fs::File, path::Path};

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    values::{BasicValue, IntValue, PointerValue},
    AddressSpace,
};

use crate::ast::{Expr, Operator, Stmt};

/// Contains mutable state of the compiler
#[derive(Debug, Default)]
pub struct Compiler<'ctx> {
    pub variables: HashMap<String, PointerValue<'ctx>>,
}

/// Contains only LLVM objects
#[derive(Debug)]
pub struct Codebuilder<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
}

impl From<inkwell::builder::BuilderError> for CompilerError {
    fn from(e: inkwell::builder::BuilderError) -> Self {
        CompilerError::BuilderError(e)
    }
}

#[derive(Debug)]
pub enum CompilerError {
    InvalidExpr,
    BuilderError(inkwell::builder::BuilderError),
    UndefinedSymbol(String),
}

impl<'ctx> Compiler<'ctx> {
    pub fn new() -> Compiler<'ctx> {
        Compiler {
            variables: HashMap::new(),
        }
    }

    pub fn get_variable(&self, name: &str) -> Option<&PointerValue<'ctx>> {
        self.variables.get(name)
    }

    pub fn set_variable(&mut self, name: String, value: PointerValue<'ctx>) {
        self.variables.insert(name, value);
    }
}

impl<'ctx> Codebuilder<'ctx> {
    pub fn new(context: &'ctx Context) -> Codebuilder<'ctx> {
        let module = context.create_module("main");
        let builder = context.create_builder();

        Codebuilder {
            context,
            module,
            builder,
        }
    }

    fn build_print_function(&self) -> Result<(), CompilerError> {
        let i64_type = self.context.i64_type();
        let void_type = self.context.void_type();
        let print_type = void_type.fn_type(&[i64_type.into()], false);
        let print = self.module.add_function("print", print_type, None);

        let string_type = self.context.ptr_type(AddressSpace::default());
        let printf_type = self.context.i32_type().fn_type(&[string_type.into()], true);
        let printf = self
            .module
            .add_function("printf", printf_type, Some(Linkage::External));

        let basic_block = self.context.append_basic_block(print, "entry");
        self.builder.position_at_end(basic_block);

        let fmt_string = self.builder.build_global_string_ptr("%ld\n", "print_fmt")?;

        let arg = print.get_first_param().unwrap();
        self.builder.build_call(
            printf,
            &[fmt_string.as_pointer_value().into(), arg.into()],
            "printf",
        )?;

        self.builder.build_return(None)?;

        Ok(())
    }

    pub fn compile(
        &mut self,
        compiler: &mut Compiler<'ctx>,
        program: Vec<Stmt>,
    ) -> Result<(), CompilerError> {
        // Create main function
        self.build_print_function()?;

        let main_type = self.context.i32_type().fn_type(&[], false);
        let main = self.module.add_function("main", main_type, None);

        // Create entry basic block
        let basic_block = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(basic_block);

        // Compile each statement of program
        for stmt in program {
            self.compile_statement(compiler, &stmt)?;
        }

        self.builder
            .build_return(Some(&self.context.i32_type().const_int(0, false)))?;

        println!("{:?}", self.module.verify());

        self.module.print_to_file("output.ll").unwrap();
        self.module.write_bitcode_to_path(Path::new("output.bc"));

        Ok(())
    }

    fn compile_statement(
        &self,
        compiler: &mut Compiler<'ctx>,
        stmt: &Stmt,
    ) -> Result<(), CompilerError> {
        match stmt {
            Stmt::Expr(expr) => {
                self.compile_expr(compiler, expr)?;
            }
            Stmt::Block { body } => {
                for stmt in body {
                    self.compile_statement(compiler, stmt)?;
                }
            }
            Stmt::If {
                condition,
                if_body,
                else_body,
            } => {
                let current_block = self.builder.get_insert_block().unwrap();
                let then_block = self
                    .context
                    .append_basic_block(current_block.get_parent().unwrap(), "then");
                let continue_block = self
                    .context
                    .append_basic_block(current_block.get_parent().unwrap(), "continue");

                let else_block = match else_body {
                    Some(_) => self
                        .context
                        .append_basic_block(current_block.get_parent().unwrap(), "else"),
                    None => continue_block,
                };

                let condition = self.compile_expr(compiler, condition)?;
                self.builder
                    .build_conditional_branch(condition, then_block, else_block)?;

                self.builder.position_at_end(then_block);
                self.compile_statement(compiler, if_body)?;
                if let Some(else_body) = else_body {
                    self.builder.build_unconditional_branch(continue_block)?;
                    self.builder.position_at_end(else_block);
                    self.compile_statement(compiler, else_body)?;
                }
                self.builder.build_unconditional_branch(continue_block)?;
                self.builder.position_at_end(continue_block);
            }
            Stmt::While { condition, body } => {
                let current_block = self.builder.get_insert_block().unwrap();
                let func = current_block.get_parent().unwrap();
                let loop_block = self.context.append_basic_block(func, "loop");
                let continue_block = self.context.append_basic_block(func, "continue");

                // Jump to loop block
                self.builder.build_unconditional_branch(loop_block)?;

                self.builder.position_at_end(loop_block);

                self.compile_statement(compiler, body)?;

                let condition = self.compile_expr(compiler, condition)?;
                self.builder
                    .build_conditional_branch(condition, loop_block, continue_block)?;

                self.builder.position_at_end(continue_block);
            }
            Stmt::Call { function, args } => {
                if function != "print" {
                    return Err(CompilerError::UndefinedSymbol(function.clone()));
                }

                let print = self.module.get_function("print").unwrap();
                let arg = self.compile_expr(compiler, &args[0])?;
                self.builder.build_call(print, &[arg.into()], "print")?;
            }
        };

        Ok(())
    }

    fn compile_expr(
        &self,
        compiler: &mut Compiler<'ctx>,
        expr: &Expr,
    ) -> Result<IntValue, CompilerError> {
        match expr {
            Expr::Integer(i) => {
                let i64_type = self.context.i64_type();
                let i64_value = i64_type.const_int(*i as u64, false);
                Ok(i64_value)
            }
            Expr::Boolean(b) => Ok(self
                .context
                .i64_type()
                .const_int(if *b { 1 } else { 0 }, false)),
            Expr::Reference(s) => {
                let ptr = match compiler.get_variable(s) {
                    Some(ptr) => *ptr,
                    None => return Err(CompilerError::UndefinedSymbol(s.clone())),
                };
                let i64_type = self.context.i64_type();
                let value = self.builder.build_load(i64_type, ptr, s)?;
                Ok(value.into_int_value())
            }
            Expr::BinaryExpr { op, left, right } => {
                let left = self.compile_expr(compiler, left)?;
                let right = self.compile_expr(compiler, right)?;

                let built = match *op {
                    Operator::Add => self.builder.build_int_add(left, right, "v_add"),
                    Operator::Subtract => self.builder.build_int_sub(left, right, "v_sub"),
                    Operator::Multiply => self.builder.build_int_mul(left, right, "v_mul"),
                    Operator::Divide => self.builder.build_int_unsigned_div(left, right, "v_div"),
                    Operator::Equals => self.builder.build_int_compare(
                        inkwell::IntPredicate::EQ,
                        left,
                        right,
                        "v_eq",
                    ),
                    Operator::NotEquals => self.builder.build_int_compare(
                        inkwell::IntPredicate::NE,
                        left,
                        right,
                        "v_ne",
                    ),
                }?;

                Ok(built)
            }
            Expr::UnaryExpr { op, expr } => todo!(), // i literally dont parse unary expressions so idk why i have this
            Expr::Assignment { symbol, value } => {
                let i64_type = self.context.i64_type();
                let ptr = match compiler.get_variable(symbol) {
                    Some(ptr) => *ptr,
                    None => {
                        let ptr = self.builder.build_alloca(i64_type, symbol)?;
                        compiler.set_variable(symbol.clone(), ptr);
                        ptr
                    }
                };
                let compiled_value = self.compile_expr(compiler, value)?;

                self.builder.build_store(ptr, compiled_value)?;
                Ok(compiled_value)
            }
        }
    }
}
