use super::parser::{Ast, BinOp, UniOp};
use super::{print_annot, Annot};
use std::collections::HashMap;
use std::error::Error as StdError;
use std::fmt;

pub struct Interpreter {
    variables: HashMap<String, i64>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            variables: HashMap::new(),
        }
    }

    /// 構文木を入力に受け取り、結果を返す関数
    /// 現在は整数しか返すことができないが、変更する予定
    pub fn eval(&mut self, expr: &Ast) -> Result<i64, InterpreterError> {
        use super::parser::AstKind::*;
        match &expr.value {
            Variable(s) => match self.variables.get(&s.to_string()) {
                Some(n) => Ok(*n),
                None => Err(InterpreterError::new(
                    InterpreterErrorKind::NotDefinedVariable(s.to_string()),
                    expr.loc.clone(),
                )),
            },
            Num(n) => Ok(*n as i64),
            EqOp { ref l, ref r } => {
                let r = self.eval(r)?;

                match &l.value {
                    Variable(s) => {
                        self.variables.insert(s.to_string(), r);
                    }
                    _ => unreachable!(),
                }
                Ok(r)
            }
            UniOp { ref op, ref e } => {
                let e = self.eval(e)?;
                Ok(self.eval_uniop(op, e))
            }
            BinOp {
                ref op,
                ref l,
                ref r,
            } => {
                let l = self.eval(l)?;
                let r = self.eval(r)?;
                self.eval_binop(op, l, r)
                    .map_err(|e| InterpreterError::new(e, expr.loc.clone()))
            }
        }
    }

    fn eval_uniop(&mut self, op: &UniOp, n: i64) -> i64 {
        use super::parser::UniOpKind::*;
        match op.value {
            Plus => n,
            Minus => -n,
        }
    }

    fn eval_binop(&mut self, op: &BinOp, l: i64, r: i64) -> Result<i64, InterpreterErrorKind> {
        use super::parser::BinOpKind::*;
        match op.value {
            Add => Ok(l + r),
            Sub => Ok(l - r),
            Mult => Ok(l * r),
            Div => {
                if r == 0 {
                    Err(InterpreterErrorKind::DivisionByZero)
                } else {
                    Ok(l / r)
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InterpreterErrorKind {
    NotDefinedVariable(String),
    DivisionByZero,
}

pub type InterpreterError = Annot<InterpreterErrorKind>;

impl InterpreterError {
    pub fn show_diagnostic(&self, std: &str) {
        eprintln!("{}", self);
        print_annot(std, self.loc.clone());
    }
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::InterpreterErrorKind::*;

        match &self.value {
            NotDefinedVariable(s) => write!(f, "name {} is not defined", s),
            DivisionByZero => write!(f, "division by zero"),
        }
    }
}

impl StdError for InterpreterError {}
