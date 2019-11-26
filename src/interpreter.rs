use super::parser::{Ast, BinOp, UniOp};
use super::{print_annot, Annot};
use std::collections::HashMap;
use std::error::Error as StdError;
use std::fmt;
use std::ops::{Add, Div, Mul, Sub};

pub struct Interpreter {
    variables: HashMap<String, Value>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
}

impl Add for Value {
    type Output = Value;

    fn add(self, other: Value) -> Value {
        use Value::*;
        match (self, other) {
            (Int(i), Int(j)) => Int(i + j),
            (Int(i), Float(j)) => Float(i as f64 + j),
            (Float(i), Int(j)) => Float(i + j as f64),
            (Float(i), Float(j)) => Float(i + j),
        }
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, other: Value) -> Value {
        use Value::*;
        match (self, other) {
            (Int(i), Int(j)) => Int(i - j),
            (Int(i), Float(j)) => Float(i as f64 - j),
            (Float(i), Int(j)) => Float(i - j as f64),
            (Float(i), Float(j)) => Float(i - j),
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, other: Value) -> Value {
        use Value::*;
        match (self, other) {
            (Int(i), Int(j)) => Int(i * j),
            (Int(i), Float(j)) => Float(i as f64 * j),
            (Float(i), Int(j)) => Float(i * j as f64),
            (Float(i), Float(j)) => Float(i * j),
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, other: Value) -> Value {
        use Value::*;
        match (self, other) {
            (Int(i), Int(j)) => Int(i / j),
            (Int(i), Float(j)) => Float(i as f64 / j),
            (Float(i), Int(j)) => Float(i / j as f64),
            (Float(i), Float(j)) => Float(i / j),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;

        match self {
            Int(i) => write!(f, "{}", i),
            Float(fl) => write!(f, "{}", fl),
        }
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            variables: HashMap::new(),
        }
    }

    /// 構文木を入力に受け取り、結果を返す関数
    /// 現在は整数しか返すことができないが、変更する予定
    pub fn eval(&mut self, expr: &Ast) -> Result<Value, InterpreterError> {
        use super::parser::AstKind::*;
        match &expr.value {
            Variable(s) => match self.variables.get(&s.to_string()) {
                Some(n) => Ok(*n),
                None => Err(InterpreterError::new(
                    InterpreterErrorKind::NotDefinedVariable(s.to_string()),
                    expr.loc.clone(),
                )),
            },
            Int(n) => Ok(Value::Int(*n as i64)),
            Float(f) => Ok(Value::Float(*f)),
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

    fn eval_uniop(&mut self, op: &UniOp, n: Value) -> Value {
        use super::parser::UniOpKind::*;
        use Value::*;

        match op.value {
            Plus => n,
            Minus => match n {
                Int(i) => Int(-i),
                Float(f) => Float(-f),
            },
        }
    }

    fn eval_binop(
        &mut self,
        op: &BinOp,
        l: Value,
        r: Value,
    ) -> Result<Value, InterpreterErrorKind> {
        use super::parser::BinOpKind::*;
        use Value::*;

        match op.value {
            Add => Ok(l + r),
            Sub => Ok(l - r),
            Mult => Ok(l * r),
            Div => match r {
                Int(0) => Err(InterpreterErrorKind::DivisionByZero),
                _ => {
                    let res = l / r;

                    match res {
                        Int(_) => Ok(res),
                        Float(f) => {
                            if f.is_normal() {
                                Ok(res)
                            } else {
                                Err(InterpreterErrorKind::DivisionByZero)
                            }
                        }
                    }
                }
            },
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

#[test]
fn test_eval() {
    use super::parser::{Ast, BinOp, UniOp};
    use super::Loc;
    let mut interpreter = Interpreter::new();

    // a = 1. + 2 * 3 - -10.
    assert_eq!(
        interpreter
            .eval(&Ast::eq(
                Ast::variable("a".to_string(), Loc(0, 1)),
                Ast::binop(
                    BinOp::sub(Loc(15, 16)),
                    Ast::binop(
                        BinOp::add(Loc(7, 8)),
                        Ast::float(1., Loc(4, 6)),
                        Ast::binop(
                            BinOp::mult(Loc(11, 12)),
                            Ast::int(2, Loc(9, 10)),
                            Ast::int(3, Loc(13, 14)),
                            Loc(9, 14)
                        ),
                        Loc(4, 14),
                    ),
                    Ast::uniop(
                        UniOp::minus(Loc(17, 18)),
                        Ast::float(10., Loc(18, 21)),
                        Loc(17, 21)
                    ),
                    Loc(4, 21)
                ),
                Loc(0, 21)
            ))
            .unwrap(),
        Value::Float(17.)
    );
}
