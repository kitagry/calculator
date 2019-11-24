pub struct RpnCompiler;

use super::parser::{Ast, BinOpKind, UniOpKind};

impl RpnCompiler {
    pub fn new() -> Self {
        RpnCompiler
    }

    /// 構文木を入力として受け取り、数式をポーランド記法で返す関数
    pub fn compile(&mut self, expr: &Ast) -> String {
        let mut buf = String::new();
        self.compile_inner(expr, &mut buf);
        buf
    }

    fn compile_inner(&mut self, expr: &Ast, buf: &mut String) {
        use super::parser::AstKind::*;

        match expr.value {
            Num(n) => buf.push_str(&n.to_string()),
            UniOp { ref op, ref e } => {
                self.compile_uniop(&op.value, buf);
                self.compile_inner(e, buf);
            }
            BinOp {
                ref op,
                ref l,
                ref r,
            } => {
                self.compile_inner(l, buf);
                buf.push_str(" ");
                self.compile_inner(r, buf);
                buf.push_str(" ");
                self.compile_binop(&op.value, buf)
            }
            _ => unimplemented!(),
        }
    }

    fn compile_uniop(&mut self, op: &UniOpKind, buf: &mut String) {
        use self::UniOpKind::*;
        match *op {
            Plus => buf.push_str("+"),
            Minus => buf.push_str("-"),
        }
    }

    fn compile_binop(&mut self, op: &BinOpKind, buf: &mut String) {
        use self::BinOpKind::*;
        match *op {
            Add => buf.push_str("+"),
            Sub => buf.push_str("-"),
            Mult => buf.push_str("*"),
            Div => buf.push_str("/"),
        }
    }
}
