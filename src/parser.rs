use super::lexer::lex;
use super::{Annot, Error, Loc, Token, TokenKind};
use std::error::Error as StdError;
use std::fmt;
use std::iter::Peekable;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq)]
pub enum AstKind {
    Variable(String),
    Int(u64),
    Float(f64),
    EqOp { l: Box<Ast>, r: Box<Ast> },
    UniOp { op: UniOp, e: Box<Ast> },
    BinOp { op: BinOp, l: Box<Ast>, r: Box<Ast> },
}

pub type Ast = Annot<AstKind>;

impl Ast {
    fn variable(s: String, loc: Loc) -> Self {
        Self::new(AstKind::Variable(s), loc)
    }

    fn int(n: u64, loc: Loc) -> Self {
        Self::new(AstKind::Int(n), loc)
    }

    fn float(f: f64, loc: Loc) -> Self {
        Self::new(AstKind::Float(f), loc)
    }

    fn eq(l: Ast, r: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::EqOp {
                l: Box::new(l),
                r: Box::new(r),
            },
            loc,
        )
    }

    fn uniop(op: UniOp, e: Ast, loc: Loc) -> Self {
        Self::new(AstKind::UniOp { op, e: Box::new(e) }, loc)
    }

    fn binop(op: BinOp, l: Ast, r: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::BinOp {
                op,
                l: Box::new(l),
                r: Box::new(r),
            },
            loc,
        )
    }
}

impl FromStr for Ast {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let tokens = lex(s)?;
        let ast = parse(tokens)?;
        Ok(ast)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UniOpKind {
    Plus,
    Minus,
}

pub type UniOp = Annot<UniOpKind>;

impl UniOp {
    fn plus(loc: Loc) -> Self {
        Self::new(UniOpKind::Plus, loc)
    }

    fn minus(loc: Loc) -> Self {
        Self::new(UniOpKind::Minus, loc)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Add,
    Sub,
    Mult,
    Div,
}

pub type BinOp = Annot<BinOpKind>;

impl BinOp {
    fn add(loc: Loc) -> Self {
        Self::new(BinOpKind::Add, loc)
    }

    fn sub(loc: Loc) -> Self {
        Self::new(BinOpKind::Sub, loc)
    }

    fn mult(loc: Loc) -> Self {
        Self::new(BinOpKind::Mult, loc)
    }

    fn div(loc: Loc) -> Self {
        Self::new(BinOpKind::Div, loc)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedToken(Token),
    NotExpression(Token),
    NotOperator(Token),
    UnclosedOpenParen(Token),
    RedundantExpression(Token),
    Eof,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ParseError::*;
        match self {
            UnexpectedToken(tok) => write!(f, "{}: {}  is not expected", tok.loc, tok.value),
            NotExpression(tok) => write!(
                f,
                "{}: {}  is not a start of expression",
                tok.loc, tok.value
            ),
            NotOperator(tok) => write!(f, "{}: {}  is an operator", tok.loc, tok.value),
            UnclosedOpenParen(tok) => write!(f, "{}: {}  is not closed", tok.loc, tok.value),
            RedundantExpression(tok) => write!(
                f,
                "{}: expression after '{}' is redundant",
                tok.loc, tok.value
            ),
            Eof => write!(f, "End of file"),
        }
    }
}

impl StdError for ParseError {}

fn parse(tokens: Vec<Token>) -> Result<Ast, ParseError> {
    let mut tokens = tokens.into_iter().peekable();
    let ret = parse_expr(&mut tokens)?;
    match tokens.next() {
        Some(tok) => Err(ParseError::RedundantExpression(tok)),
        None => Ok(ret),
    }
}

fn parse_expr<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let left_ast = parse_expr3(tokens)?;
    match tokens.next() {
        Some(Annot {
            value: TokenKind::Equal,
            loc,
        }) => {
            let right_ast = parse_expr3(tokens)?;
            match left_ast.value {
                AstKind::Variable(_) => {
                    let loc = left_ast.loc.merge(&right_ast.loc);
                    Ok(Ast::eq(left_ast, right_ast, loc))
                }
                _ => Err(ParseError::UnexpectedToken(Token {
                    value: TokenKind::Equal,
                    loc,
                })),
            }
        }
        None => Ok(left_ast),
        _ => Err(ParseError::Eof),
    }
}

/// 数字の前の+, -を認識する
fn parse_expr1<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    match tokens.peek().map(|tok| tok.value.clone()) {
        Some(TokenKind::Plus) | Some(TokenKind::Minus) => {
            let op = match tokens.next() {
                Some(Token {
                    value: TokenKind::Plus,
                    loc,
                }) => UniOp::plus(loc),
                Some(Token {
                    value: TokenKind::Minus,
                    loc,
                }) => UniOp::minus(loc),
                _ => unreachable!(),
            };

            let e = parse_atom(tokens)?;
            let loc = op.loc.merge(&e.loc);
            Ok(Ast::uniop(op, e, loc))
        }
        _ => parse_atom(tokens),
    }
}

/// ただの数字か()で囲まれた部分をパースする
/// もし、()で囲まれていれば普通のパースを行う
fn parse_atom<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value {
            TokenKind::Variable(s) => Ok(Ast::variable(s, tok.loc)),
            TokenKind::Int(n) => Ok(Ast::int(n, tok.loc)),
            TokenKind::Float(f) => Ok(Ast::float(f, tok.loc)),
            TokenKind::LParen => {
                let e = parse_expr(tokens)?;
                match tokens.next() {
                    Some(Token {
                        value: TokenKind::RParen,
                        ..
                    }) => Ok(e),
                    Some(t) => Err(ParseError::RedundantExpression(t)),
                    _ => Err(ParseError::UnclosedOpenParen(tok)),
                }
            }
            _ => Err(ParseError::NotExpression(tok)),
        })
}

fn parse_expr2<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    fn parse_expr2_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp, ParseError>
    where
        Tokens: Iterator<Item = Token>,
    {
        let op = tokens
            .peek()
            .ok_or(ParseError::Eof)
            .and_then(|tok| match tok.value {
                TokenKind::Asterisk => Ok(BinOp::mult(tok.loc.clone())),
                TokenKind::Slash => Ok(BinOp::div(tok.loc.clone())),
                _ => Err(ParseError::NotExpression(tok.clone())),
            })?;
        tokens.next();
        Ok(op)
    }

    parse_left_binop(tokens, parse_expr1, parse_expr2_op)
}

fn parse_expr3<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    fn parse_expr3_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp, ParseError>
    where
        Tokens: Iterator<Item = Token>,
    {
        let op = tokens
            .peek()
            .ok_or(ParseError::Eof)
            .and_then(|tok| match tok.value {
                TokenKind::Plus => Ok(BinOp::add(tok.loc.clone())),
                TokenKind::Minus => Ok(BinOp::sub(tok.loc.clone())),
                _ => Err(ParseError::NotExpression(tok.clone())),
            })?;
        tokens.next();
        Ok(op)
    }

    parse_left_binop(tokens, parse_expr2, parse_expr3_op)
}

fn parse_left_binop<Tokens>(
    tokens: &mut Peekable<Tokens>,
    subexpr_parser: fn(&mut Peekable<Tokens>) -> Result<Ast, ParseError>,
    op_parser: fn(&mut Peekable<Tokens>) -> Result<BinOp, ParseError>,
) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let mut e = subexpr_parser(tokens)?;
    loop {
        match tokens.peek() {
            Some(_) => {
                let op = match op_parser(tokens) {
                    Ok(op) => op,
                    Err(_) => break,
                };
                let r = subexpr_parser(tokens)?;
                let loc = e.loc.merge(&r.loc);
                e = Ast::binop(op, e, r, loc);
            }
            _ => break,
        }
    }
    Ok(e)
}

impl From<ParseError> for Error {
    fn from(e: ParseError) -> Self {
        Error::Parser(e)
    }
}

#[test]
fn test_parser() {
    // a = 1. + 2 * 3 - -10.
    let ast = parse(vec![
        Token::variable("a".to_string(), Loc(0, 1)),
        Token::equal(Loc(2, 3)),
        Token::float(1., Loc(4, 6)),
        Token::plus(Loc(7, 8)),
        Token::int(2, Loc(9, 10)),
        Token::asterisk(Loc(11, 12)),
        Token::int(3, Loc(13, 14)),
        Token::minus(Loc(15, 16)),
        Token::minus(Loc(17, 18)),
        Token::float(10., Loc(18, 21)),
    ]);

    assert_eq!(
        ast,
        Ok(Ast::eq(
            Ast::variable("a".to_string(), Loc(0, 1)),
            Ast::binop(
                BinOp::sub(Loc(15, 16)),
                Ast::binop(
                    BinOp::add(Loc(7, 8)),
                    Ast::float(1., Loc(4, 6)),
                    Ast::binop(
                        BinOp::new(BinOpKind::Mult, Loc(11, 12)),
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
    );
}
