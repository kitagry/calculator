//! lexerは字句解析を行い、Tokenの配列を出します。
//! 詳しくは[`lex`](/lexer/fn.lex.html)を参考にしてください。
use super::{Annot, Error, Loc, Token};
use std::error::Error as StdError;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LexErrorKind {
    InvalidChar(char),
    Overflow,
    Eof,
}

pub type LexError = Annot<LexErrorKind>;

impl LexError {
    fn invalid_char(c: char, loc: Loc) -> Self {
        LexError::new(LexErrorKind::InvalidChar(c), loc)
    }

    fn overflow(loc: Loc) -> Self {
        LexError::new(LexErrorKind::Overflow, loc)
    }

    fn eof(loc: Loc) -> Self {
        LexError::new(LexErrorKind::Eof, loc)
    }
}

impl From<LexError> for Error {
    fn from(e: LexError) -> Self {
        Error::Lexer(e)
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::LexErrorKind::*;
        let loc = &self.loc;
        match self.value {
            InvalidChar(c) => write!(f, "{}: invalid char '{}", loc, c),
            Overflow => write!(f, "overflow!"),
            Eof => write!(f, "End of file"),
        }
    }
}

impl StdError for LexError {}

/// lexは文字列を受け取り、Token列を返す関数です。
/// 不正な文字などがあればLexErrorを返します。
pub fn lex(input: &str) -> Result<Vec<Token>, LexError> {
    let mut tokens = Vec::new();
    let input = input.as_bytes();

    let mut pos = 0;
    macro_rules! lex_a_token {
        ($lexer:expr) => {{
            let (tok, p) = $lexer?;
            tokens.push(tok);
            pos = p;
        }};
    }

    while pos < input.len() {
        match input[pos] {
            b'a'..=b'z' | b'A'..=b'Z' => lex_a_token!(lex_variable(input, pos)),
            b'0'..=b'9' => lex_a_token!(lex_number(input, pos)),
            b'+' => lex_a_token!(lex_plus(input, pos)),
            b'-' => lex_a_token!(lex_minus(input, pos)),
            b'*' => lex_a_token!(lex_asterisk(input, pos)),
            b'/' => lex_a_token!(lex_slash(input, pos)),
            b'(' => lex_a_token!(lex_lparen(input, pos)),
            b')' => lex_a_token!(lex_rparen(input, pos)),
            b'=' => lex_a_token!(lex_equal(input, pos)),
            b' ' | b'\n' | b'\t' => {
                let ((), p) = skip_spaces(input, pos);
                pos = p;
            }
            b => return Err(LexError::invalid_char(b as char, Loc(pos, pos + 1))),
        }
    }
    Ok(tokens)
}

fn consume_byte(input: &[u8], pos: usize, b: u8) -> Result<(u8, usize), LexError> {
    if input.len() <= pos {
        return Err(LexError::eof(Loc(pos, pos)));
    }

    if input[pos] != b {
        return Err(LexError::invalid_char(
            input[pos] as char,
            Loc(pos, pos + 1),
        ));
    }

    Ok((b, pos + 1))
}

fn recognize_many(input: &[u8], mut pos: usize, mut f: impl FnMut(u8) -> bool) -> usize {
    while pos < input.len() && f(input[pos]) {
        pos += 1;
    }
    pos
}

fn lex_variable(input: &[u8], pos: usize) -> Result<(Token, usize), LexError> {
    use regex::Regex;
    use std::str::from_utf8;

    let start = pos;
    let char_regex = Regex::new("[a-zA-Z]").unwrap();
    let end = recognize_many(input, start, |b| {
        char_regex.is_match(from_utf8(&[b]).unwrap())
    });

    let s = from_utf8(&input[start..end]).unwrap();
    Ok((Token::variable(s.to_string(), Loc(start, end)), end))
}

fn lex_number(input: &[u8], pos: usize) -> Result<(Token, usize), LexError> {
    use std::str::from_utf8;
    let start = pos;
    let end = recognize_many(input, start, |b| b"1234567890".contains(&b));

    if input.len() > end && b".".contains(&input[end]) {
        let end = recognize_many(input, end + 1, |b| b"1234567890".contains(&b));
        let f = from_utf8(&input[start..end]).unwrap().parse().unwrap();
        Ok((Token::float(f, Loc(start, end)), end))
    } else {
        let n = from_utf8(&input[start..end]).unwrap().parse();
        match n {
            Ok(n) => Ok((Token::int(n, Loc(start, end)), end)),
            Err(_) => Err(LexError::overflow(Loc(start, end))),
        }
    }
}

fn lex_plus(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'+').map(|(_, end)| (Token::plus(Loc(start, end)), end))
}

fn lex_minus(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'-').map(|(_, end)| (Token::minus(Loc(start, end)), end))
}

fn lex_asterisk(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'*').map(|(_, end)| (Token::asterisk(Loc(start, end)), end))
}

fn lex_slash(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'/').map(|(_, end)| (Token::slash(Loc(start, end)), end))
}

fn lex_lparen(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'(').map(|(_, end)| (Token::lparen(Loc(start, end)), end))
}

fn lex_rparen(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b')').map(|(_, end)| (Token::rparen(Loc(start, end)), end))
}

fn lex_equal(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'=').map(|(_, end)| (Token::equal(Loc(start, end)), end))
}

fn skip_spaces(input: &[u8], pos: usize) -> ((), usize) {
    let pos = recognize_many(input, pos, |b| b" \n\t".contains(&b));
    ((), pos)
}

#[test]
fn test_lexer() {
    assert_eq!(
        lex("a = 1. + 2 * 3 - -10."),
        Ok(vec![
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
        ])
    )
}
