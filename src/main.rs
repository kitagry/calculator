extern crate parser;

use parser::interpreter::Interpreter;
use parser::parser::Ast;
use std::error::Error as StdError;
use std::io;

fn prompt(s: &str) -> io::Result<()> {
    use std::io::{stdout, Write};
    let stdout = stdout();
    let mut stdout = stdout.lock();
    stdout.write(s.as_bytes())?;
    stdout.flush()
}

fn show_trace<E: StdError>(e: E) {
    eprintln!("{}", e);
    let mut source = e.source();
    while let Some(e) = source {
        eprintln!("caused by {}", e);
        source = e.source()
    }
}

fn main() {
    use std::io::{stdin, BufRead, BufReader};

    let mut interp = Interpreter::new();
    // let mut compiler = RpnCompiler::new();

    let stdin = stdin();
    let stdin = stdin.lock();
    let stdin = BufReader::new(stdin);
    let mut lines = stdin.lines();

    loop {
        prompt("> ").unwrap();
        if let Some(Ok(line)) = lines.next() {
            let ast = match line.parse::<Ast>() {
                Ok(ast) => ast,
                Err(e) => {
                    e.show_diagnostic(&line);
                    show_trace(e);
                    continue;
                }
            };

            let n = match interp.eval(&ast) {
                Ok(n) => n,
                Err(e) => {
                    e.show_diagnostic(&line);
                    show_trace(e);
                    continue;
                }
            };
            // let rpn = compiler.compile(&ast);
            println!("{}", n);
        } else {
            break;
        }
    }
}
