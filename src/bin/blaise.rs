#[macro_use]
extern crate log;

use std::env;
use std::fs;

use blaise::prelude::*;

fn main() {
    env_logger::Builder::from_default_env()
        .default_format_timestamp(false)
        .init();

    let (name, mut args) = {
        let mut args = env::args();
        (args.next().unwrap(), args)
    };

    let file = match args.next() {
        Some(file) => file,
        None => {
            eprintln!("usage: {} <file>", name);
            ::std::process::exit(1)
        }
    };

    let input = fs::read_to_string(&file).expect("read");
    eprintln!("{}\n", input);

    let mut tokens = scan(&file, &input);
    tokens.remove_comments();
    eprintln!("{}", tokens);

    let mut parser = Parser::new(tokens);
    let program = match parser.parse() {
        Ok(program) => program,
        Err(err) => {
            error!("\n{:?}", err);
            ::std::process::exit(1);
        }
    };
    eprintln!("{:#?}", program);

    let mut interpreter = Interpreter::new();
    interpreter.evaluate(program);
}
