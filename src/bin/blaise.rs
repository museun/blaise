use std::env;
use std::fs;

use blaise::*;

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
    let mut tokens = Lexer::scan(&file, &input);
    tokens.remove_comments();
    eprintln!("{}", tokens);

    let ast = Parser::parse(tokens);
    let mut interpreter = Interpreter::new();
    interpreter.evaluate(&ast);
}
