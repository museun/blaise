#[macro_use]
extern crate log;

use std::env;
use std::fs;

use blaise::prelude::*;

fn die(data: &str) -> ! {
    eprintln!("{}", data);
    ::std::process::exit(1)
}

fn main() {
    let (name, mut args) = {
        let mut args = env::args();
        (args.next().unwrap(), args)
    };

    let file = match args.next() {
        Some(file) => file,
        None => die(&format!("usage: {} <file>", name)),
    };

    let blaise_source = env::var("BLAISE_SOURCE").is_ok();
    let blaise_tokens = env::var("BLAISE_TOKENS").is_ok();
    let blaise_ast = env::var("BLAISE_AST").is_ok();

    if env::var("BLAISE_TRACE").is_ok() {
        enable_tracer()
    }

    let colors = env::var("NO_COLOR").is_err();
    if colors {
        enable_colors()
    }

    env_logger::Builder::from_default_env()
        .default_format_timestamp(false)
        .write_style(if colors {
            // TODO maybe try parse_write_style incase the user does something different
            env_logger::fmt::WriteStyle::Auto
        } else {
            env_logger::fmt::WriteStyle::Never
        })
        .init();

    let input = fs::read_to_string(&file).expect("read");
    if blaise_source {
        eprintln!("{}\n", input);
    }

    let mut tokens = scan(&file, &input);
    tokens.remove_comments();
    if blaise_tokens {
        eprintln!("{}", tokens);
    }

    let parser = Parser::new(tokens);
    let program = match parser.parse() {
        Ok(program) => program,
        Err(err) => {
            error!("\n{:?}", err);
            ::std::process::exit(1);
        }
    };
    if blaise_ast {
        eprintln!("{:#?}", program);
    }

    let mut interpreter = Interpreter::new();
    interpreter.evaluate(program);
}
