use std::{env,fs};

#[macro_use]
extern crate blaise;

use blaise::prelude::*;
use blaise::config::*;

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

    let config = OutputConfig::parse();

    simplelog::SimpleLogger::init(
        match config.log_level {
            LogLevel::Off => simplelog::LevelFilter::Off,
            LogLevel::Trace => simplelog::LevelFilter::Trace,
            LogLevel::Debug => simplelog::LevelFilter::Debug,
            LogLevel::Info => simplelog::LevelFilter::Info,
            LogLevel::Warn => simplelog::LevelFilter::Warn,
            LogLevel::Error => simplelog::LevelFilter::Error,
        },
        simplelog::Config::default(),
    )
    .expect("to enable logging");

    if config.use_colors {
        enable_colors()
    }

    let input = fs::read_to_string(&file).expect("read");
    if config.show_source {
        eprintln!(
            "{}\n{}",
            wrap_color!(Color::BrightYellow {}, "Source=>"),
            input
        );
    }

    let mut tokens = scan(&input);
    tokens.remove_comments();

    if config.show_tokens {
        eprintln!(
            "{}\n{}",
            wrap_color!(Color::BrightYellow {}, "Tokens=>"),
            tokens
        );
    }

    if config.show_trace {
        enable_tracer();
        eprintln!("{}", wrap_color!(Color::BrightYellow {}, "Parse trace=>"));
    }
    let parser = Parser::new(tokens, input, file);
    let program = match parser.parse() {
        Ok(program) => program,
        Err(err) => die(&format!("{}", err)),
    };
    if config.show_trace {
        eprintln!();
    }

    if config.show_ast {
        eprintln!(
            "{}\n{:#?}",
            wrap_color!(Color::BrightYellow {}, "AST=>"),
            program
        );
    }

    eprintln!("{}", wrap_color!(Color::BrightYellow {}, "Evaluation=>"));
    let interpreter = Interpreter::new();
    match interpreter.evaluate(program) {
        Ok(res) => eprintln!(
            "\n{}\n{:?}",
            wrap_color!(Color::BrightYellow {}, "Result=>"),
            res
        ),
        Err(err) => die(&format!("{}", err)),
    }
}
