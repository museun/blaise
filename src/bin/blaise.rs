use std::{env,fs};
use std::sync::Arc;
use std::io::prelude::*;

use termcolor::{BufferWriter, ColorChoice};

extern crate blaise;

use blaise::prelude::*;
use blaise::config::*;

fn die(data: &str) -> ! {
    if !data.is_empty() {
        eprintln!("{}", data);
    }
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

    let w = Arc::new(BufferWriter::stderr(if config.use_colors {
        ColorChoice::Auto
    } else {
        ColorChoice::Never
    }));
    let mut writer = Writer::new(Arc::clone(&w));

    let input = fs::read_to_string(&file).expect("read");
    if config.show_source {
        writer.wrap(Color::Yellow, "Source=>\n");
        writeln!(writer, "{}", input);
        writer.flush().expect("flush");
    }

    let mut tokens = scan(&input);
    tokens.remove_comments();

    if config.show_tokens {
        writer.wrap(Color::Yellow, "Tokens=>\n");
        writeln!(writer, "{}", tokens);
        writer.flush().expect("flush");
    }

    // if config.show_trace {
    //     enable_tracer();
    //     writer.wrap(Color::Yellow, "Parse trace=>\n");
    //     writer.flush().expect("flush");
    // }

    let parser = Parser::new(tokens, input, file);
    let program = match parser.parse() {
        Ok(program) => program,
        Err(err) => {
            err.print(&mut writer);
            die("")
        }
    };
    if config.show_trace {
        writeln!(writer);
        writer.flush().expect("flush");
    }

    if config.show_ast {
        writer.wrap(Color::Yellow, "AST=>\n");
        writeln!(writer, "{:#?}", program);
        writer.flush().expect("flush");
    }

    writer.wrap(Color::Yellow, "Evaluation=>\n");
    writer.flush().expect("flush");

    match Interpreter::evaluate(program) {
        Ok(res) => {
            writer.wrap(Color::Yellow, "Result=>\n");
            writeln!(writer, "{:?}", res);
            writer.flush().expect("flush");
        }
        Err(err) => die(&format!("{:?}", err)),
    }
}
