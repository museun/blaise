//#![allow(dead_code, unused_variables, unused_imports)] // go away clippy
#![feature(custom_attribute)]

#[macro_use]
extern crate log;

pub(crate) mod trace;

#[macro_use]
pub mod colors;

pub mod config;

mod interpreter;
mod lexer;
mod parser;

#[macro_use]
pub mod prelude {
    pub use crate::interpreter::Interpreter;
    pub use crate::lexer::*;
    pub use crate::parser::{ast, Parser};

    pub use crate::colors::{Color, Writer};
    pub use crate::trace::enable_tracer;
}

#[inline(always)]
pub(crate) fn count_digits(n: usize) -> usize {
    if n == 0 {
        return 1;
    }

    let (mut n, mut x) = (n, 0);
    while n > 0 {
        n /= 10;
        x += 1;
    }
    x
}
