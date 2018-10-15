// #![allow(dead_code, unused_variables, unused_imports)]
#[macro_use]
extern crate log;

#[macro_use]
pub(crate) mod trace;

mod interpreter;
mod lexer;
mod parser;

pub mod prelude {
    pub use crate::interpreter::Interpreter;
    pub use crate::lexer::*;
    pub use crate::parser::{ast, Parser};
    pub use crate::trace::enable_tracer;
}

#[inline(always)]
pub(crate) fn count_digits(mut n: usize) -> usize {
    if n == 0 {
        return 1;
    }

    let mut x = 0;
    while n > 0 {
        n /= 10;
        x += 1;
    }
    x
}
