#[macro_use]
extern crate log;

#[macro_use]
pub(crate) mod trace;

#[macro_use]
pub(crate) mod colors;

mod interpreter;
mod lexer;
mod parser;

pub mod prelude {
    pub use crate::interpreter::Interpreter;
    pub use crate::lexer::*;
    pub use crate::parser::{ast, Parser};

    pub use crate::colors::enable_colors;
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
