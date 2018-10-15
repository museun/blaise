// #![allow(dead_code, unused_variables, unused_imports)]
#[macro_use]
extern crate log;

#[macro_export]
macro_rules! defer {
    ($e:expr) => {
        let _scoped = $crate::Scoped((), |_| $e);
    };
}

#[macro_export]
macro_rules! traced {
    ($name:expr) => {
        let _scoped = $crate::Scoped($crate::Tracer::new($name), |_| {});
    };
}

mod interpreter;
mod lexer;
mod parser;

pub mod prelude {
    pub use crate::interpreter::Interpreter;
    pub use crate::lexer::*;
    pub use crate::parser::{ast, Parser};
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

use std::ops::{Deref, DerefMut};
pub(crate) struct Scoped<T, F: FnMut(&mut T)>(T, F);

#[inline]
pub(crate) fn scoped<T, F: FnMut(&mut T)>(val: T, func: F) -> Scoped<T, F> {
    Scoped { 0: val, 1: func }
}

impl<T, F: FnMut(&mut T)> Deref for Scoped<T, F> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T, F: FnMut(&mut T)> DerefMut for Scoped<T, F> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

impl<T, F: FnMut(&mut T)> Drop for Scoped<T, F> {
    fn drop(&mut self) {
        (self.1)(&mut self.0)
    }
}

use std::borrow::Cow;
use std::sync::atomic::{AtomicUsize, Ordering};

const IDENTATION: usize = 2;
static TRACER_DEPTH: AtomicUsize = AtomicUsize::new(0);

pub fn indent() {
    let _ = TRACER_DEPTH.fetch_add(IDENTATION, Ordering::Relaxed);
}

pub fn dedent() {
    let _ = TRACER_DEPTH.fetch_sub(IDENTATION, Ordering::Relaxed);
}

pub fn reset_level() {
    TRACER_DEPTH.store(0, Ordering::Relaxed)
}

pub fn level() -> usize {
    TRACER_DEPTH.load(Ordering::Relaxed)
}

pub struct Tracer<'a> {
    label: Cow<'a, str>,
    pad: Cow<'a, str>,
}

impl<'a> Tracer<'a> {
    pub fn new(label: &str) -> Self {
        let pad = ::std::iter::repeat(".")
            .take(level())
            .collect::<String>()
            .into();

        let label: String = label.into();
        let label = label.into();
        trace!("{}>{}", pad, label);
        indent();
        Tracer { label, pad }
    }
}

impl<'a> Drop for Tracer<'a> {
    fn drop(&mut self) {
        dedent();
        trace!("<{}{}", self.pad, self.label);
    }
}
