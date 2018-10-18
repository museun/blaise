#![allow(dead_code)]
use std::borrow::Cow;
use std::ops::{Deref, DerefMut};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

const IDENTATION: usize = 2;
static TRACER_DEPTH: AtomicUsize = AtomicUsize::new(0);
pub(crate) static TRACER_ENABLED: AtomicBool = AtomicBool::new(false);

#[macro_export]
macro_rules! defer {
    ($e:expr) => {
        let _scoped = $crate::Scoped((), |_| $e);
    };
}

#[macro_export]
macro_rules! traced {
    ($name:expr) => {
        use crate::trace::{scoped, Tracer};
        let _scoped = scoped(Tracer::new($name, "".to_string()), |_| {});
    };

    ($name:expr, $fmt:expr, $($arg:tt)*) => {
        use crate::trace::{scoped, Tracer};
        let _scoped = scoped(Tracer::new($name, format!($fmt, $($arg)*)), |_| {});
    };
}

pub struct Scoped<T, F: FnMut(&mut T)>(T, F);

#[inline]
#[allow(dead_code)]
pub fn scoped<T, F: FnMut(&mut T)>(val: T, func: F) -> Scoped<T, F> {
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

pub fn enable_tracer() {
    TRACER_ENABLED.store(true, Ordering::Relaxed);
}

pub fn indent() {
    let _ = TRACER_DEPTH.fetch_add(IDENTATION, Ordering::Relaxed);
}

pub fn dedent() {
    let _ = TRACER_DEPTH.fetch_sub(IDENTATION, Ordering::Relaxed);
}

#[allow(dead_code)]
pub fn reset_level() {
    TRACER_DEPTH.store(0, Ordering::Relaxed)
}

pub fn level() -> usize {
    TRACER_DEPTH.load(Ordering::Relaxed)
}

pub struct Tracer<'a> {
    label: Cow<'a, str>,
    data: Cow<'a, str>,
    pad: Cow<'a, str>,
}

impl<'a> Tracer<'a> {
    pub fn new(label: &str, data: String) -> Self {
        let pad = ::std::iter::repeat(".")
            .take(level())
            .collect::<String>()
            .into();

        let data = if data.is_empty() {
            data.into()
        } else {
            format!(": {}", data).into()
        };

        let label: String = label.into();
        let label = label.into();
        if TRACER_ENABLED.load(::std::sync::atomic::Ordering::Relaxed) {
            // let pad = wrap_color!(colors::next_color(level()), "{}>", pad);
            eprintln!("{} {}{}", pad, label, data);
        }
        indent();
        Tracer { label, pad, data }
    }
}

impl<'a> Drop for Tracer<'a> {
    fn drop(&mut self) {
        dedent();
        if TRACER_ENABLED.load(::std::sync::atomic::Ordering::Relaxed) {
            // let pad = wrap_color!(colors::next_color(level()), "<{}", self.pad);
            eprintln!("{} {}{}", self.pad, self.label, self.data);
        }
    }
}
