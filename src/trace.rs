#![allow(dead_code)]
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

pub fn level() -> usize {
    TRACER_DEPTH.load(Ordering::Relaxed)
}

#[derive(Default)]
pub struct Tracer;
impl Tracer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn trace(&self, label: &str) -> Traced {
        Traced::new(label)
    }
}

pub struct Traced<'a> {
    label: Option<Cow<'a, str>>,
    pad: Option<Cow<'a, str>>,
}

impl<'a> Traced<'a> {
    pub fn new(label: &str) -> Self {
        let pad = ::std::iter::repeat(".")
            .take(level())
            .collect::<String>()
            .into();

        let label: String = label.into();
        let label = label.into();

        // {
        //     let mut w = w.borrow_mut();
        //     w.wrap(next_color(level()), &format!("{}", pad));
        //     writeln!(w, "> {}", label);
        //     w.flush().expect("flush");
        // }
        eprintln!("{}> {}", pad, label);

        indent();
        Self {
            label: Some(label),
            pad: Some(pad),
        }
    }
}

impl<'a> Drop for Traced<'a> {
    fn drop(&mut self) {
        dedent();
        // let pad = wrap_color!(colors::next_color(level()), "<{}", self.pad);
        if let (Some(pad), Some(label)) = (self.pad.take(), self.label.take()) {
            // let mut w = self.w.borrow_mut();
            // write!(w, "<");
            // w.wrap(next_color(level()), &format!("{}", pad));
            // writeln!(w, " {}", label);
            // w.flush().expect("flush");
            eprintln!("<{} {}", pad, label);
        }
    }
}
