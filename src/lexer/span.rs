use crate::count_digits;
use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Span {
    row: usize,
    col: usize,
    len: usize,
}

impl Span {
    pub fn new(row: usize, col: usize, len: usize) -> Self {
        Self { row, col, len }
    }

    #[inline(always)]
    pub fn row(&self) -> usize {
        self.row
    }

    #[inline(always)]
    pub fn column(&self) -> usize {
        self.col + self.length()
    }

    #[inline(always)]
    pub fn length(&self) -> usize {
        self.len + 1
    }

    pub fn total_width(&self) -> usize {
        // 2 for the colons
        1 + count_digits(self.row()) + count_digits(self.column())
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.row(), self.column())
    }
}
