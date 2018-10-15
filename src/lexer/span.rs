use crate::count_digits;

use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    file: String,
    row: usize,
    col: usize,
}

impl Span {
    pub fn new(file: impl Into<String>, row: usize, col: usize) -> Self {
        Self {
            file: file.into(),
            row,
            col,
        }
    }

    #[inline(always)]
    pub fn row(&self) -> usize {
        self.row
    }

    #[inline(always)]
    pub fn column(&self) -> usize {
        self.col
    }

    pub fn total_width(&self) -> usize {
        // 2 for the colons
        2 + self.file.len() + count_digits(self.row) + count_digits(self.col)
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.row, self.col)
    }
}
