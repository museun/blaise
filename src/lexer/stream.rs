use std::fmt;

#[derive(Clone)]
pub struct Stream<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> fmt::Debug for Stream<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Stream")
            .field("pos", &self.pos)
            .field("len", &self.input.len())
            .field("input", &self.input)
            .field("rem", &self.input.get(self.pos..))
            .finish()
    }
}

// TODO abstract this away
impl<'a> Stream<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    pub fn peek(&self) -> Option<char> {
        self.at(self.pos())
    }

    pub fn advance(&mut self, amt: usize) {
        self.pos += amt
    }

    pub fn current(&self) -> char {
        self.at(self.pos()).unwrap()
    }

    pub fn at(&self, pos: usize) -> Option<char> {
        self.input.chars().nth(pos)
    }

    #[allow(dead_code)]
    pub fn seek(&mut self, pos: usize) {
        self.pos = pos;
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    #[allow(dead_code)]
    pub fn eof(&self) -> bool {
        self.pos >= self.input.len()
    }
}

impl<'a> Iterator for Stream<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        if self.pos == self.input.len() {
            return None;
        }
        let n = self.at(self.pos());
        self.pos += 1;
        n
    }
}
