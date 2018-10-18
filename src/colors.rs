use std::sync::Arc;
use std::io::Write;

use termcolor::{WriteColor, Buffer, BufferWriter, ColorSpec};

pub enum Color {
    Black,
    Blue,
    Green,
    Red,
    Cyan,
    Magenta,
    Yellow,
    White,
}

impl From<Color> for termcolor::Color {
    fn from(c: Color) -> Self {
        match c {
            Color::Black => termcolor::Color::Black,
            Color::Blue => termcolor::Color::Blue,
            Color::Green => termcolor::Color::Green,
            Color::Red => termcolor::Color::Red,
            Color::Cyan => termcolor::Color::Cyan,
            Color::Magenta => termcolor::Color::Magenta,
            Color::Yellow => termcolor::Color::Yellow,
            Color::White => termcolor::Color::White,
        }
    }
}

pub struct Writer {
    w: Arc<BufferWriter>,
    buf: Option<Buffer>,
}

impl Writer {
    pub fn new(w: Arc<BufferWriter>) -> Self {
        Self { w, buf: None }
    }

    pub fn wrap<S: AsRef<str>>(&mut self, color: Color, data: S) {
        let buf = match self.buf {
            Some(ref mut buf) => buf,
            None => {
                self.buf = Some(self.w.buffer());
                self.buf.as_mut().unwrap()
            }
        };

        if atty::isnt(atty::Stream::Stderr) {
            write!(buf, "{}", data.as_ref());
            return;
        }

        let mut spec = ColorSpec::new();
        spec.set_fg(Some(color.into()));

        buf.set_color(&spec).expect("set color");
        write!(buf, "{}", data.as_ref());
        buf.reset().expect("reset");
    }
}

impl Write for Writer {
    fn write(&mut self, data: &[u8]) -> std::io::Result<usize> {
        let buf = match self.buf {
            Some(ref mut buf) => buf,
            None => {
                self.buf = Some(self.w.buffer());
                self.buf.as_mut().unwrap()
            }
        };

        buf.write(data)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        if let Some(ref buf) = self.buf.take() {
            self.w.print(buf)?;
        }
        Ok(())
    }
}
