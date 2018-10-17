use std::fmt;
use std::sync::atomic::{AtomicBool, Ordering};

pub(crate) static COLOR_ENABLED: AtomicBool = AtomicBool::new(false);
pub(crate) fn colors_enabled() -> bool {
    COLOR_ENABLED.load(Ordering::Relaxed)
}

pub fn enable_colors() {
    COLOR_ENABLED.store(true, Ordering::Relaxed);
}

#[derive(Copy, Clone)]
pub enum Color {
    Red = 1,
    Green = 2,
    Yellow = 3,
    Blue = 4,
    Magenta = 5,
    Cyan = 6,
    White = 7,
    BrightRed = 8,
    BrightGreen = 9,
    BrightYellow = 10,
    BrightBlue = 11,
    BrightMagenta = 12,
    BrightCyan = 13,
    BrightWhite = 14,
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if colors_enabled() {
            write!(f, "{}", self.get())
        } else {
            write!(f, "")
        }
    }
}

static COLORS: [Color; 14] = [
    Color::Red,
    Color::Green,
    Color::Yellow,
    Color::Blue,
    Color::Magenta,
    Color::Cyan,
    Color::White,
    Color::BrightRed,
    Color::BrightGreen,
    Color::BrightYellow,
    Color::BrightBlue,
    Color::BrightMagenta,
    Color::BrightCyan,
    Color::BrightWhite,
];

pub fn next_color(current: usize) -> Color {
    COLORS[(current + COLORS.len() - 1) % COLORS.len()]
}

impl Color {
    pub const fn reset() -> &'static str {
        "\x1B[m"
    }

    pub fn get(self) -> &'static str {
        match self {
            Color::Red => "\x1B[31m",
            Color::Green => "\x1B[32m",
            Color::Yellow => "\x1B[33m",
            Color::Blue => "\x1B[34m",
            Color::Magenta => "\x1B[35m",
            Color::Cyan => "\x1B[36m",
            Color::White => "\x1B[37m",

            Color::BrightRed => "\x1B[91m",
            Color::BrightGreen => "\x1B[92m",
            Color::BrightYellow => "\x1B[93m",
            Color::BrightBlue => "\x1B[94m",
            Color::BrightMagenta => "\x1B[95m",
            Color::BrightCyan => "\x1B[96m",
            Color::BrightWhite => "\x1B[97m",
        }
    }
}

#[macro_export]
macro_rules! wrap_color {
    ($color:expr, $fmt:expr) => {{
        if $crate::colors::colors_enabled()  {
            format!("{}{}{}", $color.get(), $fmt, Color::reset())
        } else {
            format!("{}", $fmt)
        }
    }};
    ($color:expr, $fmt:expr, $($arg:tt)*) => {{
        if $crate::colors::colors_enabled() {
            format!("{}{}{}", $color.get(), format_args!($fmt, $($arg)*), $crate::colors::Color::reset())
        } else{
            format!("{}", format_args!($fmt, $($arg)*))
        }
    }};
}
