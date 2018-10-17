use std::env;
use std::ops::Deref;

#[derive(Default, Debug)]
pub struct OutputConfig {
    pub show_source: bool,
    pub show_tokens: bool,
    pub show_ast: bool,
    pub show_trace: bool,
    pub use_colors: bool,
    pub log_level: LogLevel,
}

impl OutputConfig {
    pub fn parse() -> OutputConfig {
        fn check(name: &str) -> bool {
            env::var(name).is_ok()
        }

        Self {
            show_source: check("BLAISE_SOURCE"),
            show_tokens: check("BLAISE_TOKENS"),
            show_ast: check("BLAISE_AST"),
            show_trace: check("BLAISE_TRACE"),
            use_colors: !check("NO_COLOR"),
            log_level: {
                if let Ok(s) = env::var("BLAISE_LEVEL") {
                    LogLevel::parse(&s)
                } else {
                    LogLevel::default()
                }
            },
        }
    }
}

#[derive(Debug)]
pub enum LogLevel {
    Off,
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

impl Default for LogLevel {
    fn default() -> Self {
        LogLevel::Info
    }
}

impl LogLevel {
    fn parse(s: &str) -> Self {
        match s.to_ascii_lowercase().as_str() {
            "off" => LogLevel::Off,
            "trace" => LogLevel::Trace,
            "debug" => LogLevel::Debug,
            "info" => LogLevel::Info,
            "warn" => LogLevel::Warn,
            "error" => LogLevel::Error,
            _ => Self::default(),
        }
    }
}
