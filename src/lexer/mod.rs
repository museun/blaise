mod span;
mod stream;
mod tokens;

use self::stream::Stream;
use self::token::Type;

pub mod token;

pub use self::span::Span;
pub use self::token::{Token, TokenType};
pub use self::tokens::Tokens;

pub fn scan(input: &str) -> Tokens {
    let lexers = [
        whitespace_lexer,
        directive_lexer,
        comment_lexer,
        number_lexer,
        string_lexer,
        symbol_lexer,
        type_lexer,
        identifier_lexer,
        unknown_lexer,
    ];

    let mut data = vec![];

    let (mut col, mut row, mut skip) = (1, 0, 0);
    let mut stream = Stream::new(input);

    for c in input.chars() {
        row += 1;
        if c == '\n' {
            row = 0;
            col += 1;
        }

        if skip > 0 {
            skip -= 1;
            stream.advance(1);
            continue;
        }

        let mut error = None;
        'inner: for lexer in &lexers {
            match lexer(&mut stream.clone()) {
                State::Yield => continue,
                State::Error(err) => error = Some(err),
                State::Consume(n) => skip += n,
                State::Produce(n, TokenType::Comment(_, end)) => {
                    skip += n;
                    let span = Span::new(col, row, n);
                    data.push(Token::new(span, TokenType::Comment(row, row + end)))
                }
                State::Produce(n, token) => {
                    skip += n;
                    let span = Span::new(col, row, n);
                    data.push(Token::new(span, token))
                }
            }
            break 'inner;
        }

        match error {
            Some(Error::UnknownToken) => {
                let span = Span::new(col, row, 0);
                data.push(Token::new(span, TokenType::Unknown))
            }
            _e => {}
        }

        stream.advance(1);
    }

    data.push(Token::new(Span::new(col, row, 0), TokenType::EOF));
    Tokens::new(data)
}

enum Error {
    UnknownToken,
    InvalidDigits,
}

enum State {
    Yield,
    Error(Error),
    Consume(usize),
    Produce(usize, TokenType),
}

fn unknown_lexer(_: &mut Stream<'_>) -> State {
    State::Error(Error::UnknownToken)
}

fn whitespace_lexer(stream: &mut Stream<'_>) -> State {
    let c = stream.current();
    if !c.is_ascii_whitespace() {
        return State::Yield;
    }

    let mut skip = 0usize;
    for c in stream {
        if c.is_ascii_whitespace() {
            skip += 1;
        } else {
            break;
        }
    }

    State::Consume(skip.saturating_sub(1))
}

fn symbol_lexer(stream: &mut Stream<'_>) -> State {
    fn is_symbol(c: char) -> bool {
        match c as u8 {
            b'\''..=b'/' | b':'..=b'>' | b'@' | b'['..=b'^' | b'{'..=b'}' => true,
            _ => false,
        }
    }

    fn is_compound_sumbol(l: char, r: char) -> bool {
        match (l as u8, r as u8) {
            (b'<', b'>') | (b'<', b'=') | (b'>', b'=') | (b':', b'=') | (b'.', b'.') => true,
            _ => false,
        }
    }

    let c = stream.current();
    let s = if is_symbol(c) {
        let mut v = vec![c as u8];
        if let Some(r) = stream.at(stream.pos() + 1) {
            if is_compound_sumbol(c, r) {
                v.push(r as u8)
            }
        }
        v
    } else {
        stream
            .take_while(|&c| c.is_ascii_alphabetic())
            .map(|c| c as u8)
            .collect::<Vec<_>>()
    };

    let s = ::std::str::from_utf8(&s).expect("valid utf-8");
    if let Some(sym) = token::TokenType::try_parse(s) {
        return State::Produce(s.len() - 1, sym);
    }

    State::Yield
}

fn identifier_lexer(stream: &mut Stream<'_>) -> State {
    let input = stream
        .take_while(char::is_ascii_alphanumeric)
        .collect::<String>();
    if input.is_empty() {
        return State::Yield;
    }

    State::Produce(input.len() - 1, TokenType::Identifier(input))
}

fn number_lexer(stream: &mut Stream<'_>) -> State {
    use std::str::FromStr;

    // TODO determine if we want to determine signed-ness here or not
    if !stream.current().is_ascii_digit() {
        return State::Yield;
    }

    let mut is_f = false;
    let mut s = String::new();

    #[allow(clippy::while_let_loop)]
    loop {
        let c = match stream.peek() {
            Some(c) => c,
            None => break,
        };

        match c {
            c if c.is_ascii_digit() => s.push(c),
            'e' | 'E' | '.' => {
                is_f = true;
                s.push(c)
            }
            '+' | '-' => {
                if let Some(n) = stream.at(stream.pos() + 1) {
                    if is_f && n.is_ascii_digit() {
                        s.push(c);
                    } else {
                        break;
                    }
                }
            }
            _ => break,
        }
        stream.advance(1)
    }

    if let Ok(n) = i64::from_str(&s) {
        return State::Produce(s.len() - 1, TokenType::Integer(n));
    }

    if let Ok(n) = f64::from_str(&s) {
        return State::Produce(s.len() - 1, TokenType::Real(n));
    }

    State::Error(Error::InvalidDigits)
}

fn string_lexer(stream: &mut Stream<'_>) -> State {
    if '\'' != stream.current() {
        return State::Yield;
    }

    // I don't think standard pascal has "embedded strings"
    let input = stream
        .skip(1)
        .take_while(|&c| (c as u8) != b'\'')
        .collect::<String>();
    State::Produce(input.len() + 1, TokenType::String(input))
}

fn comment_lexer(stream: &mut Stream<'_>) -> State {
    let c = stream.current();
    match c {
        '{' | '(' => {}
        _ => return State::Yield,
    };
    stream.advance(1);
    let next = match stream.next() {
        Some(next) => next,
        None => return State::Yield,
    };

    if next == '}' {
        return State::Produce(2, TokenType::Comment(0, 2));
    }

    if next == '*' && c == '(' {
        // stream.advance(1);
        let mut skip = 2;
        while let Some(p) = stream.next() {
            if p == '*' {
                if let Some(')') = stream.peek() {
                    skip += 1;
                    break;
                }
            }
            skip += 1;
        }
        return State::Produce(skip, TokenType::Comment(0, skip));
    }

    if c == '{' {
        let mut skip = 1;
        for p in stream {
            skip += 1;
            if p == '}' {
                break;
            }
        }
        return State::Produce(skip, TokenType::Comment(0, skip));
    }

    State::Yield
}

fn type_lexer(stream: &mut Stream<'_>) -> State {
    const TYPES: &[(&str, Type); 4] = &[
        ("integer", Type::Integer),
        ("string", Type::String),
        ("bool", Type::Boolean),
        ("real", Type::Real),
    ];
    let c = stream.current();
    if !c.is_ascii_alphabetic() {
        return State::Yield;
    }
    let mut name = stream
        .take_while(|&c| c.is_ascii_alphabetic())
        .collect::<String>();

    if name.is_empty() {
        return State::Yield;
    }

    name.make_ascii_lowercase();
    let skip = name.len() - 1;
    for ty in TYPES {
        if ty.0 == name {
            return State::Produce(skip, TokenType::TypeName(ty.1));
        }
    }

    State::Yield
}

fn directive_lexer(_stream: &mut Stream<'_>) -> State {
    // TODO this
    State::Yield
}
