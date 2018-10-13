use super::span::Span;
use super::stream::Stream;
use super::tokens::{self, Token, Tokens};

pub struct Lexer;
impl Lexer {
    pub fn scan<'a>(file: &'a str, input: &str) -> Tokens<'a> {
        let lexers = [
            whitespace_lexer,
            directive_lexer,
            comment_lexer,
            number_lexer,
            string_lexer,
            special_lexer,
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

            let span = Span::new(file, col, row);
            let mut error = None;
            'inner: for lexer in &lexers {
                match lexer(&mut stream.clone()) {
                    State::Yield => continue,
                    State::Error(err) => error = Some(err),
                    State::Consume(n) => skip += n,
                    State::Produce(n, Token::Comment(_, end)) => {
                        skip += n;
                        data.push((span, Token::Comment(row, row + end)))
                    }
                    State::Produce(n, token) => {
                        skip += n;
                        data.push((span, token))
                    }
                }
                break 'inner;
            }

            match error {
                Some(Error::UnknownToken) => data.push((span, Token::Unknown)),
                _e => {}
            }

            stream.advance(1);
        }

        data.push((Span::new(file, col, row), Token::EOF));
        Tokens::new(data)
    }
}

enum Error {
    UnknownToken,
}

enum State {
    Yield,
    Error(Error),
    Consume(usize),
    Produce(usize, Token),
}

fn unknown_lexer(_: &mut Stream) -> State {
    State::Error(Error::UnknownToken)
}

fn whitespace_lexer(stream: &mut Stream) -> State {
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

    State::Consume(skip.checked_sub(1).or_else(|| Some(0)).unwrap())
}

fn special_lexer(stream: &mut Stream) -> State {
    fn is_symbol(c: char) -> bool {
        match c as u8 {
            b'\''...b'/' | b':'...b'>' | b'@' | b'['...b'^' | b'{'...b'}' => true,
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
    let sym = tokens::Symbol::new(s);
    if sym.is_some() {
        return State::Produce(s.len() - 1, Token::Symbol(sym.unwrap()));
    }

    let res = tokens::Reserved::new(s);
    if res.is_some() {
        return State::Produce(s.len() - 1, Token::Reserved(res.unwrap()));
    }

    State::Yield
}

fn identifier_lexer(stream: &mut Stream) -> State {
    let input = stream
        .take_while(char::is_ascii_alphanumeric)
        .collect::<String>();
    if input.is_empty() {
        return State::Yield;
    }

    State::Produce(input.len() - 1, Token::Identifier(input))
}

fn number_lexer(stream: &mut Stream) -> State {
    if !stream.current().is_ascii_digit() {
        return State::Yield;
    }

    let mut skip = 0;
    let input = stream
        .take_while(char::is_ascii_digit)
        .filter_map(|c| c.to_digit(10))
        .inspect(|_| skip += 1)
        .fold(0i32, |a, n| 10 * a + (n as i32));

    State::Produce(skip - 1, Token::Number(input))
}

fn string_lexer(stream: &mut Stream) -> State {
    if '\'' != stream.current() {
        return State::Yield;
    }

    // I don't think standard pascal has "embedded strings"
    let input = stream
        .skip(1)
        .take_while(|&c| (c as u8) != b'\'')
        .collect::<String>();
    State::Produce(input.len() + 1, Token::String(input))
}

fn comment_lexer(stream: &mut Stream) -> State {
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
        return State::Produce(2, Token::Comment(0, 2));
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
        return State::Produce(skip, Token::Comment(0, skip));
    }

    if c == '{' {
        let mut skip = 1;
        for p in stream {
            skip += 1;
            if p == '}' {
                break;
            }
        }
        return State::Produce(skip, Token::Comment(0, skip));
    }

    State::Yield
}

fn type_lexer(stream: &mut Stream) -> State {
    const TYPES: &[(&str, tokens::Type); 3] = &[
        ("integer", tokens::Type::Integer),
        ("string", tokens::Type::String),
        ("bool", tokens::Type::Bool),
    ];
    let c = stream.current();
    if !c.is_ascii_alphabetic() {
        return State::Yield;
    }
    let name = stream
        .take_while(|&c| c.is_ascii_alphabetic())
        .collect::<String>();

    if name.is_empty() {
        return State::Yield;
    }

    let skip = name.len() - 1;
    for ty in TYPES {
        if ty.0 == name {
            return State::Produce(skip, Token::Type(ty.1));
        }
    }

    State::Yield
}

fn directive_lexer(_stream: &mut Stream) -> State {
    State::Yield
}
