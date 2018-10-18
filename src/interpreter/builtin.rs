use super::*;

use std::io::prelude::*;
use std::io::{stdin, stdout};
use std::str;

// builtins
// TODO use a io::Read and io::Write as a backend
pub(crate) fn write(objects: &[Object]) -> Result<Object> {
    for obj in objects {
        match obj {
            Object::Primitive(p) => print!("{}", p),
            _ => return Err(Error::InvalidArgument),
        }
    }
    stdout().flush().expect("flush");
    Ok(Object::Unit)
}

pub(crate) fn writeln(objects: &[Object]) -> Result<Object> {
    for obj in objects {
        match obj {
            Object::Primitive(p) => println!("{}", p),
            _ => return Err(Error::InvalidArgument),
        }
    }
    if objects.is_empty() {
        println!();
    }

    stdout().flush().expect("flush");
    Ok(Object::Unit)
}

pub(crate) fn read(objects: &[Object]) -> Result<Vec<(&String, Object)>> {
    // we won't work on strings (but it will)
    let mut out = vec![];
    macro_rules! parse {
        ($e:expr) => {
            match $e {
                Ok(s) => s.into(),
                Err(_e) => continue,
            }
        };
    }

    let mut data = stdin().bytes();

    for object in objects {
        let mut buf = vec![];
        let (name, ty) = match object {
            Object::Variable(name, ty, _) => (name, ty),
            e => panic!("{:#?}", e),
        };

        while let Some(data) = data.next() {
            let data = data.map_err(|_| Error::CannotRead)?;
            buf.push(data);
            let s = match str::from_utf8(&buf) {
                Ok(s) => s,
                Err(_e) => continue,
            };

            out.push((
                name,
                match ty {
                    Type::String => parse!(s.parse::<String>()),
                    Type::Integer => parse!(s.parse::<i64>()),
                    Type::Real => parse!(s.parse::<f64>()),
                    Type::Boolean => parse!(s.parse::<bool>()),
                    e => unimplemented!("{:#?}", e),
                },
            ));

            buf.clear();
            break;
        }
    }

    if out.len() != objects.len() {
        panic!("{:#?} != {:#?}", out, objects)
    }

    Ok(out)
}

pub(crate) fn readln(objects: &[Object]) -> Result<Vec<(&String, Object)>> {
    let mut out = vec![];

    macro_rules! parse {
        ($e:expr) => {
            $e.map_err(|e| panic!("{:#?}", e))
                .and_then(|s| Ok(s.into()))
        };
    }

    for object in objects {
        loop {
            let mut buf = ::std::string::String::new();
            let _data = stdin().read_line(&mut buf).map_err(|e| {
                debug!("cannot read stdin: {}", e);
                Error::CannotRead
            })?;

            let buf = buf.trim_right_matches("\r\n");
            if buf.is_empty() {
                continue;
            }

            debug!("object: {:?}", object);
            if let Object::Variable(name, ty, _) = object {
                out.push((
                    name,
                    match ty {
                        Type::String => parse!(buf.parse::<String>())?,
                        Type::Integer => parse!(buf.parse::<i64>())?,
                        Type::Real => parse!(buf.parse::<f64>())?,
                        Type::Boolean => parse!(buf.parse::<bool>())?,
                        e => unimplemented!("{:#?}", e),
                    },
                ));
                break;
            }
            unreachable!("{:#?}", object)
        }
    }

    Ok(out)
}
