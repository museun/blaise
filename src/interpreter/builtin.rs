use super::object::Primitive::*;
use super::*;

use std::io::prelude::*;
use std::io::{stdin, stdout};

// builtins
// TODO use a io::Read and io::Write as a backend
pub(crate) fn write(data: Object) -> Result<Object, Error> {
    match data {
        Object::Primitive(Integer(n)) => print!("{}", n),
        Object::Primitive(String(s)) => print!("{}", s),
        Object::Primitive(Boolean(b)) => print!("{}", b),
        Object::Primitive(Real(r)) => print!("{}", r),
        _ => return Err(Error::InvalidArgument),
    }

    stdout().flush().expect("flush");
    Ok(Object::Unit)
}

pub(crate) fn writeln(data: Object) -> Result<Object, Error> {
    match data {
        Object::Primitive(Integer(n)) => println!("{}", n),
        Object::Primitive(String(s)) => println!("{}", s),
        Object::Primitive(Boolean(b)) => println!("{}", b),
        Object::Primitive(Real(r)) => println!("{}", r),
        _ => return Err(Error::InvalidArgument),
    }

    stdout().flush().expect("flush");
    Ok(Object::Unit)
}

pub(crate) fn readln() -> Result<Object, Error> {
    let mut buf = ::std::string::String::new();
    stdin()
        .read_line(&mut buf)
        .map_err(|e| {
            debug!("cannot read stdin: {}", e);
            Error::CannotRead
        })
        .and_then(|_| Ok(Object::Primitive(String(buf))))
}
