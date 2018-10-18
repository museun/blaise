use super::*;
use std::collections::HashMap;
use std::io::prelude::*;
use std::io::Result as IoResult;

#[derive(Debug, Clone)]
pub struct Scope {
    name: String,
    vars: HashMap<String, Object>,
    parent: Option<Box<Scope>>,
}

impl Scope {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            vars: HashMap::new(),
            parent: None,
        }
    }

    #[allow(dead_code)]
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn with_parent(name: impl Into<String>, parent: Scope) -> Scope {
        Scope {
            name: name.into(),
            vars: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn parent(self) -> Option<Scope> {
        // clone the parent
        if let Some(s) = self.parent {
            return Some(*s);
        }
        None
    }

    pub fn get(&mut self, name: impl AsRef<str>) -> Option<&Object> {
        let name = name.as_ref();
        if let Some(object) = self.vars.get(name) {
            return Some(object);
        };

        if let Some(ref mut scope) = self.parent {
            return scope.get(name);
        }
        None
    }

    pub fn set(&mut self, name: impl Into<String>, object: impl Into<Object>) {
        self.vars.insert(name.into(), object.into());
    }

    pub fn dump<W: Write>(&self, w: &mut W) -> IoResult<()> {
        self.print(0, w)
    }

    fn print<W: Write>(&self, depth: usize, w: &mut W) -> IoResult<()> {
        let pad = std::iter::repeat(" ").take(depth).collect::<String>();
        writeln!(w, "{}Scope: {}", pad, self.name)?;
        if let Some(ref parent) = self.parent {
            parent.print(depth + 4, w)?;
        }

        let depth = depth + 4;
        let pad = std::iter::repeat(" ").take(depth).collect::<String>();

        let mut output = vec![vec![]; 6];
        for (name, obj) in &self.vars {
            let v = match obj {
                Object::Unit => &mut output[5],
                Object::Primitive(_) => &mut output[4],
                Object::Procedure(_, _, _) => &mut output[2],
                Object::Function(_, _, _, _) => &mut output[3],
                Object::Variable(_, _) => &mut output[1],
                Object::Builtin(_) => &mut output[0],
            };
            v.push(format!("{}{} -> {}", pad, name, obj));
        }

        for (n, col) in output.iter().enumerate() {
            for s in col {
                writeln!(w, "{}", s)?
            }
            if !col.is_empty() && n != output.len() - 1 {
                writeln!(w)?;
            }
        }
        Ok(())
    }
}
