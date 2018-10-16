use super::*;
use std::collections::HashMap;

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
}
