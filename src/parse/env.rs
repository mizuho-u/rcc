use std::collections::HashMap;

pub struct Env<'a> {
    varmap: HashMap<String, String>,
    parent: Option<Box<&'a Env<'a>>>,
}

impl<'a> Env<'a> {
    pub fn new() -> Env<'a> {
        Env {
            varmap: HashMap::new(),
            parent: None,
        }
    }

    pub fn extend(&'a self) -> Env<'a> {
        Env {
            varmap: HashMap::new(),
            parent: Some(Box::new(self)),
        }
    }

    pub fn contains_current(&self, k: &String) -> bool {
        self.varmap.contains_key(k)
    }

    pub fn contains(&self, k: &String) -> bool {
        let mut b = self.varmap.contains_key(k);

        if let Some(p) = self.parent.as_ref() {
            b = p.contains(k);
        };

        b
    }

    pub fn get_current(&self, k: &String) -> Option<&String> {
        self.varmap.get(k)
    }

    pub fn get(&self, k: &String) -> Option<&String> {
        let v = self.get_current(k);
        if v.is_some() {
            return v;
        }

        if let Some(p) = &self.parent {
            p.get(k)
        } else {
            None
        }
    }

    pub fn set(&mut self, k: String, v: String) {
        self.varmap.insert(k, v);
    }
}
