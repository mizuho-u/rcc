use std::collections::HashMap;

pub struct Env<'a, T> {
    idmap: HashMap<String, T>,
    parent: Option<Box<&'a Env<'a, T>>>,
}

pub struct Entry {
    pub new_name: String,
    pub has_linkage: bool,
}

impl<'a, T> Env<'a, T> {
    pub fn new() -> Env<'a, T> {
        Env {
            idmap: HashMap::new(),
            parent: None,
        }
    }

    pub fn extend(&'a self) -> Env<'a, T> {
        Env {
            idmap: HashMap::new(),
            parent: Some(Box::new(self)),
        }
    }

    pub fn contains_current(&self, k: &String) -> bool {
        self.idmap.contains_key(k)
    }

    pub fn contains(&self, k: &String) -> bool {
        let mut b = self.idmap.contains_key(k);

        if let Some(p) = self.parent.as_ref() {
            b = p.contains(k);
        };

        b
    }

    pub fn get_current(&self, k: &String) -> Option<&T> {
        self.idmap.get(k)
    }

    pub fn get(&self, k: &String) -> Option<&T> {
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

    pub fn set(&mut self, k: String, v: T) {
        self.idmap.insert(k, v);
    }
}
