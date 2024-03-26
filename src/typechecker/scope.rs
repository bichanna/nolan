use std::collections::HashMap;

#[derive(Debug)]
pub struct Scope<'a> {
    map: HashMap<&'a String, bool>,
}

impl<'a> Scope<'a> {
    pub fn new() -> Scope<'a> {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn set(&mut self, key: &'a String, val: bool) {
        self.map.insert(key, val);
    }

    pub fn contains(&self, key: &'a String) -> bool {
        self.map.contains_key(key)
    }
}
