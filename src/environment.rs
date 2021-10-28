use std::collections::HashMap;

use crate::object::Object;

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Box<dyn Object>>,
    parent: Option<Box<Self>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            parent: None,
        }
    }

    pub fn enclose(parent: Self) -> Self {
        Self {
            store: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn get(&self, key: &String) -> Option<&Box<dyn Object>> {
        println!("Getting {}", key);
        match self.store.get(key) {
            Some(key) => Some(key),
            None => match &self.parent {
                Some(parent) => parent.get(key),
                None => None
            }
        }
    }

    pub fn set(&mut self, key: String, val: Box<dyn Object>) -> Option<Box<dyn Object>> {
        println!("Setting {}", key);
        self.store.insert(key, val)
    }
}