use std::collections::HashMap;
use crate::object::Object;

pub struct Environment {
    store: HashMap<String, Box<dyn Object>>
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new()
        }
    }

    pub fn get(&self, key: &String) -> Option<&Box<dyn Object>> {
        println!("Getting {}", key);
        self.store.get(key)
    }

    pub fn set(&mut self, key: String, val: Box<dyn Object>) -> Option<Box<dyn Object>> {
        println!("Setting {}", key);
        self.store.insert(key, val)
    }
}