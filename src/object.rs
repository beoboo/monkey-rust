use std::any::Any;
use std::fmt::Debug;

#[derive(PartialEq)]
pub enum ObjectType {
    Boolean,
    Integer,
    Null,
}

pub const TRUE : Boolean = Boolean{value: true};
pub const FALSE : Boolean = Boolean{value: false};
pub const NULL : Null = Null{};

pub trait Object: Debug {
    fn get_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
    fn as_any(&self) -> &dyn Any;
    fn eq(&self, other: &dyn Object) -> bool;
}

#[derive(Debug)]
pub struct Integer {
    pub value: i64,
}

impl Object for Integer {
    fn get_type(&self) -> ObjectType {
        ObjectType::Integer
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eq(&self, other: &dyn Object) -> bool {
        match other.as_any().downcast_ref::<Integer>() {
            Some(other) => self.value == other.value,
            _ => false
        }
    }
}

#[derive(Debug)]
pub struct Boolean {
    pub value: bool,
}

impl Object for Boolean {
    fn get_type(&self) -> ObjectType {
        ObjectType::Boolean
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eq(&self, other: &dyn Object) -> bool {
        match other.as_any().downcast_ref::<Boolean>() {
            Some(other) => self.value == other.value,
            _ => false
        }
    }
}

#[derive(Debug)]
pub struct Null {}

impl Object for Null {
    fn get_type(&self) -> ObjectType {
        ObjectType::Null
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eq(&self, other: &dyn Object) -> bool {
        other.get_type() == ObjectType::Null
    }
}