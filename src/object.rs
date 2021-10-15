use std::any::Any;
use std::fmt::Debug;
use std::ops::Deref;

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Boolean,
    Error,
    Integer,
    Null,
    ReturnValue,
}

pub const TRUE : Boolean = Boolean{value: true};
pub const FALSE : Boolean = Boolean{value: false};
pub const NULL : Null = Null{};

// pub fn convert<'a, T: 'static + Sized>(obj: &'a Box<dyn Object>) -> Option<&'a T> {
//     (obj as &'a dyn Any).downcast_ref::<T>()
// }

pub trait Object: Debug {
    fn get_type(&self) -> ObjectType;
    fn inspect(&self) -> String;

    fn as_any(&self) -> &dyn Any;
    fn as_boxed_object(&self) -> Box<dyn Object>;
    fn eq(&self, other: &dyn Object) -> bool;
}

#[derive(Debug, Clone)]
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

    fn as_boxed_object(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }

    fn eq(&self, other: &dyn Object) -> bool {
        match other.as_any().downcast_ref::<Integer>() {
            Some(other) => self.value == other.value,
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
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

    fn as_boxed_object(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }

    fn eq(&self, other: &dyn Object) -> bool {
        match other.as_any().downcast_ref::<Boolean>() {
            Some(other) => self.value == other.value,
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
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

    fn as_boxed_object(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }

    fn eq(&self, other: &dyn Object) -> bool {
        other.get_type() == ObjectType::Null
    }
}

#[derive(Debug)]
pub struct ReturnValue {
    pub value: Box<dyn Object>
}

impl Object for ReturnValue {
    fn get_type(&self) -> ObjectType {
        ObjectType::ReturnValue
    }

    fn inspect(&self) -> String {
        self.value.inspect()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_boxed_object(&self) -> Box<dyn Object> {
        Box::new(ReturnValue{value: self.value.as_boxed_object()})
    }

    fn eq(&self, other: &dyn Object) -> bool {
        match other.as_any().downcast_ref::<ReturnValue>() {
            Some(other) => self.value.eq(other.value.deref()),
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String
}

impl Error {
    pub(crate) fn new(message: String) -> Self {
        Self {
            message
        }
    }
}

impl Object for Error {
    fn get_type(&self) -> ObjectType {
        ObjectType::Error
    }

    fn inspect(&self) -> String {
        format!("Error: {}", self.message)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_boxed_object(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }

    fn eq(&self, other: &dyn Object) -> bool {
        match other.as_any().downcast_ref::<Error>() {
            Some(other) => self.message.eq(other.message.deref()),
            _ => false
        }
    }
}

pub fn is_error(obj: &Option<Box<dyn Object>>) -> bool {
    match obj {
        Some(obj) => obj.get_type() == ObjectType::Error,
        None => true
    }
}