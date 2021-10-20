use std::any::Any;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use crate::ast::{Identifier, BlockStatement};
use crate::environment::Environment;

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Boolean,
    Builtin,
    Error,
    Function,
    Integer,
    Null,
    ReturnValue,
    String,
}

pub const TRUE : Boolean = Boolean{value: true};
pub const FALSE : Boolean = Boolean{value: false};
pub const NULL : Null = Null{};

type BuiltinFunction = fn(Vec<Box<dyn Object>>) -> Option<Box<dyn Object>>;


// pub fn convert<'a, T: 'static + Sized>(obj: &'a Box<dyn Object>) -> Option<&'a T> {
//     (obj as &'a dyn Any).downcast_ref::<T>()
// }

pub trait BaseObject {
    fn clone_box(&self) -> Box<dyn Object>;
}

impl<T> BaseObject for T
    where T: 'static + Object + Clone + Debug {
    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Object> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

impl Debug for Box<dyn Object> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub trait Object : BaseObject {
    fn get_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
    fn eq(&self, other: &dyn Object) -> bool;
    fn as_any(&self) -> &dyn Any;
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

    fn eq(&self, other: &dyn Object) -> bool {
        match other.as_any().downcast_ref::<Integer>() {
            Some(other) => self.value == other.value,
            _ => false
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
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

    fn eq(&self, other: &dyn Object) -> bool {
        match other.as_any().downcast_ref::<Boolean>() {
            Some(other) => self.value == other.value,
            _ => false
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct StringE {
    pub value: String,
}

impl Object for StringE {
    fn get_type(&self) -> ObjectType {
        ObjectType::String
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn eq(&self, other: &dyn Object) -> bool {
        match other.as_any().downcast_ref::<StringE>() {
            Some(other) => self.value == other.value,
            _ => false
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
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

    fn eq(&self, other: &dyn Object) -> bool {
        other.get_type() == ObjectType::Null
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
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

    fn eq(&self, other: &dyn Object) -> bool {
        match other.as_any().downcast_ref::<ReturnValue>() {
            Some(other) => self.value.eq(other.value.deref()),
            _ => false
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String
}

impl Error {
    pub fn new(message: String) -> Self {
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

    fn eq(&self, other: &dyn Object) -> bool {
        match other.as_any().downcast_ref::<Error>() {
            Some(other) => self.message.eq(other.message.deref()),
            _ => false
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl Object for Function {
    fn get_type(&self) -> ObjectType {
        ObjectType::Function
    }

    fn inspect(&self) -> String {
        let mut params: Vec<String> = vec![];
        for p in &self.parameters {
            params.push(format!("{}", p))
        }
        format!("fn ({}) {{\n{}\n}}", params.join(", "), self.body)
    }

    fn eq(&self, other: &dyn Object) -> bool {
        match other.as_any().downcast_ref::<Function>() {
            Some(other) => {
                for (idx, p) in self.parameters.iter().enumerate() {
                    if p.value != other.parameters[idx].value {
                        return false
                    }
                }

                self.body.to_string() == other.body.to_string()
            },
            _ => false
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct Builtin {
    pub function: BuiltinFunction,
}

impl Object for Builtin {
    fn get_type(&self) -> ObjectType {
        ObjectType::Builtin
    }

    fn inspect(&self) -> String {
        "builtin function".to_string()
    }

    fn eq(&self, _other: &dyn Object) -> bool {
        false
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub fn is_error(obj: &Option<Box<dyn Object>>) -> bool {
    match obj {
        Some(obj) => obj.get_type() == ObjectType::Error,
        None => true
    }
}