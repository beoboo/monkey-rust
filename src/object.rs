use std::any::Any;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Deref;

use crate::ast::*;
use crate::environment::Environment;
use crate::token::{Token, TokenType};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ObjectType {
    Array,
    Boolean,
    Builtin,
    Error,
    Function,
    Macro,
    Map,
    Integer,
    Null,
    Quote,
    ReturnValue,
    String,
}

pub const TRUE: Boolean = Boolean { value: true };
pub const FALSE: Boolean = Boolean { value: false };
pub const NULL: Null = Null {};

type BuiltinFunction = fn(Vec<Box<dyn Object>>) -> Option<Box<dyn Object>>;

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

pub trait Object: BaseObject {
    fn get_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
    fn eq(&self, other: &dyn Object) -> bool;
    fn as_any(&self) -> &dyn Any;
    fn to_node(&self) -> Box<dyn Node>;
    fn map_key(&self) -> Option<MapKey>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MapKey {
    pub object_type: ObjectType,
    pub value: u64,
}

#[derive(Debug, Clone)]
pub struct Pair {
    pub key: Box<dyn Object>,
    pub value: Box<dyn Object>,
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

    fn to_node(&self) -> Box<dyn Node> {
        Box::new(IntegerLiteral {
            token: Token::new(TokenType::Integer, format!("{}", self.value).as_str()),
            value: self.value,
        })
    }

    fn map_key(&self) -> Option<MapKey> {
        Some(MapKey { object_type: ObjectType::Integer, value: self.value as u64 })
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

    fn to_node(&self) -> Box<dyn Node> {
        Box::new(BooleanLiteral {
            token: Token::new(if self.value { TokenType::True } else { TokenType::False }, format!("{}", self.value).as_str()),
            value: self.value,
        })
    }

    fn map_key(&self) -> Option<MapKey> {
        Some(MapKey { object_type: ObjectType::String, value: if self.value { 1 } else { 0 } })
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

    fn to_node(&self) -> Box<dyn Node> {
        Box::new(StringLiteral {
            token: Token::new(TokenType::String, self.value.as_str()),
            value: self.value.clone(),
        })
    }

    fn map_key(&self) -> Option<MapKey> {
        let mut hasher = DefaultHasher::new();
        self.value.hash(&mut hasher);
        let hash = hasher.finish();

        Some(MapKey { object_type: ObjectType::String, value: hash })
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

    fn to_node(&self) -> Box<dyn Node> {
        panic!("Not implemented")
    }

    fn map_key(&self) -> Option<MapKey> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct ReturnValue {
    pub value: Box<dyn Object>,
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

    fn to_node(&self) -> Box<dyn Node> {
        panic!("Not implemented")
    }

    fn map_key(&self) -> Option<MapKey> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
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

    fn to_node(&self) -> Box<dyn Node> {
        panic!("Not implemented")
    }

    fn map_key(&self) -> Option<MapKey> {
        None
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
                        return false;
                    }
                }

                self.body.to_string() == other.body.to_string()
            }
            _ => false
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_node(&self) -> Box<dyn Node> {
        panic!("Not implemented")
    }

    fn map_key(&self) -> Option<MapKey> {
        None
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

    fn to_node(&self) -> Box<dyn Node> {
        panic!("Not implemented")
    }

    fn map_key(&self) -> Option<MapKey> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct Array {
    pub elements: Vec<Box<dyn Object>>,
}

impl Object for Array {
    fn get_type(&self) -> ObjectType {
        ObjectType::Array
    }

    fn inspect(&self) -> String {
        let mut elements: Vec<String> = vec![];
        for e in &self.elements {
            elements.push(format!("{}", e.inspect()))
        }
        format!("[{}]", elements.join(", "))
    }

    fn eq(&self, _other: &dyn Object) -> bool {
        false
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_node(&self) -> Box<dyn Node> {
        panic!("Not implemented")
    }

    fn map_key(&self) -> Option<MapKey> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct Map {
    pub pairs: HashMap<MapKey, Pair>,
}

impl Object for Map {
    fn get_type(&self) -> ObjectType {
        ObjectType::Map
    }

    fn inspect(&self) -> String {
        let mut pairs: Vec<String> = vec![];
        for pair in self.pairs.values() {
            pairs.push(format!("{}: {}", pair.key.inspect(), pair.value.inspect()))
        }
        format!("{{{}}}", pairs.join(", "))
    }

    fn eq(&self, _other: &dyn Object) -> bool {
        false
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_node(&self) -> Box<dyn Node> {
        panic!("Not implemented")
    }

    fn map_key(&self) -> Option<MapKey> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct Quote {
    pub node: Option<Box<dyn Node>>,
}

impl Object for Quote {
    fn get_type(&self) -> ObjectType {
        ObjectType::Quote
    }

    fn inspect(&self) -> String {
        format!("QUOTE({})", match &self.node {
            Some(node) => node.to_string(),
            None => "".to_string()
        })
    }

    fn eq(&self, _other: &dyn Object) -> bool {
        false
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_node(&self) -> Box<dyn Node> {
        match self.node.clone() {
            Some(node) => node,
            None => panic!("Invalid"),
        }
    }

    fn map_key(&self) -> Option<MapKey> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct Macro {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl Object for Macro {
    fn get_type(&self) -> ObjectType {
        ObjectType::Macro
    }

    fn inspect(&self) -> String {
        let mut params: Vec<String> = vec![];
        for p in &self.parameters {
            params.push(format!("{}", p))
        }
        format!("fn ({}) {{\n{}\n}}", params.join(", "), self.body)
    }

    fn eq(&self, other: &dyn Object) -> bool {
        match other.as_any().downcast_ref::<Macro>() {
            Some(other) => {
                for (idx, p) in self.parameters.iter().enumerate() {
                    if p.value != other.parameters[idx].value {
                        return false;
                    }
                }

                self.body.to_string() == other.body.to_string()
            }
            _ => false
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_node(&self) -> Box<dyn Node> {
        panic!("Not implemented")
    }

    fn map_key(&self) -> Option<MapKey> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strings() {
        let hello1 = StringE { value: "hello world".to_string() };
        let hello2 = StringE { value: "hello world".to_string() };

        let diff1 = StringE { value: "another world".to_string() };
        let diff2 = StringE { value: "another world".to_string() };

        assert_eq!(hello1.map_key(), hello2.map_key());
        assert_eq!(diff1.map_key(), diff2.map_key());
        assert_ne!(hello1.map_key(), diff1.map_key());
    }

    #[test]
    fn test_integers() {
        let int1 = Integer { value: 1 };
        let int2 = Integer { value: 1 };

        let diff1 = Integer { value: 2 };
        let diff2 = Integer { value: 2 };

        assert_eq!(int1.map_key(), int2.map_key());
        assert_eq!(diff1.map_key(), diff2.map_key());
        assert_ne!(int1.map_key(), diff1.map_key());
    }

    #[test]
    fn test_booleans() {
        let bool1 = Boolean { value: true };
        let bool2 = Boolean { value: true };

        let diff1 = Boolean { value: false };
        let diff2 = Boolean { value: false };

        assert_eq!(bool1.map_key(), bool2.map_key());
        assert_eq!(diff1.map_key(), diff2.map_key());
        assert_ne!(bool1.map_key(), diff1.map_key());
    }
}