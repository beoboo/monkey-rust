use crate::object::*;
use crate::ast::{Statement, LetStatement, MacroLiteral};

pub fn is_error(obj: &Option<Box<dyn Object>>) -> bool {
    match obj {
        Some(obj) => obj.get_type() == ObjectType::Error,
        None => true
    }
}

pub fn is_macro_definition(node: &Box<dyn Statement>) -> bool {
    let let_statement = match node.as_any().downcast_ref::<LetStatement>() {
        Some(stmt) => stmt,
        None => return false
    };

    let_statement.value.as_any().downcast_ref::<MacroLiteral>().is_some()
}
