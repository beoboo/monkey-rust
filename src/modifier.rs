use crate::ast::*;

use intertrait::cast::*;

pub type ModifierFn<'a> = dyn FnMut(Box<dyn Node>) -> Box<dyn Node> + 'a;

pub struct ModifierImpl<'a> {
    pub modifier_fn: &'a mut ModifierFn<'a>,
}

pub trait Modifier {
    fn modify(&mut self, node: Box<dyn Node>) -> Box<dyn Node>;

    fn modify_expression(&mut self, expr: Box<dyn Expression>) -> Box<dyn Expression> {
        let node = self.modify(expr.clone_node());
        node.cast::<dyn Expression>().unwrap().clone() as Box<dyn Expression>
    }

    fn modify_statement(&mut self, stmt: &Box<dyn Statement>) -> Box<dyn Statement> {
        let node = self.modify(stmt.clone_node());
        node.cast::<dyn Statement>().unwrap().clone() as Box<dyn Statement>
    }

    fn modify_block_statement(&mut self, block: BlockStatement) -> BlockStatement {
        let node = self.modify(block.clone_node());
        node.as_any().downcast_ref::<BlockStatement>().unwrap().clone()
    }

    fn modify_identifier(&mut self, identifier: &Identifier) -> Identifier {
        let node = self.modify(identifier.clone_node());
        node.as_any().downcast_ref::<Identifier>().unwrap().clone()
    }

    fn apply(&mut self, node: Box<dyn Node>) -> Box<dyn Node>;
}

impl<'a> Modifier for ModifierImpl<'a> {
    fn modify(&mut self, node: Box<dyn Node>) -> Box<dyn Node> {
        println!("Modifying: {:?}", node);

        node.modify(self)
    }

    fn apply(&mut self, node: Box<dyn Node>) -> Box<dyn Node> {
        println!("Applying: {:?}", node);
        (self.modifier_fn)(node.clone())
    }
}