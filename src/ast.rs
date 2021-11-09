use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};

use intertrait::*;

use crate::environment::Environment;
use crate::evaluator::Evaluator;
use crate::object::{Array, Function, Integer, Object, ReturnValue, StringE};
use crate::token::Token;
use crate::utils::is_error;
use crate::modifier::Modifier;
use crate::compiler::{Compiler, CompilerResult};
use crate::op_code::OpCode;

pub trait BaseNode: Any {
    fn clone_node(&self) -> Box<dyn Node>;
}

pub trait BaseExpression {
    fn clone_expression(&self) -> Box<dyn Expression>;
}

pub trait BaseStatement {
    fn clone_statement(&self) -> Box<dyn Statement>;
}

impl<T: 'static + Node + Clone> BaseNode for T {
    fn clone_node(&self) -> Box<dyn Node> {
        Box::new(self.clone())
    }
}

impl<T: 'static + Expression + Clone> BaseExpression for T {
    fn clone_expression(&self) -> Box<dyn Expression> {
        Box::new(self.clone())
    }
}

impl<T: 'static + Statement + Clone> BaseStatement for T {
    fn clone_statement(&self) -> Box<dyn Statement> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Node> {
    fn clone(&self) -> Box<dyn Node> {
        self.clone_node()
    }
}

impl Clone for Box<dyn Expression> {
    fn clone(&self) -> Box<dyn Expression> {
        self.clone_expression()
    }
}

impl Clone for Box<dyn Statement> {
    fn clone(&self) -> Box<dyn Statement> {
        self.clone_statement()
    }
}

impl PartialEq<Self> for dyn Expression {
    fn eq(&self, other: &Self) -> bool {
        return self.to_string() == other.to_string();
    }
}

impl Eq for dyn Expression {}

impl Hash for dyn Expression {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state)
    }
}

pub trait Node: BaseNode + Debug + Display + CastFrom {
    fn token_literal(&self) -> &str;
    fn as_any(&self) -> &dyn Any;
    fn as_node(&self) -> &dyn Node;
    fn eval(&self, evaluator: &Evaluator, env: &mut Environment) -> Option<Box<dyn Object>>;
    fn compile(&self, compiler: &mut Compiler) -> CompilerResult;
    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node>;
}

pub trait Statement: BaseStatement + Node {}

pub trait Expression: BaseExpression + Node {
    fn value(&self) -> String;
}

#[derive(Clone, Debug)]
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> &str {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, evaluator: &Evaluator, env: &mut Environment) -> Option<Box<dyn Object>> {
        evaluator.eval_program(self, env)
    }

    fn compile(&self, compiler: &mut Compiler) -> CompilerResult {
        compiler.compile_statements(&self.statements)
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        let mut node = self.clone();
        let statements = node.statements.clone();
        for (i, statement) in statements.iter().enumerate() {
            node.statements[i] = modifier.modify_statement(statement);
        }

        modifier.apply(node.clone_node())
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }

        Ok(())
    }
}

// Statements
#[derive(Clone, Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, evaluator: &Evaluator, env: &mut Environment) -> Option<Box<dyn Object>> {
        let evaluated = evaluator.eval(Box::new(self.value.as_node()), env);
        if is_error(&evaluated) {
            return evaluated;
        }

        let evaluated = evaluated.unwrap();

        env.set(self.name.value.clone(), evaluated);

        None
    }

    fn compile(&self, compiler: &mut Compiler) -> CompilerResult {
        compiler.compile_let_statement(self)
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        let mut node = self.clone();
        node.value = modifier.modify_expression(node.value);

        modifier.apply(node.clone_node())
    }
}

#[cast_to]
impl Statement for LetStatement {}


impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} = {};", self.token_literal(), self.name, self.value)
    }
}

#[derive(Clone, Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Box<dyn Expression>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, evaluator: &Evaluator, env: &mut Environment) -> Option<Box<dyn Object>> {
        let evaluated = evaluator.eval(Box::new(self.return_value.as_node()), env);
        if is_error(&evaluated) {
            return evaluated;
        }

        Some(Box::new(ReturnValue { value: evaluated.unwrap() }))
    }

    fn compile(&self, _compiler: &mut Compiler) -> CompilerResult {
        todo!()
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        let mut node = self.clone();
        node.return_value = modifier.modify_expression(node.return_value);

        modifier.apply(node.clone_node())
    }
}

#[cast_to]
impl Statement for ReturnStatement {}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {};", self.token_literal(), self.return_value)
    }
}

#[derive(Clone, Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Box<dyn Expression>,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, evaluator: &Evaluator, env: &mut Environment) -> Option<Box<dyn Object>> {
        evaluator.eval(Box::new(self.expression.as_node()), env)
    }

    fn compile(&self, compiler: &mut Compiler) -> CompilerResult {
        compiler.compile(self.expression.clone_node())?;
        compiler.emit(OpCode::OpPop, vec![]);

        Ok(())
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        let mut node = self.clone();
        node.expression = modifier.modify_expression(node.expression);

        modifier.apply(node.clone_node())
    }
}

#[cast_to]
impl Statement for ExpressionStatement {}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression)
    }
}

#[derive(Clone, Debug)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, evaluator: &Evaluator, env: &mut Environment) -> Option<Box<dyn Object>> {
        evaluator.eval_block_statement(self, env)
    }

    fn compile(&self, compiler: &mut Compiler) -> CompilerResult {
        compiler.compile_statements(&self.statements)
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        let mut node = self.clone();
        let statements = node.statements.clone();
        for (i, statement) in statements.iter().enumerate() {
            node.statements[i] = modifier.modify_statement(statement);
        }

        modifier.apply(node.clone_node())
    }
}

#[cast_to]
impl Statement for BlockStatement {}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, evaluator: &Evaluator, env: &mut Environment) -> Option<Box<dyn Object>> {
        evaluator.eval_identifier(self, env)
    }

    fn compile(&self, compiler: &mut Compiler) -> CompilerResult {
        compiler.compile_identifier(self)
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        modifier.apply(self.clone_node())
    }
}

#[cast_to]
impl Expression for Identifier {
    fn value(&self) -> String {
        self.value.clone()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone, Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, evaluator: &Evaluator, env: &mut Environment) -> Option<Box<dyn Object>> {
        let right = evaluator.eval(Box::new(self.right.as_node()), env);
        if is_error(&right) {
            return right;
        }

        evaluator.eval_prefix_expression(&self.operator, right.unwrap())
    }

    fn compile(&self, compiler: &mut Compiler) -> CompilerResult {
        compiler.compile(self.right.clone_node())?;

        match self.operator.as_str() {
            "!" => compiler.emit(OpCode::OpBang, vec![]),
            "-" => compiler.emit(OpCode::OpMinus, vec![]),
            _ => return Err(format!("Unknown operator: {}", self.operator))
        };

        Ok(())
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        let mut node = self.clone();
        node.right = modifier.modify_expression(node.right);

        modifier.apply(node.clone_node())
    }
}

#[cast_to]
impl Expression for PrefixExpression {
    fn value(&self) -> String {
        "prefix".to_string()
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Clone, Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, evaluator: &Evaluator, env: &mut Environment) -> Option<Box<dyn Object>> {
        let left = evaluator.eval(Box::new(self.left.as_node()), env);
        if is_error(&left) {
            return left;
        }

        let right = evaluator.eval(Box::new(self.right.as_node()), env);
        if is_error(&right) {
            return right;
        }

        evaluator.eval_infix_expression(&self.operator, left.unwrap(), right.unwrap())
    }

    fn compile(&self, compiler: &mut Compiler) -> CompilerResult {
        if self.operator == "<" {
            compiler.compile(self.right.clone_node())?;
            compiler.compile(self.left.clone_node())?;
            compiler.emit(OpCode::OpGreaterThan, vec![]);

            return Ok(())
        }

        compiler.compile(self.left.clone_node())?;
        compiler.compile(self.right.clone_node())?;

        match self.operator.as_str() {
            "+" => compiler.emit(OpCode::OpAdd, vec![]),
            "-" => compiler.emit(OpCode::OpSub, vec![]),
            "*" => compiler.emit(OpCode::OpMul, vec![]),
            "/" => compiler.emit(OpCode::OpDiv, vec![]),
            ">" => compiler.emit(OpCode::OpGreaterThan, vec![]),
            "==" => compiler.emit(OpCode::OpEqual, vec![]),
            "!=" => compiler.emit(OpCode::OpNotEqual, vec![]),
            _ => return Err(format!("Unknown operator: {}", self.operator))
        };

        Ok(())
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        let mut node = self.clone();
        node.left = modifier.modify_expression(node.left);
        node.right = modifier.modify_expression(node.right);

        modifier.apply(node.clone_node())
    }
}

#[cast_to]
impl Expression for InfixExpression {
    fn value(&self) -> String {
        "infix".to_string()
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Clone, Debug)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<dyn Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, evaluator: &Evaluator, env: &mut Environment) -> Option<Box<dyn Object>> {
        evaluator.eval_if_expression(self, env)
    }

    fn compile(&self, compiler: &mut Compiler) -> CompilerResult {
        compiler.compile(self.condition.clone_node())?;
        let jump_not_truthy_pos = compiler.emit(OpCode::OpJumpNotTruthy, vec![9999]);
        compiler.compile(self.consequence.clone_node())?;

        compiler.pop_last_pop_instruction();

        let jump_pos = compiler.emit(OpCode::OpJump, vec![9999]);
        let after_consequence_pos = compiler.instructions.len();
        compiler.change_operand(jump_not_truthy_pos, after_consequence_pos as u32);

        match &self.alternative {
            Some(alternative) => {
                compiler.compile(alternative.clone_node())?;
                compiler.pop_last_pop_instruction();
            }
            None => {
                compiler.emit(OpCode::OpNull, vec![]);
            }
        }

        let after_alternative_pos = compiler.instructions.len();
        compiler.change_operand(jump_pos, after_alternative_pos as u32);

        Ok(())
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        let mut node = self.clone();
        node.condition = modifier.modify_expression(node.condition);
        node.consequence = modifier.modify_block_statement(node.consequence);
        if node.alternative.is_some() {
            node.alternative = Some(modifier.modify_block_statement(node.alternative.unwrap()))
        }

        modifier.apply(node.clone_node())
    }
}

#[cast_to]
impl Expression for IfExpression {
    fn value(&self) -> String {
        "if".to_string()
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let alt_string = match &self.alternative {
            Some(expr) => format!(" else {}", expr),
            None => "".to_string()
        };

        write!(f, "if {} {}{})", self.condition, self.consequence, alt_string)
    }
}

#[derive(Clone, Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, _evaluator: &Evaluator, _env: &mut Environment) -> Option<Box<dyn Object>> {
        Some(Box::new(Integer { value: self.value }))
    }

    fn compile(&self, compiler: &mut Compiler) -> CompilerResult {
        let integer = Box::new(Integer { value: self.value });
        let pos = compiler.add_constant(integer);
        compiler.emit(OpCode::OpConstant, vec![pos]);
        Ok(())
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        modifier.apply(self.clone_node())
    }
}

#[cast_to]
impl Expression for IntegerLiteral {
    fn value(&self) -> String {
        self.value.to_string()
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone, Debug)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl Node for BooleanLiteral {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, evaluator: &Evaluator, _env: &mut Environment) -> Option<Box<dyn Object>> {
        evaluator.native_to_bool(self.value)
    }

    fn compile(&self, compiler: &mut Compiler) -> CompilerResult {
        compiler.emit(if self.value { OpCode::OpTrue } else { OpCode::OpFalse}, vec![]);
        Ok(())
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        modifier.apply(self.clone_node())
    }
}

#[cast_to]
impl Expression for BooleanLiteral {
    fn value(&self) -> String {
        self.value.to_string()
    }
}

impl Display for BooleanLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone, Debug)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl Node for StringLiteral {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, _evaluator: &Evaluator, _env: &mut Environment) -> Option<Box<dyn Object>> {
        Some(Box::new(StringE { value: self.value.clone() }))
    }

    fn compile(&self, _compiler: &mut Compiler) -> CompilerResult {
        todo!()
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        modifier.apply(self.clone_node())
    }
}

#[cast_to]
impl Expression for StringLiteral {
    fn value(&self) -> String {
        self.value.to_string()
    }
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone, Debug)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, _evaluator: &Evaluator, env: &mut Environment) -> Option<Box<dyn Object>> {
        Some(Box::new(Function { parameters: self.parameters.clone(), body: self.body.clone(), env: env.clone() }))
    }

    fn compile(&self, _compiler: &mut Compiler) -> CompilerResult {
        todo!()
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        let mut node = self.clone();
        let parameters = node.parameters.clone();
        for (i, _) in parameters.iter().enumerate() {
            node.parameters[i] = modifier.modify_identifier(&node.parameters[i]);
        }
        node.body = modifier.modify_block_statement(node.body);

        modifier.apply(node.clone_node())
    }
}

#[cast_to]
impl Expression for FunctionLiteral {
    fn value(&self) -> String {
        "fn".to_string()
    }
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut params = vec![];
        for param in &self.parameters {
            params.push(format!("{}", param));
        }
        write!(f, "{}({}){}", self.token_literal(), params.join(", "), self.body)
    }
}

#[derive(Clone, Debug)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<dyn Expression>,
    pub arguments: Vec<Box<dyn Expression>>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, evaluator: &Evaluator, env: &mut Environment) -> Option<Box<dyn Object>> {
        if self.function.token_literal() == "quote" {
            return evaluator.quote(self.arguments[0].clone_node(), env);
        }

        let function = evaluator.eval(Box::new(self.function.as_node()), env);
        if is_error(&function) {
            return function;
        }

        let function = function.unwrap();

        let args = evaluator.eval_expressions(self.arguments.clone(), env);
        if args.len() == 1 {
            let arg = args[0].clone();
            if is_error(&Some(arg.clone())) {
                return Some(arg);
            }
        }

        evaluator.eval_function_call(function, args)
    }

    fn compile(&self, _compiler: &mut Compiler) -> CompilerResult {
        todo!()
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        modifier.apply(self.clone_node())
    }
}

#[cast_to]
impl Expression for CallExpression {
    fn value(&self) -> String {
        "call".to_string()
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut args = vec![];
        for param in &self.arguments {
            args.push(format!("{}", param));
        }
        write!(f, "{}({})", self.function, args.join(", "))
    }
}


#[derive(Clone, Debug)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Box<dyn Expression>>,
}

impl Node for ArrayLiteral {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, evaluator: &Evaluator, env: &mut Environment) -> Option<Box<dyn Object>> {
        let elements = evaluator.eval_expressions(self.elements.clone(), env);

        if elements.len() == 1 {
            let element = elements[0].clone();
            if is_error(&Some(element.clone())) {
                return Some(element);
            }
        }

        Some(Box::new(Array { elements }))
    }

    fn compile(&self, _compiler: &mut Compiler) -> CompilerResult {
        todo!()
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        let mut node = self.clone();
        for (i, element) in node.elements.clone().iter().enumerate() {
            node.elements[i] = modifier.modify_expression(element.clone());
        }

        modifier.apply(node.clone_node())
    }
}

#[cast_to]
impl Expression for ArrayLiteral {
    fn value(&self) -> String {
        "fn".to_string()
    }
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut elements = vec![];
        for element in &self.elements {
            elements.push(format!("{}", element));
        }
        write!(f, "[{}]", elements.join(", "))
    }
}

#[derive(Clone, Debug)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub index: Box<dyn Expression>,
}

impl Node for IndexExpression {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, evaluator: &Evaluator, env: &mut Environment) -> Option<Box<dyn Object>> {
        let left = evaluator.eval(Box::new(self.left.as_node()), env);
        if is_error(&left) {
            return left;
        }

        let index = evaluator.eval(Box::new(self.index.as_node()), env);
        if is_error(&index) {
            return index;
        }

        return evaluator.eval_index_expression(left.unwrap(), index.unwrap());
    }

    fn compile(&self, _compiler: &mut Compiler) -> CompilerResult {
        todo!()
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        let mut node = self.clone();
        node.left = modifier.modify_expression(node.left);
        node.index = modifier.modify_expression(node.index);

        modifier.apply(node.clone_node())
    }
}

#[cast_to]
impl Expression for IndexExpression {
    fn value(&self) -> String {
        "fn".to_string()
    }
}

impl Display for IndexExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

#[derive(Clone, Debug)]
pub struct MapLiteral {
    pub token: Token,
    pub pairs: HashMap<Box<dyn Expression>, Box<dyn Expression>>,
}

impl Node for MapLiteral {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, evaluator: &Evaluator, env: &mut Environment) -> Option<Box<dyn Object>> {
        evaluator.eval_map_literal(self, env)
    }

    fn compile(&self, _compiler: &mut Compiler) -> CompilerResult {
        todo!()
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        let mut node = self.clone();

        let mut pairs = HashMap::new();
        for (key, value) in node.pairs {
            let key = modifier.modify_expression(key);
            let value = modifier.modify_expression(value);
            pairs.insert(key, value);
        }

        node.pairs = pairs;

        modifier.apply(node.clone_node())
    }
}

#[cast_to]
impl Expression for MapLiteral {
    fn value(&self) -> String {
        "hash".to_string()
    }
}

impl Display for MapLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut pairs = vec![];
        for (key, value) in &self.pairs {
            pairs.push(format!("{}: {}", key, value));
        }
        write!(f, "{{{}}}", pairs.join(", "))
    }
}

#[derive(Clone, Debug)]
pub struct MacroLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Node for MacroLiteral {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn eval(&self, _evaluator: &Evaluator, _env: &mut Environment) -> Option<Box<dyn Object>> {
        panic!("Not implemented")
    }

    fn compile(&self, _compiler: &mut Compiler) -> CompilerResult {
        todo!()
    }

    fn modify(&self, modifier: &mut dyn Modifier) -> Box<dyn Node> {
        let mut node = self.clone();
        let parameters = node.parameters.clone();
        for (i, _) in parameters.iter().enumerate() {
            node.parameters[i] = modifier.modify_identifier(&node.parameters[i]);
        }
        node.body = modifier.modify_block_statement(node.body);

        modifier.apply(node.clone_node())
    }
}

#[cast_to]
impl Expression for MacroLiteral {
    fn value(&self) -> String {
        "fn".to_string()
    }
}

impl Display for MacroLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut params = vec![];
        for param in &self.parameters {
            params.push(format!("{}", param));
        }
        write!(f, "{}({}){}", self.token_literal(), params.join(", "), self.body)
    }
}

#[cfg(test)]
mod tests {
    use crate::token::TokenType;

    use super::*;
    use crate::modifier::ModifierImpl;

    #[test]
    fn test_to_string() {
        let program = Program {
            statements: vec![
                Box::new(LetStatement {
                    token: Token::new(TokenType::Let, "let"),
                    name: Identifier {
                        token: Token::new(TokenType::Ident, "myVar"),
                        value: "myVar".to_string(),
                    },
                    value: Box::new(Identifier {
                        token: Token::new(TokenType::Ident, "anotherVar"),
                        value: "anotherVar".to_string(),
                    }),
                })
            ]
        };

        assert_eq!(format!("{}", program), "let myVar = anotherVar;");
    }

    #[test]
    fn test_modify() {
        let one: fn() -> Box<dyn Expression> = || { Box::new(IntegerLiteral { token: Token::new(TokenType::Integer, "1"), value: 1 }) };
        let two: fn() -> Box<dyn Expression> = || { Box::new(IntegerLiteral { token: Token::new(TokenType::Integer, "2"), value: 2 }) };
        let mut one_to_two = Box::new(|n: Box<dyn Node>| -> Box<dyn Node> {
            let mut integer = match n.as_any().downcast_ref::<IntegerLiteral>() {
                Some(integer) => integer.clone(),
                None => return n.clone(),
            };

            if integer.value != 1 {
                return n.clone();
            }

            integer.value = 2;
            Box::new(integer.clone())
        });
        let mut modifier = ModifierImpl { modifier_fn: &mut one_to_two };

        struct Test {
            input: Box<dyn Node>,
            expected: Box<dyn Node>,
        }

        let mut tests = vec![
            Test { input: one().clone_node(), expected: two().clone_node() },
            Test {
                input: Box::new(Program {
                    statements: vec![
                        Box::new(ExpressionStatement { token: Token::empty(), expression: one() })
                    ]
                }),
                expected: Box::new(Program {
                    statements: vec![
                        Box::new(ExpressionStatement { token: Token::empty(), expression: two() })
                    ]
                }),
            },
            Test {
                input: Box::new(InfixExpression {
                    token: Token::empty(),
                    left: one(),
                    operator: "+".to_string(),
                    right: one(),
                }),
                expected: Box::new(InfixExpression {
                    token: Token::empty(),
                    left: two(),
                    operator: "+".to_string(),
                    right: two(),
                }),
            },
            Test {
                input: Box::new(PrefixExpression {
                    token: Token::empty(),
                    operator: "+".to_string(),
                    right: one(),
                }),
                expected: Box::new(PrefixExpression {
                    token: Token::empty(),
                    operator: "+".to_string(),
                    right: two(),
                }),
            },
            Test {
                input: Box::new(IndexExpression {
                    token: Token::empty(),
                    left: one(),
                    index: one(),
                }),
                expected: Box::new(IndexExpression {
                    token: Token::empty(),
                    left: two(),
                    index: two(),
                }),
            },
            Test {
                input: Box::new(IfExpression {
                    token: Token::empty(),
                    condition: one(),
                    consequence: BlockStatement {
                        token: Token::empty(),
                        statements: vec![
                            Box::new(ExpressionStatement { token: Token::empty(), expression: one() })
                        ],
                    },
                    alternative: Some(BlockStatement {
                        token: Token::empty(),
                        statements: vec![
                            Box::new(ExpressionStatement { token: Token::empty(), expression: one() })
                        ],
                    }),
                }),
                expected: Box::new(IfExpression {
                    token: Token::empty(),
                    condition: two(),
                    consequence: BlockStatement {
                        token: Token::empty(),
                        statements: vec![
                            Box::new(ExpressionStatement { token: Token::empty(), expression: two() })
                        ],
                    },
                    alternative: Some(BlockStatement {
                        token: Token::empty(),
                        statements: vec![
                            Box::new(ExpressionStatement { token: Token::empty(), expression: two() })
                        ],
                    }),
                }),
            },
            Test {
                input: Box::new(ReturnStatement {
                    token: Token::empty(),
                    return_value: one(),
                }),
                expected: Box::new(ReturnStatement {
                    token: Token::empty(),
                    return_value: two(),
                }),
            },
            Test {
                input: Box::new(LetStatement {
                    token: Token::empty(),
                    name: Identifier { token: Token::empty(), value: "one".to_string() },
                    value: one(),
                }),
                expected: Box::new(LetStatement {
                    token: Token::empty(),
                    name: Identifier { token: Token::empty(), value: "one".to_string() },
                    value: two(),
                }),
            },
            Test {
                input: Box::new(FunctionLiteral {
                    token: Token::empty(),
                    parameters: vec![],
                    body: BlockStatement {
                        token: Token::empty(),
                        statements: vec![
                            Box::new(ExpressionStatement { token: Token::empty(), expression: one() })
                        ],
                    },
                }),
                expected: Box::new(FunctionLiteral {
                    token: Token::empty(),
                    parameters: vec![],
                    body: BlockStatement {
                        token: Token::empty(),
                        statements: vec![
                            Box::new(ExpressionStatement { token: Token::empty(), expression: two() })
                        ],
                    },
                }),
            },
            Test {
                input: Box::new(ArrayLiteral {
                    token: Token::empty(),
                    elements: vec![one(), one()],
                }),
                expected: Box::new(ArrayLiteral {
                    token: Token::empty(),
                    elements: vec![two(), two()],
                }),
            },
        ];

        for test in &mut tests {
            let modified = modifier.modify(test.input.clone());

            assert_eq!(modified.to_string(), test.expected.to_string());
        }

        let map_literal = Box::new(MapLiteral {
            token: Token::empty(),
            pairs: vec![
                (one(), one())
            ].into_iter().collect(),
        });

        let modified = modifier.modify(map_literal.clone());//;
        let map_literal = modified.as_any().downcast_ref::<MapLiteral>().unwrap();

        for (key, value) in &map_literal.pairs {
            assert_eq!(key.to_string(), "2".to_string());
            assert_eq!(value.to_string(), "2".to_string());
        }
    }
}