use core::fmt;
use std::fmt::Formatter;

#[derive(Debug, PartialEq, Clone, Eq, Hash, Copy)]
pub enum TokenType {
    Assign,
    Asterisk,
    Bang,
    Comma,
    Colon,
    Eq,
    Else,
    False,
    Function,
    Ident,
    Gt,
    Illegal,
    If,
    Integer,
    LBrace,
    LBracket,
    Let,
    LParen,
    Lt,
    Minus,
    NotEq,
    Plus,
    RBrace,
    RBracket,
    RParen,
    Return,
    Slash,
    String,
    True,
    Semicolon,
    EOF,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
   }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: &str) -> Self {
        Self {
            token_type,
            literal: literal.to_string(),
        }
    }

    pub fn lookup_keyword(ident: &str) -> TokenType {
        match ident {
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "fn" => TokenType::Function,
            "if" => TokenType::If,
            "let" => TokenType::Let,
            "return" => TokenType::Return,
            "true" => TokenType::True,
            _ => TokenType::Ident
        }
    }
}