#[derive(Debug, PartialEq)]
pub enum TokenType {
    Assign,
    Asterisk,
    Bang,
    Comma,
    Eq,
    Else,
    False,
    Function,
    Ident,
    Gt,
    Illegal,
    If,
    Number,
    LBrace,
    Let,
    LParen,
    Lt,
    Minus,
    NotEq,
    Plus,
    RBrace,
    RParen,
    Return,
    Slash,
    True,
    Semicolon,
    EOF,
}

#[derive(Debug, PartialEq)]
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