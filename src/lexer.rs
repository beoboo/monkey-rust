use crate::token::*;

pub struct Lexer {
    input: String,
    position: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.to_string(),
            position: 0,
        }
    }

    pub fn next_token(&mut self) -> Token {
        let mut ch = self.read_next();

        while is_whitespace(ch) {
            ch = self.read_next();
        }

        match ch {
            '=' => {
                if self.peek_next() == '=' {
                    let next = self.read_next();
                    let mut str = ch.to_string();
                    str.push(next);

                    Token::new(TokenType::Eq, str.as_str())
                } else {
                    self.new_token(TokenType::Assign, ch)
                }
            }
            '!' => {
                if self.peek_next() == '=' {
                    let next = self.read_next();
                    let mut str = ch.to_string();
                    str.push(next);

                    Token::new(TokenType::NotEq, str.as_str())
                } else {
                    self.new_token(TokenType::Bang, ch)
                }
            }
            '+' => self.new_token(TokenType::Plus, ch),
            '-' => self.new_token(TokenType::Minus, ch),
            '/' => self.new_token(TokenType::Slash, ch),
            '*' => self.new_token(TokenType::Asterisk, ch),
            '(' => self.new_token(TokenType::LParen, ch),
            ')' => self.new_token(TokenType::RParen, ch),
            '{' => self.new_token(TokenType::LBrace, ch),
            '}' => self.new_token(TokenType::RBrace, ch),
            '[' => self.new_token(TokenType::LBracket, ch),
            ']' => self.new_token(TokenType::RBracket, ch),
            ',' => self.new_token(TokenType::Comma, ch),
            ':' => self.new_token(TokenType::Colon, ch),
            ';' => self.new_token(TokenType::Semicolon, ch),
            '<' => self.new_token(TokenType::Lt, ch),
            '>' => self.new_token(TokenType::Gt, ch),
            '"' => Token::new(TokenType::String, self.read_string()),
            '\0' => Token::new(TokenType::EOF, ""),
            _ => {
                if is_alpha(ch) {
                    let literal = self.read_identifier();
                    Token::new(Token::lookup_keyword(literal), literal)
                } else if is_digit(ch) {
                    Token::new(TokenType::Integer, self.read_integer())
                } else {
                    self.new_token(TokenType::Illegal, ch)
                }
            }
        }
    }

    fn new_token(&self, token_type: TokenType, ch: char) -> Token {
        Token::new(token_type, ch.to_string().as_str())
    }

    fn read_next(&mut self) -> char {
        if self.position >= self.input.len() {
            return '\0';
        }

        let ch = self.read_char(self.position);
        self.position += 1;
        ch
    }

    fn peek_next(&self) -> char {
        if self.position >= self.input.len() {
            return '\0';
        }

        self.read_char(self.position)
    }

    fn read_identifier(&mut self) -> &str {
        let mut position = self.position;

        let mut ch = self.read_char(position);
        while is_alpha(ch) {
            position += 1;
            ch = self.read_char(position);
        }

        let ident = self.input[(self.position - 1)..position].as_ref();
        self.position = position;
        ident
    }

    fn read_integer(&mut self) -> &str {
        let mut position = self.position;

        let mut ch = self.read_char(position);
        while is_digit(ch) {
            position += 1;
            ch = self.read_char(position);
        }

        let number = self.input[(self.position - 1)..position].as_ref();
        self.position = position;
        number
    }

    fn read_string(&mut self) -> &str {
        let position = self.position;

        let mut ch = self.read_next();
        while ch != '"' && ch != '\0' {
            ch = self.read_next();
        }

        self.input[position..self.position - 1].as_ref()
    }

    fn read_char(&self, position: usize) -> char {
        if self.input.len() <= position {
            return '\0';
        }
        self.input.chars().nth(position).unwrap()
    }
}

fn is_alpha(ch: char) -> bool {
    match ch {
        'a'..='z' | 'A'..='Z' | '_' => true,
        _ => false
    }
}

fn is_digit(ch: char) -> bool {
    match ch {
        '0'..='9' => true,
        _ => false
    }
}

fn is_whitespace(ch: char) -> bool {
    match ch {
        ' ' | '\n' | '\t' | '\r' => true,
        _ => false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn operators() {
        let input = "=+(){},;";
        let expected = vec![
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Plus, "+"),
            Token::new(TokenType::LParen, "("),
            Token::new(TokenType::RParen, ")"),
            Token::new(TokenType::LBrace, "{"),
            Token::new(TokenType::RBrace, "}"),
            Token::new(TokenType::Comma, ","),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::EOF, ""),
        ];

        let mut lexer = Lexer::new(input);

        for item in expected {
            let token = lexer.next_token();

            assert_eq!(token.token_type, item.token_type);
            assert_eq!(token.literal, item.literal);
        }
    }

    #[test]
    fn next_token() {
        let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);

!-/*5;
5 < 10 > 5;

if (5 < 10) {
  return true;
} else {
  return false;
}

10 == 10;
10 != 9;
\"foobar\"
\"foo bar\"\
[1, 2]
{\"foo\": \"bar\"}
macro(x, y) { x + y };
";

        let expected = vec![
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Ident, "five"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Integer, "5"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Ident, "ten"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Integer, "10"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Ident, "add"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Function, "fn"),
            Token::new(TokenType::LParen, "("),
            Token::new(TokenType::Ident, "x"),
            Token::new(TokenType::Comma, ","),
            Token::new(TokenType::Ident, "y"),
            Token::new(TokenType::RParen, ")"),
            Token::new(TokenType::LBrace, "{"),
            Token::new(TokenType::Ident, "x"),
            Token::new(TokenType::Plus, "+"),
            Token::new(TokenType::Ident, "y"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::RBrace, "}"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Ident, "result"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Ident, "add"),
            Token::new(TokenType::LParen, "("),
            Token::new(TokenType::Ident, "five"),
            Token::new(TokenType::Comma, ","),
            Token::new(TokenType::Ident, "ten"),
            Token::new(TokenType::RParen, ")"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Bang, "!"),
            Token::new(TokenType::Minus, "-"),
            Token::new(TokenType::Slash, "/"),
            Token::new(TokenType::Asterisk, "*"),
            Token::new(TokenType::Integer, "5"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Integer, "5"),
            Token::new(TokenType::Lt, "<"),
            Token::new(TokenType::Integer, "10"),
            Token::new(TokenType::Gt, ">"),
            Token::new(TokenType::Integer, "5"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::If, "if"),
            Token::new(TokenType::LParen, "("),
            Token::new(TokenType::Integer, "5"),
            Token::new(TokenType::Lt, "<"),
            Token::new(TokenType::Integer, "10"),
            Token::new(TokenType::RParen, ")"),
            Token::new(TokenType::LBrace, "{"),
            Token::new(TokenType::Return, "return"),
            Token::new(TokenType::True, "true"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::RBrace, "}"),
            Token::new(TokenType::Else, "else"),
            Token::new(TokenType::LBrace, "{"),
            Token::new(TokenType::Return, "return"),
            Token::new(TokenType::False, "false"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::RBrace, "}"),
            Token::new(TokenType::Integer, "10"),
            Token::new(TokenType::Eq, "=="),
            Token::new(TokenType::Integer, "10"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Integer, "10"),
            Token::new(TokenType::NotEq, "!="),
            Token::new(TokenType::Integer, "9"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::String, "foobar"),
            Token::new(TokenType::String, "foo bar"),
            Token::new(TokenType::LBracket, "["),
            Token::new(TokenType::Integer, "1"),
            Token::new(TokenType::Comma, ","),
            Token::new(TokenType::Integer, "2"),
            Token::new(TokenType::RBracket, "]"),
            Token::new(TokenType::LBrace, "{"),
            Token::new(TokenType::String, "foo"),
            Token::new(TokenType::Colon, ":"),
            Token::new(TokenType::String, "bar"),
            Token::new(TokenType::RBrace, "}"),
            Token::new(TokenType::Macro, "macro"),
            Token::new(TokenType::LParen, "("),
            Token::new(TokenType::Ident, "x"),
            Token::new(TokenType::Comma, ","),
            Token::new(TokenType::Ident, "y"),
            Token::new(TokenType::RParen, ")"),
            Token::new(TokenType::LBrace, "{"),
            Token::new(TokenType::Ident, "x"),
            Token::new(TokenType::Plus, "+"),
            Token::new(TokenType::Ident, "y"),
            Token::new(TokenType::RBrace, "}"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::EOF, ""),
        ];

        let mut lexer = Lexer::new(input);

        for item in expected {
            let token = lexer.next_token();

            assert_eq!(token.token_type, item.token_type);
            assert_eq!(token.literal, item.literal);
        }
    }
}