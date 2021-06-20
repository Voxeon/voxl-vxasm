use alloc::string::String;
use voxl_instruction_set::{Instruction, Register};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Position {
    pub row: usize,
    pub col: usize,
}

/// Represents an understandable token for the preprocessor and parser
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    tp: TokenType,
    lexeme: String,
    pos: Position,
    file_name: String,
}

/// The supported token types
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenType {
    Register(Register),
    UnsignedIntegerLiteral(u64),
    SignedIntegerLiteral(i64),
    FloatLiteral(f64),
    Opcode(u8),
    Comma,
    Colon,

    Identifier,
    Constant,
    Import,
    If,
    Else,
    Endif,
    Repeat,
    EndRepeat,
}

impl Position {
    pub fn new(row: usize, col: usize) -> Position {
        return Position { row, col };
    }
}

impl Token {
    /// Creates a new token
    pub fn new(tp: TokenType, lexeme: String, pos: Position, file_name: String) -> Self {
        return Self {
            tp,
            lexeme,
            pos,
            file_name,
        };
    }

    /// Returns the token type of this token.
    pub fn token_type(&self) -> TokenType {
        return self.tp;
    }

    /// The raw lexeme for this token.
    pub fn lexeme(&self) -> &String {
        return &self.lexeme;
    }

    /// Gets the position of this token in the file.
    pub fn pos(&self) -> Position {
        return self.pos;
    }

    /// Gets a reference to the file name that this token originated from.
    pub fn file_name(&self) -> &String {
        return &self.file_name;
    }
}

impl TokenType {
    pub fn match_identifier(s: &str) -> TokenType {
        return match s {
            "import" => TokenType::Import,
            "const" => TokenType::Constant,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "endif" => TokenType::Endif,
            "repeat" => TokenType::Repeat,
            "end_repeat" => TokenType::EndRepeat,
            _ => {
                if let Some(code) = Instruction::from_string(s) {
                    TokenType::Opcode(code)
                } else {
                    TokenType::Identifier
                }
            }
        };
    }
}
