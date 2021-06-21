use voxl_instruction_set::Register;

use crate::text_mapping::TextRange;

/// Represents an understandable token for the preprocessor and parser
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    tp: TokenType,
    lexeme: TextRange,
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

impl Token {
    /// Creates a new token
    pub fn new(tp: TokenType, lexeme: TextRange) -> Self {
        return Self { tp, lexeme };
    }

    /// Returns the token type of this token.
    pub fn token_type(&self) -> TokenType {
        return self.tp;
    }

    /// The raw lexeme for this token.
    pub fn lexeme(&self) -> &TextRange {
        return &self.lexeme;
    }
}

impl TokenType {
    pub fn match_identifier(range: &TextRange) -> Option<TokenType> {
        return Some(match range.string().as_str() {
            "import" => TokenType::Import,
            "const" => TokenType::Constant,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "endif" => TokenType::Endif,
            "repeat" => TokenType::Repeat,
            "end_repeat" => TokenType::EndRepeat,
            _ => return None,
        });
    }
}
