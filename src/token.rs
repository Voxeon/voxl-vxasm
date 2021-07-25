use voxl_instruction_set::instruction_arguments::Register;

use crate::text_mapping::TextRange;
use paste::paste;

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
    String,
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

macro_rules! match_variant {
    (args $($variant:path : $name:ident),*) => {
        $(
            paste! {
                pub const fn [<is_$name>](&self) -> bool {
                    return match self.tp {
                        $variant(..) => true,
                        _ => false,
                    };
                }
            }
        )*
    };

    ($($variant:path : $name:ident),*) => {
        $(
            paste! {
                pub const fn [<is_$name>](&self) -> bool {
                    return match self.tp {
                        $variant => true,
                        _ => false,
                    };
                }
            }
        )*
    };
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

    match_variant!(args
        TokenType::Register: register,
        TokenType::UnsignedIntegerLiteral: unsigned_integer,
        TokenType::SignedIntegerLiteral: signed_integer,
        TokenType::FloatLiteral: float,
        TokenType::Opcode: opcode
    );

    match_variant!(
        TokenType::String: string,
        TokenType::Comma: comma,
        TokenType::Colon: colon,
        TokenType::Identifier: identifier,
        TokenType::Constant: constant,
        TokenType::Import: import,
        TokenType::If: r#if,
        TokenType::Else: r#else,
        TokenType::Endif: end_if,
        TokenType::Repeat: repeat,
        TokenType::EndRepeat: end_repeat
    );

    pub fn is_directive(&self) -> bool {
        return match self.tp {
            TokenType::Identifier
            | TokenType::Constant
            | TokenType::Import
            | TokenType::If
            | TokenType::Else
            | TokenType::Endif
            | TokenType::Repeat
            | TokenType::EndRepeat => true,
            _ => false,
        };
    }
}

impl TokenType {
    pub fn match_identifier(range: &TextRange) -> Option<TokenType> {
        return Some(match range.string().as_str() {
            "import" => TokenType::Import,
            "const" => TokenType::Constant,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "end_if" => TokenType::Endif,
            "repeat" => TokenType::Repeat,
            "end_repeat" => TokenType::EndRepeat,
            _ => return None,
        });
    }
}
