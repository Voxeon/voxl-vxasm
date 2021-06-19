use voxl_instruction_set::Register;

/// Represents an understandable token for the preprocessor and parser
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    tp: TokenType,
    lexeme: String,
    row: usize,
    col: usize,
    file_name: String,
}

/// The supported token types
#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Register(Register),
    UnsignedIntegerLiteral(u64),
    SignedIntegerLiteral(i64),
    FloatLiteral(f64),
    Opcode(String, u8),
    Comma,
    Label(String),
    Constant(String, String),
    LableDeclaration(String),
    Import(String),
    If(String),
    Else,
    Endif,
    Repeat(usize),
    EndRepeat,
}

impl Token {
    /// Creates a new token
    pub fn new(tp: TokenType, lexeme: String, row: usize, col: usize, file_name: String) -> Self {
        return Self {
            tp,
            lexeme,
            row,
            col,
            file_name,
        };
    }

    /// Returns the token type of this token.
    pub fn token_type(&self) -> &TokenType {
        return &self.tp;
    }

    /// The raw lexeme for this token.
    pub fn lexeme(&self) -> &String {
        return &self.lexeme;
    }

    /// Gets the position of this token in the file. Returns (column, row).
    pub fn pos(&self) -> (usize, usize) {
        return (self.col, self.row);
    }

    /// Gets a reference to the file name that this token originated from.
    pub fn file_name(&self) -> &String {
        return &self.file_name;
    }
}

impl TokenType {
    /// Checks if this token type is an assembler directive.
    pub fn is_directive(&self) -> bool {
        return match self {
            Self::If(_)
            | Self::Else
            | Self::Endif
            | Self::Repeat(_)
            | Self::EndRepeat
            | Self::Import(_)
            | Self::Label(_)
            | Self::Constant(_, _)
            | Self::LableDeclaration(_) => true,
            _ => false,
        };
    }
}
