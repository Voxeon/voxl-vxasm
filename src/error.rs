use crate::text_mapping::{FilePtr, Position, TextRange};
use crate::token::Token;
use alloc::string::{String, ToString};
use core::fmt;

pub trait VXASMError: fmt::Display + fmt::Debug {
    fn reportable_format(&self) -> String {
        return self.to_string();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LexerError {
    UnexpectedCharacter(char, Position, FilePtr),
    EmptyIdentifier(Position, FilePtr),
    InvalidHexLiteral(TextRange),
    InvalidBinaryLiteral(TextRange),
    UnexpectedSecondDecimalPoint(Position, FilePtr),
    InvalidFloatLiteral(TextRange),
    InvalidUnsignedIntegerLiteral(TextRange),
    InvalidSignedIntegerLiteral(TextRange),
    InvalidRegister(TextRange),
    ExpectedRegisterFoundEOF(Position, FilePtr),
    UnknownDirective(TextRange),
    UnterminatedString(TextRange),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PreProcessorError {
    NoLabelDefinedWithName(String),
    FileAlreadyImported(Token),
    FileTokensNotProvidedToken(Token),
    FileTokensNotProvided(FilePtr),
    FileTokensNotProvidedReferenced(FilePtr, Token),
    ExpectedUnsignedIntegerFoundEOF(Token),
    ExpectedUnsignedIntegerFound(Token, Token),
    ExpectedStringFoundEOF(Token),
    ExpectedStringFound(Token, Token),
    InvalidConstantValue(Token, Token),
    UndefinedLabel(Token),
    UnexpectedEndif(Token),
    UnexpectedEndRepeat(Token),
    UnexpectedElse(Token),
    UnterminatedRepeat(Token),
    ForbiddenDirective(Token),
    ExpectedIdentifierFlagFound(Token, Token),
    UnterminatedIf(Token),
    UnterminatedElse(Token),
    ExpectedIdentifierFoundEOF(Token),
    InvalidConstantName(Token),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParserError {
    UnexpectedEOF,
    UnexpectedEOFReference(Token),
    ExpectedOpcode(Token),
    UnknownOpcode(Token),
    ExpectedRegisterForOpcodeArgument(Token, usize),
    ExpectedUnsignedIntegerForOpcodeArgument(Token, usize),
    ExpectedImmediateForOpcodeArgument(Token, usize),
    ExpectedCommaAfter(Token),
}

impl VXASMError for LexerError {}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            LexerError::UnexpectedCharacter(ch, pos, file) => {
                write!(f, "Unexpected character \'{}\' in {} at {}.", ch, file, pos)
            }
            LexerError::EmptyIdentifier(pos, file) => {
                write!(f, "Empty identifier in {} at {}.", file, pos)
            }
            LexerError::InvalidHexLiteral(range) => write!(f, "Invalid hex literal. {}", range),
            LexerError::InvalidBinaryLiteral(range) => {
                write!(f, "Invalid binary literal. {}", range)
            }
            LexerError::UnexpectedSecondDecimalPoint(pos, file) => {
                write!(f, "Unexpected second decimal point in {} at {}", file, pos)
            }
            LexerError::InvalidFloatLiteral(range) => write!(f, "Invalid float literal. {}", range),
            LexerError::InvalidUnsignedIntegerLiteral(range) => {
                write!(f, "Invalid unsigned integer literal. {}", range)
            }
            LexerError::InvalidSignedIntegerLiteral(range) => {
                write!(f, "Invalid signed integer literal. {}", range)
            }
            LexerError::InvalidRegister(range) => write!(f, "Invalid register. {}", range),
            LexerError::ExpectedRegisterFoundEOF(pos, file) => {
                write!(f, "Expected register but found EOF in {} at {}", file, pos)
            }
            LexerError::UnknownDirective(range) => write!(f, "Unknown directive. {}", range),
            LexerError::UnterminatedString(range) => write!(f, "Unterminated string. {}", range),
        };
    }
}

impl VXASMError for PreProcessorError {}

impl fmt::Display for PreProcessorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            PreProcessorError::NoLabelDefinedWithName(name) => {
                write!(f, "No label is defined with the name {}", name)
            }
            PreProcessorError::FileAlreadyImported(import) => {
                write!(f, "File is already imported. {}", import.lexeme())
            }
            PreProcessorError::FileTokensNotProvidedToken(import) => write!(
                f,
                "This file has not been included for assembly. {}",
                import.lexeme()
            ),
            PreProcessorError::FileTokensNotProvided(fptr) => {
                write!(f, "This file has not been included for assembly. {}", fptr)
            }
            PreProcessorError::FileTokensNotProvidedReferenced(file, tok) => write!(
                f,
                "The file tokens were not provided for the file {}. {}",
                file,
                tok.lexeme()
            ),
            PreProcessorError::ExpectedUnsignedIntegerFoundEOF(reference) => write!(
                f,
                "Expected an unsigned integer following {} but found EOF",
                reference.lexeme()
            ),
            PreProcessorError::ExpectedUnsignedIntegerFound(reference, found) => write!(
                f,
                "Expected an unsigned integer following {}. Found {}",
                reference.lexeme(),
                found.lexeme()
            ),
            PreProcessorError::ExpectedStringFoundEOF(reference) => {
                write!(f, "Expected a string but found EOF. {}", reference.lexeme())
            }
            PreProcessorError::ExpectedStringFound(reference, found) => write!(
                f,
                "Expected a string following: {}, but found {}",
                reference.lexeme(),
                found.lexeme()
            ),
            PreProcessorError::InvalidConstantValue(reference, found) => write!(
                f,
                "Invalid value for constant: {}, value: {}",
                reference.lexeme(),
                found.lexeme()
            ),
            PreProcessorError::UndefinedLabel(lbl) => write!(f, "Undefined label {}", lbl.lexeme()),
            PreProcessorError::UnexpectedElse(reference)
            | PreProcessorError::UnexpectedEndif(reference)
            | PreProcessorError::UnexpectedEndRepeat(reference) => {
                write!(f, "Unexpected token. {}", reference.lexeme())
            }
            PreProcessorError::UnterminatedRepeat(reference) => {
                write!(f, "Unterminated repeat. {}", reference.lexeme())
            }
            PreProcessorError::ForbiddenDirective(reference) => write!(
                f,
                "Assembler directives are not allowed here. {}",
                reference.lexeme()
            ),
            PreProcessorError::ExpectedIdentifierFlagFound(reference, found) => write!(
                f,
                "Expected an identifier flag following {} but found {}",
                reference.lexeme(),
                found.lexeme()
            ),
            PreProcessorError::UnterminatedIf(reference) => write!(
                f,
                "Expected 'end_if' or 'else' following 'if'. {}",
                reference.lexeme()
            ),
            PreProcessorError::UnterminatedElse(reference) => write!(
                f,
                "Expected 'end_if' following 'else'. {}",
                reference.lexeme()
            ),
            PreProcessorError::ExpectedIdentifierFoundEOF(reference) => write!(
                f,
                "Expected an identifier instead found EOF. {}",
                reference.lexeme()
            ),
            PreProcessorError::InvalidConstantName(lbl) => {
                write!(f, "Invalid constant name. {}", lbl.lexeme())
            }
        };
    }
}

impl VXASMError for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            ParserError::UnexpectedEOF => write!(f, "Unexpected EOF."),
            ParserError::UnexpectedEOFReference(reference) => {
                write!(f, "Unexpected EOF following {}.", reference.lexeme())
            }
            ParserError::ExpectedOpcode(tok) => {
                write!(f, "Expected opcode but found {}", tok.lexeme())
            }
            ParserError::UnknownOpcode(tok) => write!(f, "Unknown opcode {}", tok.lexeme()),
            ParserError::ExpectedRegisterForOpcodeArgument(tok, arg) => write!(
                f,
                "Argument {} is invalid. Expected a register but found {}",
                arg,
                tok.lexeme()
            ),
            ParserError::ExpectedUnsignedIntegerForOpcodeArgument(tok, arg) => write!(
                f,
                "Argument {} is invalid. Expected an unsigned integer but found {}",
                arg,
                tok.lexeme()
            ),
            ParserError::ExpectedImmediateForOpcodeArgument(tok, arg) => write!(
                f,
                "Argument {} is invalid. Expected an immediate but found {}",
                arg,
                tok.lexeme()
            ),
            ParserError::ExpectedCommaAfter(reference) => {
                write!(f, "Unexpected ',' following {}.", reference.lexeme())
            }
        };
    }
}
