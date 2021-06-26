use alloc::vec::Vec;
use voxl_instruction_set::{Instruction, Register};

use crate::error::LexerError;
use crate::text_mapping::{FilePtr, Position, TextRange};
use crate::token::{Token, TokenType};

type LexerResult<T> = Result<T, LexerError>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumericType {
    Signed,
    Unsigned,
    Float,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Lexer {
    chars: Vec<char>,
    file: FilePtr,
    tokens: Vec<Token>,
    index: usize,
    row: usize,
    col: usize,
    default_numeric: NumericType,
}

impl Lexer {
    pub fn tokenize(file: FilePtr) -> Result<Vec<Token>, LexerError> {
        let mut lexer = Lexer::new(
            file.contents().chars().collect(),
            file,
            NumericType::Unsigned,
        );

        lexer.process()?;

        return Ok(lexer.into_tokens());
    }

    pub fn new(chars: Vec<char>, file: FilePtr, default_numeric: NumericType) -> Self {
        return Self {
            chars,
            file,
            tokens: Vec::new(),
            index: 0,
            row: 0,
            col: 0,
            default_numeric,
        };
    }

    pub fn process(&mut self) -> Result<(), LexerError> {
        while let Some(c) = self.current() {
            match c {
                '\n' => self.increment_row(),
                '%' => {
                    self.increment();

                    self.process_directive()?
                }
                '"' => {
                    self.increment();
                    self.process_string()?
                }
                '#' => {
                    self.increment();

                    while let Some(c) = self.current() {
                        if c == '\n' {
                            break;
                        } else {
                            self.increment();
                        }
                    }
                }
                ',' => {
                    self.increment();

                    self.tokens.push(self.new_token(TokenType::Comma, 1));
                }
                ':' => {
                    self.increment();

                    self.tokens.push(self.new_token(TokenType::Colon, 1));
                }
                '$' => {
                    self.increment();

                    self.process_register()?;
                }
                '0' => {
                    if self.peek().is_some() {
                        match self.peek().unwrap() {
                            'x' => {
                                self.increment();
                                self.increment();
                                self.process_hex()?;
                            }
                            'b' => {
                                self.increment();
                                self.increment();
                                self.process_binary()?;
                            }
                            'i' => {
                                self.increment();
                                self.increment();
                                self.process_signed()?;
                            }
                            'u' => {
                                self.increment();
                                self.increment();
                                self.process_unsigned()?;
                            }
                            'f' => {
                                self.increment();
                                self.increment();
                                self.process_float()?;
                            }
                            _ => self.process_default_numeric()?,
                        }
                    } else {
                        self.process_default_numeric()?;
                    }
                }
                _ => {
                    if c.is_whitespace() {
                        self.increment();
                    } else if c.is_alphabetic() || c == '_' {
                        self.process_identifier()?;
                    } else if c.is_digit(10) || c == '-' {
                        self.process_default_numeric()?;
                    } else {
                        return Err(LexerError::UnexpectedCharacter(
                            c,
                            self.current_position(),
                            self.file.clone(),
                        ));
                    }
                }
            }
        }

        return Ok(());
    }

    pub fn into_tokens(self) -> Vec<Token> {
        return self.tokens;
    }

    fn process_register(&mut self) -> LexerResult<()> {
        let starting_position = self.current_position();

        fn consume_until_end_identifier(s: &mut Lexer) -> Position {
            let mut end_position = s.current_position();

            while let Some(c) = s.current() {
                if !c.is_alphanumeric() {
                    break;
                }

                s.increment();
                end_position = s.current_position();
            }

            return end_position;
        }

        if self.remaining_length() == 0 {
            return Err(LexerError::ExpectedRegisterFoundEOF(
                starting_position,
                self.file.clone(),
            ));
        } else if self.remaining_length() < 2 {
            return Err(LexerError::InvalidRegister(TextRange::new(
                starting_position,
                consume_until_end_identifier(self),
                self.file.clone(),
            )));
        }

        if self.current() != Some('r') {
            return Err(LexerError::InvalidRegister(TextRange::new(
                starting_position,
                consume_until_end_identifier(self),
                self.file.clone(),
            )));
        }

        self.increment();

        macro_rules! len_3_reg {
            ($f_ch:literal : $f_r:expr $(,$ch:literal : $r:expr),*) => {
                {
                    self.increment();

                    if self.current().is_none() {
                        return Err(LexerError::ExpectedRegisterFoundEOF(starting_position, self.file.clone()));
                    }

                    let out;

                    if self.current() == Some($f_ch) {
                        self.increment();

                        out = ($f_r, 3)
                    } $(
                        else if self.current() == Some($ch) {
                            self.increment();

                            out = ($r, 3)
                        }
                    )* else {
                        return Err(LexerError::InvalidRegister(TextRange::new(
                            starting_position,
                            consume_until_end_identifier(self),
                            self.file.clone(),
                        )));
                    }

                    out
                }
            }
        }

        let (reg, len) = match self.current().unwrap() {
            'f' => {
                len_3_reg!('p': Register::RFP, 'l': Register::RFL)
            }
            's' => {
                len_3_reg!('p': Register::RSP)
            }
            'o' => {
                len_3_reg!('u': Register::ROU)
            }
            'r' => {
                len_3_reg!('a': Register::RRA, 'b': Register::RRB)
            }
            c => {
                if c.is_digit(10) {
                    self.increment();

                    let reference = self.current_position();
                    let end = consume_until_end_identifier(self);

                    if end != reference {
                        return Err(LexerError::InvalidRegister(TextRange::new(
                            starting_position,
                            end,
                            self.file.clone(),
                        )));
                    }

                    let reg = Register::from(Register::R0 as u8 + c.to_digit(10).unwrap() as u8);
                    (reg, 2)
                } else {
                    return Err(LexerError::InvalidRegister(TextRange::new(
                        starting_position,
                        consume_until_end_identifier(self),
                        self.file.clone(),
                    )));
                }
            }
        };

        self.tokens
            .push(self.new_token(TokenType::Register(reg), len));

        return Ok(());
    }

    fn process_directive(&mut self) -> Result<(), LexerError> {
        let mut len = 0;

        while let Some(c) = self.current() {
            if !c.is_alphabetic() && c != '_' {
                break;
            }

            self.increment();
            len += 1;
        }

        if len == 0 {
            return Err(LexerError::EmptyIdentifier(
                self.current_position(),
                self.file.clone(),
            ));
        }

        let range = self.current_range(len);

        if let Some(identifier) = TokenType::match_identifier(&range) {
            self.tokens.push(Token::new(identifier, range));
        } else {
            return Err(LexerError::UnknownDirective(range));
        }

        return Ok(());
    }

    fn process_string(&mut self) -> Result<(), LexerError> {
        let mut len = 0;
        let mut terminated = false;

        while let Some(ch) = self.current() {
            if ch == '"' {
                terminated = true;
                self.increment();
                break;
            } else if ch == '\n' {
                break;
            }

            len += 1;
            self.increment();
        }

        if !terminated {
            return Err(LexerError::UnterminatedString(self.current_range(len)));
        }

        let range = self.current_range_offset(len, 1);

        self.tokens.push(Token::new(TokenType::String, range));

        return Ok(());
    }

    fn process_identifier(&mut self) -> Result<(), LexerError> {
        let mut len = 0;
        let mut possible_opcode = true;

        while let Some(c) = self.current() {
            if !c.is_alphabetic() && c != '_' {
                break;
            }

            if c == '_' {
                possible_opcode = false;
            }

            self.increment();
            len += 1;
        }

        if len == 0 {
            return Err(LexerError::EmptyIdentifier(
                self.current_position(),
                self.file.clone(),
            ));
        }

        if possible_opcode {
            let range = self.current_range(len);
            if let Some(code) = Instruction::from_string(&range.string()) {
                self.tokens
                    .push(self.new_token(TokenType::Opcode(code), len));
                return Ok(());
            }
        }

        self.tokens.push(self.new_token(TokenType::Identifier, len));

        return Ok(());
    }

    fn process_hex(&mut self) -> Result<(), LexerError> {
        let mut len = 0;

        while let Some(c) = self.current() {
            if c.is_digit(16) {
                self.increment();

                len += 1;
            } else {
                break;
            }
        }

        let range = self.current_range(len);

        if let Ok(n) = u64::from_str_radix(&range.string(), 16) {
            self.tokens
                .push(Token::new(TokenType::UnsignedIntegerLiteral(n), range));
        } else {
            return Err(LexerError::InvalidHexLiteral(range));
        }

        return Ok(());
    }

    fn process_binary(&mut self) -> Result<(), LexerError> {
        let mut n: u64 = 0;
        let mut len = 0;

        while let Some(c) = self.current() {
            if c.is_digit(2) {
                self.increment();

                if len == 64 {
                    return Err(LexerError::InvalidBinaryLiteral(self.current_range(len)));
                }

                if n == 0 {
                    n = c.to_digit(2).unwrap() as u64;
                } else {
                    n <<= 1;
                    n |= c.to_digit(2).unwrap() as u64;
                }

                len += 1;
            } else {
                break;
            }
        }

        self.tokens
            .push(self.new_token(TokenType::UnsignedIntegerLiteral(n), len));

        return Ok(());
    }

    fn process_default_numeric(&mut self) -> Result<(), LexerError> {
        return match self.default_numeric {
            NumericType::Signed => self.process_signed(),
            NumericType::Unsigned => {
                if self.current() == Some('-') {
                    return Err(LexerError::UnexpectedCharacter(
                        '-',
                        self.current_position(),
                        self.file.clone(),
                    ));
                }

                self.process_unsigned()
            }
            NumericType::Float => self.process_float(),
        };
    }

    fn process_signed(&mut self) -> Result<(), LexerError> {
        let mut len;
        let mut n = 0;
        let negative = self.current() == Some('-');

        if negative {
            self.increment();
            len = 1;
        } else {
            len = 0;
        }

        while let Some(c) = self.current() {
            if !c.is_digit(10) {
                break;
            }

            if n == 0 {
                n = c.to_digit(10).unwrap() as i64;
            } else {
                n *= 10;
                if let Some(new) = n.checked_add(c.to_digit(10).unwrap() as i64) {
                    n = new;
                } else {
                    return Err(LexerError::InvalidSignedIntegerLiteral(
                        self.current_range(len + 1),
                    ));
                }
            }

            self.increment();
            len += 1;
        }

        if len == 0 {
            return Err(LexerError::InvalidSignedIntegerLiteral(TextRange::new(
                self.current_position(),
                self.current_position(),
                self.file.clone(),
            )));
        }

        if negative {
            n *= -1;
        }

        self.tokens
            .push(self.new_token(TokenType::SignedIntegerLiteral(n), len));

        return Ok(());
    }

    fn process_unsigned(&mut self) -> Result<(), LexerError> {
        let mut len = 0;
        let mut n = 0;

        while let Some(c) = self.current() {
            if !c.is_digit(10) {
                break;
            }

            if n == 0 {
                n = c.to_digit(10).unwrap() as u64;
            } else {
                n *= 10;
                if let Some(new) = n.checked_add(c.to_digit(10).unwrap() as u64) {
                    n = new;
                } else {
                    return Err(LexerError::InvalidUnsignedIntegerLiteral(
                        self.current_range(len + 1),
                    ));
                }
            }

            self.increment();
            len += 1;
        }

        if len == 0 {
            return Err(LexerError::InvalidSignedIntegerLiteral(TextRange::new(
                self.current_position(),
                self.current_position(),
                self.file.clone(),
            )));
        }

        self.tokens
            .push(self.new_token(TokenType::UnsignedIntegerLiteral(n), len));

        return Ok(());
    }

    fn process_float(&mut self) -> Result<(), LexerError> {
        let mut len;
        let mut found_point = false;

        if self.current() == Some('-') {
            len = 1;
            self.increment();
        } else {
            len = 0;
        }

        while let Some(c) = self.current() {
            if !c.is_digit(10) {
                if c == '.' && found_point || c != '.' {
                    break;
                } else {
                    found_point = true;
                }
            }

            self.increment();
            len += 1;
        }

        if len == 0 {
            return Err(LexerError::InvalidFloatLiteral(TextRange::new(
                self.current_position(),
                self.current_position(),
                self.file.clone(),
            )));
        }

        let range = self.current_range(len);

        if let Ok(f) = fast_float::parse(range.string()) {
            self.tokens
                .push(Token::new(TokenType::FloatLiteral(f), range));
        } else {
            return Err(LexerError::InvalidFloatLiteral(range));
        }

        return Ok(());
    }

    fn current(&self) -> Option<char> {
        if self.index < self.chars.len() {
            return Some(self.chars[self.index]);
        } else {
            return None;
        }
    }

    fn peek(&self) -> Option<char> {
        if self.index + 1 < self.chars.len() {
            return Some(self.chars[self.index + 1]);
        } else {
            return None;
        }
    }

    fn increment(&mut self) {
        self.index += 1;
        self.col += 1;
    }

    fn increment_row(&mut self) {
        self.index += 1;
        self.col = 0;
        self.row += 1;
    }

    fn new_token(&self, tp: TokenType, lexeme_len: usize) -> Token {
        return Token::new(tp, self.current_range(lexeme_len));
    }

    fn current_range(&self, lexeme_len: usize) -> TextRange {
        return self.current_range_offset(lexeme_len, 0);
    }

    fn current_range_offset(&self, lexeme_len: usize, offset: usize) -> TextRange {
        return TextRange::new(
            Position::new(
                self.index - lexeme_len - offset,
                self.row,
                self.col - lexeme_len - offset,
            ),
            Position::new(self.index - offset, self.row, self.col - offset),
            self.file.clone(),
        );
    }

    fn current_position(&self) -> Position {
        return Position::new(self.index, self.row, self.col);
    }

    fn remaining_length(&self) -> usize {
        return self.chars.len() - self.index;
    }
}

#[cfg(test)]
mod tests {
    use crate::text_mapping::FileInfoManager;

    use super::*;
    use alloc::string::{String, ToString};
    use alloc::vec;

    macro_rules! new_token {
        ($tp:expr, $col:expr, $len:expr, $file:expr) => {
            Token::new(
                $tp,
                TextRange::new(
                    Position::new($col, 0, $col),
                    Position::new($col + $len, 0, $col + $len),
                    $file,
                ),
            )
        };

        ($tp:expr, $index:expr, $row:expr, $col:expr, $len:expr, $file:expr) => {
            Token::new(
                $tp,
                TextRange::new(
                    Position::new($index, $row, $col),
                    Position::new($index + $len, $row, $col + $len),
                    $file,
                ),
            )
        };
    }

    #[test]
    fn test_registers() {
        let input = "$rsp $rfp $rou $rfl $rra $rrb $r0 $r1 $r2 $r3 $r4 $r5 $r6 $r7 $r8 $r9";

        let mut f_man = FileInfoManager::new();

        let f = f_man.new_file(String::new(), input.to_string());

        let output = Lexer::tokenize(f.clone()).unwrap();

        for i in 0..16 {
            assert_eq!(
                output[i],
                new_token!(
                    TokenType::Register(Register::from(i as u8)),
                    1 + if i >= 6 { 5 * 6 + 4 * (i - 6) } else { 5 * i },
                    if i >= 6 { 2 } else { 3 },
                    f.clone()
                )
            );
        }
    }

    mod directives {
        use super::*;

        macro_rules! test_directive {
            ($name:ident, $input:expr, $tp:expr) => {
                #[test]
                fn $name() {
                    let input: &str = $input;

                    let mut f_man = FileInfoManager::new();

                    let f = f_man.new_file(String::new(), input.to_string());

                    let output = Lexer::tokenize(f.clone()).unwrap();

                    assert_eq!(output, vec![new_token!($tp, 1, input.len() - 1, f.clone())]);
                }
            };
        }

        test_directive!(test_repeat, "%repeat", TokenType::Repeat);
        test_directive!(test_end_repeat, "%end_repeat", TokenType::EndRepeat);
        test_directive!(test_if, "%if", TokenType::If);
        test_directive!(test_else, "%else", TokenType::Else);
        test_directive!(test_end_if, "%end_if", TokenType::Endif);
        test_directive!(test_import, "%import", TokenType::Import);
        test_directive!(test_const, "%const", TokenType::Constant);
    }

    #[test]
    fn test_identifier() {
        let input: &str = "MAIN";

        let mut f_man = FileInfoManager::new();

        let f = f_man.new_file(String::new(), input.to_string());

        let output = Lexer::tokenize(f.clone()).unwrap();

        assert_eq!(
            output,
            vec![new_token!(TokenType::Identifier, 0, input.len(), f.clone())]
        );
    }

    #[test]
    fn test_string() {
        let input: &str = "\"MAIN\"";

        let mut f_man = FileInfoManager::new();

        let f = f_man.new_file(String::new(), input.to_string());

        let output = Lexer::tokenize(f.clone()).unwrap();

        assert_eq!(output, vec![new_token!(TokenType::String, 1, 4, f.clone())]);
    }

    #[test]
    fn test_opcode_and_identifier() {
        let input: &str = "call MAIN";
        let mut f_man = FileInfoManager::new();
        let f = f_man.new_file(String::new(), input.to_string());
        let output = Lexer::tokenize(f.clone()).unwrap();

        assert_eq!(
            output,
            vec![
                new_token!(TokenType::Opcode(0x43), 0, 4, f.clone()),
                new_token!(TokenType::Identifier, 5, 4, f.clone())
            ]
        );
    }

    #[test]
    fn test_single_instruction_example_ldi() {
        let input = "ldi 52, $r0";
        let mut f_man = FileInfoManager::new();
        let f = f_man.new_file(String::new(), input.to_string());
        let output = Lexer::tokenize(f.clone()).unwrap();

        assert_eq!(
            output,
            vec![
                new_token!(TokenType::Opcode(3), 0, 3, f.clone()),
                new_token!(TokenType::UnsignedIntegerLiteral(52), 4, 2, f.clone()),
                new_token!(TokenType::Comma, 6, 1, f.clone()),
                new_token!(TokenType::Register(Register::R0), 9, 2, f.clone()),
            ]
        );
    }

    #[test]
    fn test_instruction_examples() {
        let input = "ldi 0u52, $r1\nmalloc $r0, $r1";
        let mut f_man = FileInfoManager::new();
        let f = f_man.new_file(String::new(), input.to_string());
        let output = Lexer::tokenize(f.clone()).unwrap();
        let expected = vec![
            new_token!(TokenType::Opcode(0x3), 0, 3, f.clone()),
            new_token!(TokenType::UnsignedIntegerLiteral(52), 6, 2, f.clone()),
            new_token!(TokenType::Comma, 8, 1, f.clone()),
            new_token!(TokenType::Register(Register::R1), 11, 2, f.clone()),
            new_token!(TokenType::Opcode(0x9), 14, 1, 0, 6, f.clone()),
            new_token!(TokenType::Register(Register::R0), 22, 1, 8, 2, f.clone()),
            new_token!(TokenType::Comma, 24, 1, 10, 1, f.clone()),
            new_token!(TokenType::Register(Register::R1), 27, 1, 13, 2, f.clone()),
        ];

        assert_eq!(output.len(), expected.len());

        for i in 0..output.len() {
            assert_eq!(output[i], expected[i]);
        }
    }

    #[test]
    fn test_hex() {
        let input = "0x2abcdef";
        let mut f_man = FileInfoManager::new();
        let f = f_man.new_file(String::new(), input.to_string());
        let output = Lexer::tokenize(f.clone()).unwrap();

        assert_eq!(
            output,
            vec![new_token!(
                TokenType::UnsignedIntegerLiteral(0x2abcdef),
                2,
                input.len() - 2,
                f.clone()
            )]
        )
    }

    #[test]
    fn test_bin() {
        let input = "0b01100110";
        let mut f_man = FileInfoManager::new();
        let f = f_man.new_file(String::new(), input.to_string());
        let output = Lexer::tokenize(f.clone()).unwrap();

        assert_eq!(
            output,
            vec![new_token!(
                TokenType::UnsignedIntegerLiteral(0b01100110),
                2,
                input.len() - 2,
                f.clone()
            )]
        )
    }

    #[test]
    fn test_bin_64bits() {
        let input = "0b1110011001100110011001100110011001100110011001100110011001100110";
        let mut f_man = FileInfoManager::new();
        let f = f_man.new_file(String::new(), input.to_string());
        let output = Lexer::tokenize(f.clone()).unwrap();

        assert_eq!(
            output,
            vec![new_token!(
                TokenType::UnsignedIntegerLiteral(
                    0b1110011001100110011001100110011001100110011001100110011001100110
                ),
                2,
                input.len() - 2,
                f.clone()
            )]
        )
    }

    #[test]
    fn test_bin_65bits() {
        let input = "0b11100110011001100110011001100110011001100110011001100110011001101";
        let mut f_man = FileInfoManager::new();
        let f = f_man.new_file(String::new(), input.to_string());
        let output = Lexer::tokenize(f.clone()).unwrap_err();

        assert_eq!(
            output,
            LexerError::InvalidBinaryLiteral(TextRange::new(
                Position::new(3, 0, 3),
                Position::new(input.len(), 0, input.len()),
                f.clone()
            ))
        )
    }

    #[test]
    fn test_signed_int() {
        let input = "0i-123";
        let mut f_man = FileInfoManager::new();
        let f = f_man.new_file(String::new(), input.to_string());
        let output = Lexer::tokenize(f.clone()).unwrap();

        assert_eq!(
            output,
            vec![new_token!(
                TokenType::SignedIntegerLiteral(-123),
                2,
                4,
                f.clone()
            )]
        );
    }

    #[test]
    fn test_float() {
        let input = "0f-123.333333";
        let mut f_man = FileInfoManager::new();
        let f = f_man.new_file(String::new(), input.to_string());
        let output = Lexer::tokenize(f.clone()).unwrap();

        assert_eq!(
            output,
            vec![new_token!(
                TokenType::FloatLiteral(-123.333333),
                2,
                input.len() - 2,
                f.clone()
            )]
        );
    }

    #[test]
    fn test_comment_eol() {
        let input = "ldi 52, $r0 #452";
        let mut f_man = FileInfoManager::new();
        let f = f_man.new_file(String::new(), input.to_string());
        let output = Lexer::tokenize(f.clone()).unwrap();

        assert_eq!(
            output,
            vec![
                new_token!(TokenType::Opcode(3), 0, 3, f.clone()),
                new_token!(TokenType::UnsignedIntegerLiteral(52), 4, 2, f.clone()),
                new_token!(TokenType::Comma, 6, 1, f.clone()),
                new_token!(TokenType::Register(Register::R0), 9, 2, f.clone()),
            ]
        );
    }

    #[test]
    fn test_comment_full_line() {
        let input = "#ldi 52, $r0";
        let mut f_man = FileInfoManager::new();
        let f = f_man.new_file(String::new(), input.to_string());
        let output = Lexer::tokenize(f.clone()).unwrap();

        assert_eq!(output, Vec::new());
    }
}
