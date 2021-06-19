use voxl_instruction_set::Register;

use crate::token::{Position, Token, TokenType};

#[derive(Clone, Debug, PartialEq)]
pub enum LexerError {
    UnexpectedCharacter(char, Position),
    EmptyIdentifier(Position),
    InvalidHexLiteral(String, Position),
    InvalidBinaryLiteral(String, Position),
    UnexpectedSecondDecimalPoint(Position),
    InvalidFloatLiteral(String, Position),
    InvalidUnsignedIntegerLiteral(String, Position),
    InvalidSignedIntegerLiteral(String, Position),
    InvalidRegister(String, Position),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Lexer {
    chars: Vec<char>,
    file: String,
    tokens: Vec<Token>,
    index: usize,
    row: usize,
    col: usize,
}

impl Lexer {
    pub fn tokenize(chars: Vec<char>, file: String) -> Result<Vec<Token>, LexerError> {
        let mut lexer = Lexer::new(chars, file);

        lexer.process()?;

        return Ok(lexer.into_tokens());
    }

    pub fn new(chars: Vec<char>, file: String) -> Self {
        return Self {
            chars,
            file,
            tokens: Vec::new(),
            index: 0,
            row: 0,
            col: 0,
        };
    }

    pub fn process(&mut self) -> Result<(), LexerError> {
        while let Some(c) = self.current() {
            match c {
                '\n' => self.increment_row(),
                '%' => {
                    self.increment();

                    self.process_identifier()?
                }
                '#' => {
                    self.increment();

                    while let Some(c) = self.current() {
                        if c != '\n' {
                            self.increment();
                        }
                    }
                }
                ',' => {
                    self.increment();

                    self.tokens
                        .push(self.new_token(TokenType::Comma, ",".to_string()));
                }
                ':' => {
                    self.increment();

                    self.tokens
                        .push(self.new_token(TokenType::Colon, ":".to_string()));
                }
                '$' => {
                    self.increment();

                    self.process_register()?;
                }
                'h' => {
                    self.increment();
                    self.process_hex()?;
                }
                'b' => {
                    self.increment();
                    self.process_binary()?;
                }
                _ => {
                    if c.is_whitespace() {
                        self.increment();
                    } else if c.is_alphabetic() || c == '_' {
                        self.process_identifier()?;
                    } else if c.is_numeric() || c == '-' {
                        self.process_numeric()?;
                    } else {
                        return Err(LexerError::UnexpectedCharacter(c, self.current_position()));
                    }
                }
            }
        }

        return Ok(());
    }

    pub fn into_tokens(self) -> Vec<Token> {
        return self.tokens;
    }

    fn process_register(&mut self) -> Result<(), LexerError> {
        let starting_position = self.current_position();
        let mut reg = String::new();

        while let Some(c) = self.current() {
            if c.is_alphanumeric() {
                if reg.len() < 3 {
                    self.increment();

                    reg.push(c);
                } else {
                    return Err(LexerError::UnexpectedCharacter(c, self.current_position()));
                }
            } else {
                break;
            }
        }

        let tp = match reg.as_str() {
            "rsp" => TokenType::Register(Register::RSP),
            "rfp" => TokenType::Register(Register::RFP),
            "rfl" => TokenType::Register(Register::RFL),
            "rou" => TokenType::Register(Register::ROU),
            "rra" => TokenType::Register(Register::RRA),
            "rrb" => TokenType::Register(Register::RRB),
            "r0" => TokenType::Register(Register::R0),
            "r1" => TokenType::Register(Register::R1),
            "r2" => TokenType::Register(Register::R2),
            "r3" => TokenType::Register(Register::R3),
            "r4" => TokenType::Register(Register::R4),
            "r5" => TokenType::Register(Register::R5),
            "r6" => TokenType::Register(Register::R6),
            "r7" => TokenType::Register(Register::R7),
            "r8" => TokenType::Register(Register::R8),
            "r9" => TokenType::Register(Register::R9),
            _ => return Err(LexerError::InvalidRegister(reg, starting_position)),
        };

        self.tokens.push(self.new_token(tp, reg));

        return Ok(());
    }

    fn process_identifier(&mut self) -> Result<(), LexerError> {
        let mut identifier = String::new();

        while let Some(c) = self.current() {
            if !c.is_alphabetic() && c != '_' {
                break;
            }

            identifier.push(c);
            self.increment();
        }

        if identifier.is_empty() {
            return Err(LexerError::EmptyIdentifier(self.current_position()));
        }

        let tp = TokenType::match_identifier(&identifier);

        self.tokens.push(self.new_token(tp, identifier));

        return Ok(());
    }

    fn process_hex(&mut self) -> Result<(), LexerError> {
        let mut num = String::new();
        let starting_pos = self.current_position();

        while let Some(c) = self.current() {
            if c.is_numeric() || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') {
                self.increment();

                num.push(c);
            } else {
                break;
            }
        }

        if let Ok(n) = u64::from_str_radix(&num, 16) {
            self.tokens
                .push(self.new_token(TokenType::UnsignedIntegerLiteral(n), format!("h{}", num)));
        } else {
            return Err(LexerError::InvalidHexLiteral(num, starting_pos));
        }

        return Ok(());
    }

    fn process_binary(&mut self) -> Result<(), LexerError> {
        let mut num = String::new();
        let starting_pos = self.current_position();

        while let Some(c) = self.current() {
            if c == '1' || c == '0' {
                self.increment();

                num.push(c);
            } else {
                break;
            }
        }

        if let Ok(n) = u64::from_str_radix(&num, 2) {
            self.tokens
                .push(self.new_token(TokenType::UnsignedIntegerLiteral(n), format!("b{}", num)));
        } else {
            return Err(LexerError::InvalidBinaryLiteral(num, starting_pos));
        }

        return Ok(());
    }

    fn process_numeric(&mut self) -> Result<(), LexerError> {
        let starting_position = self.current_position();
        let negative;

        let mut is_float = false;
        let mut n_string;

        if self.current() == Some('-') {
            negative = true;

            self.increment();

            n_string = String::from("-");
        } else {
            negative = false;

            n_string = String::new();
        }

        while let Some(c) = self.current() {
            if c.is_numeric() {
                n_string.push(c);
                self.increment();
            } else if c == '.' {
                if is_float {
                    return Err(LexerError::UnexpectedSecondDecimalPoint(
                        self.current_position(),
                    ));
                } else {
                    is_float = true;
                    n_string.push(c);
                    self.increment();
                }
            } else {
                break;
            }
        }

        if is_float {
            let f: f64 = n_string.clone().parse().map_err(|_| {
                LexerError::InvalidFloatLiteral(n_string.clone(), starting_position)
            })?;

            self.tokens
                .push(self.new_token(TokenType::FloatLiteral(f), n_string));
        } else {
            if negative {
                let n: i64 = n_string.clone().parse().map_err(|_| {
                    LexerError::InvalidSignedIntegerLiteral(n_string.clone(), starting_position)
                })?;

                self.tokens
                    .push(self.new_token(TokenType::SignedIntegerLiteral(n), n_string));
            } else {
                let n: u64 = n_string.clone().parse().map_err(|_| {
                    LexerError::InvalidUnsignedIntegerLiteral(n_string.clone(), starting_position)
                })?;

                self.tokens
                    .push(self.new_token(TokenType::UnsignedIntegerLiteral(n), n_string));
            }
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

    fn increment(&mut self) {
        self.index += 1;
        self.col += 1;
    }

    fn increment_row(&mut self) {
        self.index += 1;
        self.col = 0;
        self.row += 1;
    }

    fn new_token(&self, tp: TokenType, lexeme: String) -> Token {
        let len = lexeme.len();

        return Token::new(
            tp,
            lexeme,
            Position::new(self.row, self.col - len),
            self.file.clone(),
        );
    }

    fn current_position(&self) -> Position {
        return Position::new(self.row, self.col);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn new_token(tp: TokenType, lexeme: &str, row: usize, col: usize) -> Token {
        return Token::new(
            tp,
            lexeme.to_string(),
            Position::new(row, col),
            String::new(),
        );
    }

    #[test]
    fn test_registers() {
        let input = "$rsp $rfp $rfl $rra $rrb $r0 $r1 $r2 $r3 $r4 $r5 $r6 $r7 $r8 $r9";

        let output = Lexer::tokenize(input.chars().collect(), String::new()).unwrap();

        assert_eq!(
            output,
            vec![
                new_token(TokenType::Register(Register::RSP), "rsp", 0, 1),
                new_token(TokenType::Register(Register::RFP), "rfp", 0, 6),
                new_token(TokenType::Register(Register::RFL), "rfl", 0, 11),
                new_token(TokenType::Register(Register::RRA), "rra", 0, 16),
                new_token(TokenType::Register(Register::RRB), "rrb", 0, 21),
                new_token(TokenType::Register(Register::R0), "r0", 0, 26),
                new_token(TokenType::Register(Register::R1), "r1", 0, 30),
                new_token(TokenType::Register(Register::R2), "r2", 0, 34),
                new_token(TokenType::Register(Register::R3), "r3", 0, 38),
                new_token(TokenType::Register(Register::R4), "r4", 0, 42),
                new_token(TokenType::Register(Register::R5), "r5", 0, 46),
                new_token(TokenType::Register(Register::R6), "r6", 0, 50),
                new_token(TokenType::Register(Register::R7), "r7", 0, 54),
                new_token(TokenType::Register(Register::R8), "r8", 0, 58),
                new_token(TokenType::Register(Register::R9), "r9", 0, 62),
            ]
        );
    }

    #[test]
    fn test_single_instruction_example_ldi() {
        let input = "ldi 52, $r0";

        let output = Lexer::tokenize(input.chars().collect(), String::new()).unwrap();

        assert_eq!(
            output,
            vec![
                new_token(TokenType::Opcode(3), "ldi", 0, 0),
                new_token(TokenType::UnsignedIntegerLiteral(52), "52", 0, 4),
                new_token(TokenType::Comma, ",", 0, 6),
                new_token(TokenType::Register(Register::R0), "r0", 0, 9),
            ]
        );
    }
    mod identifiers {
        use super::*;

        macro_rules! test_directive {
            ($name:ident, $input:expr, $tp:expr) => {
                #[test]
                fn $name() {
                    let input: &str = $input;

                    let output = Lexer::tokenize(input.chars().collect(), String::new()).unwrap();

                    assert_eq!(output, vec![new_token($tp, &$input[1..], 0, 1),]);
                }
            };
        }

        test_directive!(test_repeat, "%repeat", TokenType::Repeat);
        test_directive!(test_end_repeat, "%end_repeat", TokenType::EndRepeat);
        test_directive!(test_if, "%if", TokenType::If);
        test_directive!(test_else, "%else", TokenType::Else);
        test_directive!(test_endif, "%endif", TokenType::Endif);
        test_directive!(test_import, "%import", TokenType::Import);
        test_directive!(test_const, "%const", TokenType::Constant);

        #[test]
        fn test_identifier() {
            let input: &str = "MAIN";

            let output = Lexer::tokenize(input.chars().collect(), String::new()).unwrap();

            assert_eq!(output, vec![new_token(TokenType::Identifier, input, 0, 0),]);
        }
    }

    #[test]
    fn test_hex() {
        let input = "h2abcdef";
        let output = Lexer::tokenize(input.chars().collect(), String::new()).unwrap();

        assert_eq!(
            output,
            vec![new_token(
                TokenType::UnsignedIntegerLiteral(0x2abcdef),
                input,
                0,
                0
            )]
        )
    }

    #[test]
    fn test_bin() {
        let input = "b01100110";
        let output = Lexer::tokenize(input.chars().collect(), String::new()).unwrap();

        assert_eq!(
            output,
            vec![new_token(
                TokenType::UnsignedIntegerLiteral(0b01100110),
                input,
                0,
                0
            )]
        );
    }

    #[test]
    fn test_signed_int() {
        let input = "-123";
        let output = Lexer::tokenize(input.chars().collect(), String::new()).unwrap();

        assert_eq!(
            output,
            vec![new_token(
                TokenType::SignedIntegerLiteral(-123),
                input,
                0,
                0
            )]
        );
    }

    #[test]
    fn test_float() {
        let input = "-123.333333";
        let output = Lexer::tokenize(input.chars().collect(), String::new()).unwrap();

        assert_eq!(
            output,
            vec![new_token(TokenType::FloatLiteral(-123.333333), input, 0, 0)]
        );
    }

    #[test]
    fn test_comment_eol() {
        let input = "ldi 52, $r0 #452";

        let output = Lexer::tokenize(input.chars().collect(), String::new()).unwrap();

        assert_eq!(
            output,
            vec![
                new_token(TokenType::Opcode(3), "ldi", 0, 0),
                new_token(TokenType::UnsignedIntegerLiteral(52), "52", 0, 4),
                new_token(TokenType::Comma, ",", 0, 6),
                new_token(TokenType::Register(Register::R0), "r0", 0, 9),
            ]
        );
    }

    #[test]
    fn test_comment_full_line() {
        let input = "#ldi 52, $r0";

        let output = Lexer::tokenize(input.chars().collect(), String::new()).unwrap();

        assert_eq!(output, Vec::new());
    }
}
