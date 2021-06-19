use voxl_instruction_set::Register;

use crate::token::{Position, Token, TokenType};

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
            if c.is_alphabetic() {
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
            if !c.is_alphabetic() {
                break;
            }

            identifier.push(c);
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
                .push(self.new_token(TokenType::UnsignedIntegerLiteral(n), format!("h{}", num)));
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
