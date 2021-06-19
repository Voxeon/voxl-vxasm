use crate::token::{Token, TokenType};

pub enum LexerError {
    UnexpectedCharacter(char, usize, usize),
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
                '%' => self.process_directive(),
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
                _ => {
                    if c.is_whitespace() {
                        self.increment();
                    } else if c.is_alphabetic() || c == '_' {
                        self.process_identifier();
                    } else if c.is_numeric() || c == '-' {
                        self.process_numeric();
                    } else {
                        return Err(LexerError::UnexpectedCharacter(c, self.row, self.col));
                    }
                }
            }
        }

        return Ok(());
    }

    pub fn into_tokens(self) -> Vec<Token> {
        return self.tokens;
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
        return Token::new(
            tp,
            lexeme,
            self.row,
            self.col - lexeme.len(),
            self.file.clone(),
        );
    }
}
