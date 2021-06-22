use core::iter::Peekable;

use alloc::rc::Rc;
use alloc::string::String;
use alloc::vec::Vec;
use hashbrown::{HashMap, HashSet};

use crate::text_mapping::FileInfo;
use crate::token::{Token, TokenType};

#[derive(Clone, Debug, PartialEq)]
pub enum ParserError {
    FileAlreadyImported(Token),
    FileTokensNotProvidedToken(Token),
    FileTokensNotProvided(Rc<FileInfo>),
    FileTokensNotProvidedReferenced(Rc<FileInfo>, Token),
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
}

#[derive(Debug)]
pub struct PreProcessor {
    tokens: HashMap<Rc<FileInfo>, Vec<Token>>,
    processed_files: HashSet<Rc<FileInfo>>,
    constants: HashMap<String, Token>,
    flags: HashSet<String>,
    opcode_count: usize,
    primary_output: Vec<Token>,
    secondary_output: Vec<Token>,
}

type ParserResult<T> = Result<T, ParserError>;

impl PreProcessor {
    pub fn new(tokens: HashMap<Rc<FileInfo>, Vec<Token>>, flags: HashSet<String>) -> Self {
        return Self {
            tokens,
            processed_files: HashSet::new(),
            constants: HashMap::new(),
            flags,
            opcode_count: 0,
            primary_output: Vec::new(),
            secondary_output: Vec::new(),
        };
    }

    pub fn add_flag(&mut self, flag: String) {
        self.flags.insert(flag);
    }

    /// Replaces most of the assembler directives with their text.
    ///
    /// Processes the definitions of constants, if statements and imports in the order they are defined.
    /// Labels are replaced by addresses.
    pub fn primary_process(&mut self, root_file: &Rc<FileInfo>) -> ParserResult<()> {
        return self.process_file(root_file, None);
    }

    pub fn secondary_process(&mut self) -> ParserResult<()> {
        let primary_output = core::mem::replace(&mut self.primary_output, Vec::new());

        for token in primary_output.into_iter() {
            match token.token_type() {
                TokenType::Identifier => {
                    if let Some(cons) = self.constants.get(&token.lexeme().string()) {
                        self.secondary_output.push(cons.clone());
                    } else {
                        return Err(ParserError::UndefinedLabel(token));
                    }
                }
                _ => {
                    self.secondary_output.push(token);
                }
            }
        }

        return Ok(());
    }

    pub fn primary_output(&self) -> &Vec<Token> {
        return &self.primary_output;
    }

    pub fn into_output(self) -> Vec<Token> {
        return self.secondary_output;
    }

    pub fn output(&self) -> &Vec<Token> {
        return &self.secondary_output;
    }

    fn process_file(&mut self, file: &Rc<FileInfo>, file_name: Option<&Token>) -> ParserResult<()> {
        if self.processed_files.contains(file) {
            if let Some(file_name) = file_name {
                return Err(ParserError::FileAlreadyImported(file_name.clone()));
            } else {
                // Should never occur because file_name is only none for the root file.
                panic!("Unexpected doubly processed file");
            }
        }

        if !self.tokens.contains_key(file) {
            if let Some(file_name) = file_name {
                return Err(ParserError::FileTokensNotProvidedReferenced(
                    file.clone(),
                    file_name.clone(),
                ));
            } else {
                return Err(ParserError::FileTokensNotProvided(file.clone()));
            }
        }

        let mut tokens = self.tokens.remove(file).unwrap().into_iter().peekable();
        self.processed_files.insert(file.clone());

        while let Some(token) = tokens.next() {
            match token.token_type() {
                TokenType::Identifier => self.handle_identifier(token, &mut tokens)?,
                TokenType::Constant => self.handle_constant_definition(token, &mut tokens)?,
                TokenType::Import => self.handle_import(token, &mut tokens)?,
                TokenType::If => self.handle_if(token, &mut tokens)?,
                TokenType::Else => return Err(ParserError::UnexpectedElse(token)),
                TokenType::Endif => return Err(ParserError::UnexpectedEndif(token)),
                TokenType::Repeat => self.handle_repeat(token, &mut tokens)?,
                TokenType::EndRepeat => return Err(ParserError::UnexpectedEndRepeat(token)),
                TokenType::Opcode(_) => {
                    self.opcode_count += 1;
                    self.primary_output.push(token);
                }
                _ => {
                    self.primary_output.push(token);
                }
            }
        }

        return Ok(());
    }

    fn handle_identifier<T: Iterator<Item = Token>>(
        &mut self,
        constant_identifier: Token,
        tokens: &mut Peekable<T>,
    ) -> ParserResult<()> {
        let str_ident = constant_identifier.lexeme().string();

        if tokens.peek().is_some() {
            match tokens.peek().unwrap().token_type() {
                TokenType::Colon => {
                    tokens.next();

                    self.constants.insert(
                        str_ident,
                        Token::new(
                            TokenType::UnsignedIntegerLiteral(self.opcode_count as u64),
                            constant_identifier.lexeme().clone(),
                        ),
                    );

                    return Ok(());
                }
                _ => (),
            }
        }

        if let Some(v) = self.constants.get(&str_ident) {
            self.primary_output.push(v.clone());
        } else {
            self.primary_output.push(constant_identifier);
        }

        return Ok(());
    }

    fn handle_constant_definition<T: Iterator<Item = Token>>(
        &mut self,
        constant_identifier: Token,
        tokens: &mut T,
    ) -> ParserResult<()> {
        if let Some(name) = tokens.next() {
            if !name.is_identifier() {
                todo!();
            }

            if let Some(value) = tokens.next() {
                if !value.is_unsigned_integer()
                    && !value.is_signed_integer()
                    && !value.is_float()
                    && !value.is_string()
                {
                    return Err(ParserError::InvalidConstantValue(
                        constant_identifier,
                        value,
                    ));
                }

                self.constants.insert(name.lexeme().string(), value);

                return Ok(());
            } else {
                return Err(ParserError::ExpectedUnsignedIntegerFoundEOF(
                    constant_identifier,
                ));
            }
        } else {
            todo!();
        }
    }

    fn handle_import<T: Iterator<Item = Token>>(
        &mut self,
        import_identifier: Token,
        tokens: &mut T,
    ) -> ParserResult<()> {
        if let Some(next) = tokens.next() {
            if !next.is_string() {
                return Err(ParserError::ExpectedStringFound(import_identifier, next));
            }

            let file = next.lexeme().string();
            let mut f = None;

            for file_info in &self.processed_files {
                if file_info.name() == &file {
                    return Err(ParserError::FileAlreadyImported(next));
                }
            }

            for file_info in self.tokens.keys() {
                if file_info.name() == &file {
                    f = Some(file_info.clone());
                    break;
                }
            }

            if f == None {
                return Err(ParserError::FileTokensNotProvidedToken(next));
            }

            return self.process_file(&f.unwrap(), Some(&next));
        } else {
            return Err(ParserError::ExpectedStringFoundEOF(import_identifier));
        }
    }

    fn handle_repeat<T: Iterator<Item = Token>>(
        &mut self,
        repeat_identifier: Token,
        tokens: &mut T,
    ) -> ParserResult<()> {
        if let Some(next) = tokens.next() {
            let times = match next.token_type() {
                TokenType::UnsignedIntegerLiteral(v) => v,
                _ => {
                    return Err(ParserError::ExpectedUnsignedIntegerFound(
                        repeat_identifier,
                        next,
                    ))
                }
            };

            let mut cache = Vec::new();

            let mut terminated = false;

            let mut running_opcode_count = 0;

            while let Some(next) = tokens.next() {
                if next.is_end_repeat() {
                    terminated = true;
                    break;
                } else if next.is_directive() && !next.is_identifier() {
                    return Err(ParserError::ForbiddenDirective(next));
                } else {
                    if next.is_opcode() {
                        running_opcode_count += 1;
                    }

                    cache.push(next);
                }
            }

            if !terminated {
                return Err(ParserError::UnterminatedRepeat(repeat_identifier));
            }

            for i in 0..times {
                if i == times - 1 {
                    self.primary_output.append(&mut cache)
                } else {
                    self.primary_output.append(&mut cache.clone())
                }
            }

            self.opcode_count += (running_opcode_count * times) as usize;

            return Ok(());
        } else {
            return Err(ParserError::ExpectedUnsignedIntegerFoundEOF(
                repeat_identifier,
            ));
        }
    }

    fn handle_if<T: Iterator<Item = Token>>(
        &mut self,
        if_identifier: Token,
        tokens: &mut T,
    ) -> ParserResult<()> {
        if let Some(flag) = tokens.next() {
            if !flag.is_identifier() {
                return Err(ParserError::ExpectedIdentifierFlagFound(
                    if_identifier,
                    flag,
                ));
            }

            let keep = self.flags.contains(&flag.lexeme().string());

            let mut cache = Vec::new();

            let mut terminated = false;
            let mut else_token = None;

            while let Some(next) = tokens.next() {
                if next.is_end_if() {
                    terminated = true;
                    break;
                } else if next.is_else() {
                    else_token = Some(next);
                    break;
                } else if next.is_directive() && !next.is_identifier() {
                    return Err(ParserError::ForbiddenDirective(next));
                } else {
                    if keep {
                        if next.is_opcode() {
                            self.opcode_count += 1;
                        }

                        cache.push(next);
                    }
                }
            }

            if !terminated && else_token.is_none() {
                return Err(ParserError::UnterminatedIf(if_identifier));
            }

            if else_token.is_some() {
                while let Some(next) = tokens.next() {
                    if next.is_end_if() {
                        terminated = true;
                        break;
                    } else if next.is_directive() && !next.is_identifier() {
                        return Err(ParserError::ForbiddenDirective(next));
                    } else {
                        if !keep {
                            if next.is_opcode() {
                                self.opcode_count += 1;
                            }

                            cache.push(next);
                        }
                    }
                }

                if !terminated {
                    return Err(ParserError::UnterminatedElse(else_token.unwrap()));
                }
            }

            self.primary_output.append(&mut cache);

            return Ok(());
        } else {
            return Err(ParserError::ExpectedIdentifierFoundEOF(if_identifier));
        }
    }
}

#[cfg(test)]
mod tests {
    use alloc::string::ToString;
    use alloc::vec;
    use voxl_instruction_set::Register;

    use crate::lexer::Lexer;
    use crate::text_mapping::FileInfoManager;

    use super::*;

    fn process(inputs: &[(&str, &str)]) -> Vec<TokenType> {
        return process_flags(inputs, &[]);
    }

    fn process_flags(inputs: &[(&str, &str)], flags: &[&str]) -> Vec<TokenType> {
        let mut root_file = None;
        let mut f_man = FileInfoManager::new();
        let mut tokens = HashMap::new();

        for (file_name, input) in inputs {
            let f = f_man.new_file(file_name.to_string(), input.to_string());
            let output = Lexer::tokenize(input.chars().collect(), f.clone()).unwrap();

            if root_file.is_none() {
                root_file = Some(f.clone());
            }

            tokens.insert(f, output);
        }

        let mut flags_set = HashSet::new();

        for flag in flags {
            flags_set.insert(flag.to_string());
        }

        let mut processor = PreProcessor::new(tokens, flags_set);

        processor
            .primary_process(root_file.as_ref().unwrap())
            .unwrap();

        processor.secondary_process().unwrap();

        return processor
            .into_output()
            .into_iter()
            .map(|t| t.token_type())
            .collect();
    }

    #[test]
    fn test_import() {}

    #[test]
    fn test_constant_replace() {
        assert_eq!(
            process(&[("root.asm", "%const bob 0u52\nldi $r0, bob")]),
            vec![
                TokenType::Opcode(0x3),
                TokenType::Register(Register::R0),
                TokenType::Comma,
                TokenType::UnsignedIntegerLiteral(52)
            ]
        );
    }

    #[test]
    fn test_constant_replace_multi() {
        assert_eq!(
            process(&[("root.asm", "%const uint 0u52\n%const flt 0f53.2\n%const str \"cowabunga\"\nldi $r0, uint\nldi $r1, uint\nldf $r0, flt")]),
            vec![
                TokenType::Opcode(0x3),
                TokenType::Register(Register::R0),
                TokenType::Comma,
                TokenType::UnsignedIntegerLiteral(52),
                TokenType::Opcode(0x3),
                TokenType::Register(Register::R1),
                TokenType::Comma,
                TokenType::UnsignedIntegerLiteral(52),
                TokenType::Opcode(0x4),
                TokenType::Register(Register::R0),
                TokenType::Comma,
                TokenType::FloatLiteral(53.2),
            ]
        );
    }

    #[test]
    fn test_label() {
        assert_eq!(
            process(&[("root.asm", "main:\n\tjmp func\nfunc:\n\tjmp main")]),
            vec![
                TokenType::Opcode(0x37),
                TokenType::UnsignedIntegerLiteral(1),
                TokenType::Opcode(0x37),
                TokenType::UnsignedIntegerLiteral(0),
            ]
        );
    }

    #[test]
    fn test_repeat() {
        assert_eq!(
            process(&[("root.asm", "%repeat 3\nldi $r0, 52\n%end_repeat")]),
            vec![
                TokenType::Opcode(0x3),
                TokenType::Register(Register::R0),
                TokenType::Comma,
                TokenType::UnsignedIntegerLiteral(52),
                TokenType::Opcode(0x3),
                TokenType::Register(Register::R0),
                TokenType::Comma,
                TokenType::UnsignedIntegerLiteral(52),
                TokenType::Opcode(0x3),
                TokenType::Register(Register::R0),
                TokenType::Comma,
                TokenType::UnsignedIntegerLiteral(52),
            ]
        );
    }

    #[test]
    fn test_repeat_label() {
        assert_eq!(
            process(&[(
                "root.asm",
                "%repeat 3\njmp FUNC\n%end_repeat\nFUNC:\nldi $r0, 42"
            )]),
            vec![
                TokenType::Opcode(0x37),
                TokenType::UnsignedIntegerLiteral(3),
                TokenType::Opcode(0x37),
                TokenType::UnsignedIntegerLiteral(3),
                TokenType::Opcode(0x37),
                TokenType::UnsignedIntegerLiteral(3),
                TokenType::Opcode(0x3),
                TokenType::Register(Register::R0),
                TokenType::Comma,
                TokenType::UnsignedIntegerLiteral(42),
            ]
        );
    }

    #[test]
    fn test_if_success() {
        assert_eq!(
            process_flags(
                &[(
                    "root.asm",
                    "%if COMPILE_INT\nldi $r0, 52\n%else\n\tldi $r0, 53\n%end_if"
                )],
                &["COMPILE_INT"]
            ),
            vec![
                TokenType::Opcode(0x3),
                TokenType::Register(Register::R0),
                TokenType::Comma,
                TokenType::UnsignedIntegerLiteral(52),
            ]
        );
    }

    #[test]
    fn test_if_else() {
        assert_eq!(
            process_flags(
                &[(
                    "root.asm",
                    "%if COMPILE_INT\nldi $r0, 52\n%else\n\tldi $r0, 53\n%end_if"
                )],
                &["DONT_COMPILE_INT"]
            ),
            vec![
                TokenType::Opcode(0x3),
                TokenType::Register(Register::R0),
                TokenType::Comma,
                TokenType::UnsignedIntegerLiteral(53),
            ]
        );
    }

    #[test]
    fn test_if_else_label() {
        assert_eq!(
            process_flags(
                &[(
                    "root.asm",
                    "%if COMPILE_INT\njmp FUNC\n%else\n\tldi $r0, 53\n%end_if\nFUNC:\n\tldi $r0, 42"
                )],
                &["DONT_COMPILE_INT"]
            ),
            vec![
                TokenType::Opcode(0x3),
                TokenType::Register(Register::R0),
                TokenType::Comma,
                TokenType::UnsignedIntegerLiteral(53),
                TokenType::Opcode(0x3),
                TokenType::Register(Register::R0),
                TokenType::Comma,
                TokenType::UnsignedIntegerLiteral(42),
            ]
        );
    }

    #[test]
    fn test_if_success_label() {
        assert_eq!(
            process_flags(
                &[(
                    "root.asm",
                    "%if COMPILE_INT\njmp FUNC\n%else\n\tldi $r0, 53\n%end_if\nFUNC:\n\tldi $r0, 42"
                )],
                &["COMPILE_INT"]
            ),
            vec![
                TokenType::Opcode(0x37),
                TokenType::UnsignedIntegerLiteral(1),
                TokenType::Opcode(0x3),
                TokenType::Register(Register::R0),
                TokenType::Comma,
                TokenType::UnsignedIntegerLiteral(42),
            ]
        );
    }

    #[test]
    fn test_if_fail_label() {
        assert_eq!(
            process_flags(
                &[(
                    "root.asm",
                    "%if COMPILE_INT\nldi $r0, 53\n%else\n\tjmp FUNC\n%end_if\nFUNC:\n\tldi $r0, 42"
                )],
                &["DONT_COMPILE_INT"]
            ),
            vec![
                TokenType::Opcode(0x37),
                TokenType::UnsignedIntegerLiteral(1),
                TokenType::Opcode(0x3),
                TokenType::Register(Register::R0),
                TokenType::Comma,
                TokenType::UnsignedIntegerLiteral(42),
            ]
        );
    }
}
