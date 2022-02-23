use core::iter::Peekable;

use alloc::string::{String, ToString};
use alloc::vec::Vec;
use hashbrown::{HashMap, HashSet};

use crate::error::PreProcessorError;
use crate::token::{Token, TokenType};

#[derive(Debug)]
pub struct StringPreProcessor {
    tokens: Vec<Token>,
    constants: HashMap<String, Token>,
    flags: HashSet<String>,
    opcode_count: usize,
    primary_output: Vec<Token>,
    secondary_output: Vec<Token>,
}

type PreProcessorResult<T> = Result<T, PreProcessorError>;

impl StringPreProcessor {
    pub fn new(tokens: Vec<Token>, flags: HashSet<String>) -> Self {
        return Self {
            tokens,
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

    pub fn run(mut self) -> PreProcessorResult<Vec<Token>> {
        self.primary_process()?;
        self.secondary_process()?;

        return Ok(self.secondary_output);
    }

    pub fn run_with_tracking(mut self, label: &str) -> PreProcessorResult<(Vec<Token>, u64)> {
        self.primary_process()?;
        self.secondary_process()?;

        let offset;

        if let Some(v) = self.constants.get(label) {
            match v.token_type() {
                TokenType::UnsignedIntegerLiteral(v) => offset = v,
                _ => return Err(PreProcessorError::NoLabelDefinedWithName(label.to_string())),
            }
        } else {
            return Err(PreProcessorError::NoLabelDefinedWithName(label.to_string()));
        }

        return Ok((self.secondary_output, offset));
    }

    /// Replaces most of the assembler directives with their text.
    ///
    /// Processes the definitions of constants and if statements in the order they are defined.
    /// Labels are replaced by addresses.
    pub fn primary_process(&mut self) -> PreProcessorResult<()> {
        let mut tokens = core::mem::replace(&mut self.tokens, Vec::new())
            .into_iter()
            .peekable();

        while let Some(token) = tokens.next() {
            match token.token_type() {
                TokenType::Identifier => self.handle_identifier(token, &mut tokens)?,
                TokenType::Constant => self.handle_constant_definition(token, &mut tokens)?,
                TokenType::Import => return Err(PreProcessorError::ImportNotPermitted(token)),
                TokenType::If => self.handle_if(token, &mut tokens)?,
                TokenType::Else => return Err(PreProcessorError::UnexpectedElse(token)),
                TokenType::Endif => return Err(PreProcessorError::UnexpectedEndif(token)),
                TokenType::Repeat => self.handle_repeat(token, &mut tokens)?,
                TokenType::EndRepeat => return Err(PreProcessorError::UnexpectedEndRepeat(token)),
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

    pub fn secondary_process(&mut self) -> PreProcessorResult<()> {
        let primary_output = core::mem::replace(&mut self.primary_output, Vec::new());

        for token in primary_output.into_iter() {
            match token.token_type() {
                TokenType::Identifier => {
                    if let Some(cons) = self.constants.get(&token.lexeme().string()) {
                        self.secondary_output.push(cons.clone());
                    } else {
                        return Err(PreProcessorError::UndefinedLabel(token));
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

    fn handle_identifier<T: Iterator<Item = Token>>(
        &mut self,
        constant_identifier: Token,
        tokens: &mut Peekable<T>,
    ) -> PreProcessorResult<()> {
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
    ) -> PreProcessorResult<()> {
        if let Some(name) = tokens.next() {
            if !name.is_identifier() {
                return Err(PreProcessorError::InvalidConstantName(name));
            }

            if let Some(value) = tokens.next() {
                if !value.is_unsigned_integer()
                    && !value.is_signed_integer()
                    && !value.is_float()
                    && !value.is_string()
                {
                    return Err(PreProcessorError::InvalidConstantValue(
                        constant_identifier,
                        value,
                    ));
                }

                self.constants.insert(name.lexeme().string(), value);

                return Ok(());
            } else {
                return Err(PreProcessorError::ExpectedUnsignedIntegerFoundEOF(
                    constant_identifier,
                ));
            }
        } else {
            return Err(PreProcessorError::ExpectedIdentifierFoundEOF(
                constant_identifier,
            ));
        }
    }

    fn handle_repeat<T: Iterator<Item = Token>>(
        &mut self,
        repeat_identifier: Token,
        tokens: &mut T,
    ) -> PreProcessorResult<()> {
        if let Some(next) = tokens.next() {
            let times = match next.token_type() {
                TokenType::UnsignedIntegerLiteral(v) => v,
                _ => {
                    return Err(PreProcessorError::ExpectedUnsignedIntegerFound(
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
                    return Err(PreProcessorError::ForbiddenDirective(next));
                } else {
                    if next.is_opcode() {
                        running_opcode_count += 1;
                    }

                    cache.push(next);
                }
            }

            if !terminated {
                return Err(PreProcessorError::UnterminatedRepeat(repeat_identifier));
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
            return Err(PreProcessorError::ExpectedUnsignedIntegerFoundEOF(
                repeat_identifier,
            ));
        }
    }

    fn handle_if<T: Iterator<Item = Token>>(
        &mut self,
        if_identifier: Token,
        tokens: &mut T,
    ) -> PreProcessorResult<()> {
        if let Some(flag) = tokens.next() {
            if !flag.is_identifier() {
                return Err(PreProcessorError::ExpectedIdentifierFlagFound(
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
                    return Err(PreProcessorError::ForbiddenDirective(next));
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
                return Err(PreProcessorError::UnterminatedIf(if_identifier));
            }

            if else_token.is_some() {
                while let Some(next) = tokens.next() {
                    if next.is_end_if() {
                        terminated = true;
                        break;
                    } else if next.is_directive() && !next.is_identifier() {
                        return Err(PreProcessorError::ForbiddenDirective(next));
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
                    return Err(PreProcessorError::UnterminatedElse(else_token.unwrap()));
                }
            }

            self.primary_output.append(&mut cache);

            return Ok(());
        } else {
            return Err(PreProcessorError::ExpectedIdentifierFoundEOF(if_identifier));
        }
    }
}

#[cfg(test)]
mod tests {
    use alloc::string::ToString;
    use alloc::vec;
    use vxl_iset::instruction_arguments::Register;

    use crate::processing::Lexer;

    use super::*;

    fn process(input: &str) -> Vec<TokenType> {
        return process_flags(input, &[]);
    }

    fn process_flags(input: &str, flags: &[&str]) -> Vec<TokenType> {
        let tokens = Lexer::tokenize_string(input.to_string()).unwrap();

        let mut flags_set = HashSet::new();

        for flag in flags {
            flags_set.insert(flag.to_string());
        }

        let mut processor = StringPreProcessor::new(tokens, flags_set);

        processor.primary_process().unwrap();

        processor.secondary_process().unwrap();

        return processor
            .into_output()
            .into_iter()
            .map(|t| t.token_type())
            .collect();
    }

    #[test]
    fn test_constant_replace() {
        assert_eq!(
            process("%const bob 0u52\nldi $r0, bob"),
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
            process("%const uint 0u52\n%const flt 0f53.2\n%const str \"cowabunga\"\nldi $r0, uint\nldi $r1, uint\nldf $r0, flt"),
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
            process("main:\n\tjmp func\nfunc:\n\tjmp main"),
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
            process("%repeat 3\nldi $r0, 52\n%end_repeat"),
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
            process("%repeat 3\njmp FUNC\n%end_repeat\nFUNC:\nldi $r0, 42"),
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
                "%if COMPILE_INT\nldi $r0, 52\n%else\n\tldi $r0, 53\n%end_if",
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
                "%if COMPILE_INT\nldi $r0, 52\n%else\n\tldi $r0, 53\n%end_if",
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
                "%if COMPILE_INT\njmp FUNC\n%else\n\tldi $r0, 53\n%end_if\nFUNC:\n\tldi $r0, 42",
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
                "%if COMPILE_INT\njmp FUNC\n%else\n\tldi $r0, 53\n%end_if\nFUNC:\n\tldi $r0, 42",
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
                "%if COMPILE_INT\nldi $r0, 53\n%else\n\tjmp FUNC\n%end_if\nFUNC:\n\tldi $r0, 42",
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
