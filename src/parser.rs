use alloc::vec::Vec;
use voxl_instruction_set::{Address, Immediate, Instruction, Register};

use crate::error::ParserError;
use crate::token::{Token, TokenType};
use core::iter::Peekable;

type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug)]
pub struct Parser {
    instructions: Vec<Instruction>,
    tokens_iter: Peekable<alloc::vec::IntoIter<Token>>,
}

impl Parser {
    pub fn with_tokens(tokens: Vec<Token>) -> Self {
        return Self {
            instructions: Vec::new(),
            tokens_iter: tokens.into_iter().peekable(),
        };
    }
    pub fn parse(mut self) -> ParserResult<Vec<Instruction>> {
        while self.tokens_iter.peek().is_some() {
            self.parse_opcode()?;
        }

        return Ok(self.instructions);
    }

    fn parse_opcode(&mut self) -> ParserResult<()> {
        let opcode_token = self.next(None)?;

        let opcode = match opcode_token.token_type() {
            TokenType::Opcode(code) => code,
            _ => return Err(ParserError::ExpectedOpcode(opcode_token)),
        };

        let addresses;

        if let Some(a) = Instruction::address_count(opcode) {
            addresses = a;
        } else {
            return Err(ParserError::UnknownOpcode(opcode_token));
        }

        let immediates = Instruction::immediate_count(opcode).unwrap();
        let registers = Instruction::register_count(opcode).unwrap();

        let (registers, immediates, addresses) =
            self.get_arguments(opcode_token, opcode, registers, immediates, addresses)?;

        self.instructions
            .push(Instruction::new(opcode, registers, addresses, immediates).unwrap());

        return Ok(());
    }

    fn get_arguments(
        &mut self,
        opcode_token: Token,
        opcode: u8,
        registers: usize,
        immediates: usize,
        addresses: usize,
    ) -> ParserResult<(Vec<Register>, Vec<Immediate>, Vec<Address>)> {
        let mut i = 0;

        let mut registers_vec = Vec::with_capacity(registers);
        let mut immediates_vec = Vec::with_capacity(immediates);
        let mut addresses_vec = Vec::with_capacity(addresses);

        while i < registers + immediates + addresses {
            let next;

            if let Some(n) = Instruction::get_type_for_index(opcode, i) {
                next = n;
            } else {
                panic!("Unexpected mismatch of opcodes and type acquisition.");
            }

            let tok;

            if next == 0 {
                // Register
                tok = self.next(Some(&opcode_token))?;

                let reg = match tok.token_type() {
                    TokenType::Register(r) => r,
                    _ => {
                        return Err(ParserError::ExpectedRegisterForOpcodeArgument(
                            opcode_token,
                            i + 1,
                        ))
                    }
                };

                registers_vec.push(reg);
            } else if next == 1 {
                // Immediate
                tok = self.next(Some(&opcode_token))?;

                let imm = match tok.token_type() {
                    TokenType::UnsignedIntegerLiteral(u) => Immediate::from(u),
                    TokenType::SignedIntegerLiteral(i) => Immediate::from(i),
                    TokenType::FloatLiteral(f) => Immediate::from(f),
                    _ => {
                        return Err(ParserError::ExpectedImmediateForOpcodeArgument(
                            opcode_token,
                            i + 1,
                        ))
                    }
                };

                immediates_vec.push(imm);
            } else if next == 2 {
                // Address
                tok = self.next(Some(&opcode_token))?;

                let add = match tok.token_type() {
                    TokenType::UnsignedIntegerLiteral(u) => u,
                    _ => {
                        return Err(ParserError::ExpectedUnsignedIntegerForOpcodeArgument(
                            opcode_token,
                            i + 1,
                        ))
                    }
                };

                addresses_vec.push(Address::from(add));
            } else {
                panic!("Unknown argument type {}", next);
            }

            i += 1;

            if i != registers + immediates + addresses {
                let comma = self.next(Some(&tok))?;

                if !comma.is_comma() {
                    return Err(ParserError::ExpectedCommaAfter(tok));
                }
            }
        }

        return Ok((registers_vec, immediates_vec, addresses_vec));
    }

    fn next(&mut self, reference_token: Option<&Token>) -> ParserResult<Token> {
        if let Some(tok) = self.tokens_iter.next() {
            return Ok(tok);
        } else {
            if let Some(reference_token) = reference_token {
                return Err(ParserError::UnexpectedEOFReference(reference_token.clone()));
            } else {
                return Err(ParserError::UnexpectedEOF);
            }
        }
    }
}
