use crate::token::Token;
use alloc::vec::Vec;

pub trait Parser {
    type Output;

    fn parse(&mut self, tokens: Vec<Token>) -> Self::Output;
}
