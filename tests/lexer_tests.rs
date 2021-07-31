use std::io::Read;
use std::string::String;

use vxl_iset::instruction_arguments::Register;
use vxlasm::processing::Lexer;
use vxlasm::text_mapping::FileInfoManager;
use vxlasm::token::TokenType;
use vxlasm::token::TokenType::*;

#[test]
fn test_lex_sample_vsm() {
    let mut input = String::new();
    std::fs::File::open("sample.vsm")
        .unwrap()
        .read_to_string(&mut input)
        .unwrap();

    let mut f_man = FileInfoManager::new();
    let f = f_man.new_file(String::new(), input.clone());
    let output = Lexer::tokenize(f.clone()).unwrap();
    let output: Vec<TokenType> = output.into_iter().map(|t| t.token_type()).collect();

    assert_eq!(
        output,
        vec![
            Identifier,
            Colon,
            Opcode(3),
            Register(Register::R0),
            Comma,
            UnsignedIntegerLiteral(10),
            Opcode(9),
            Register(Register::R0),
            Comma,
            Register(Register::R0),
            Opcode(3),
            Register(Register::R1),
            Comma,
            UnsignedIntegerLiteral(97),
            Opcode(15),
            Register(Register::R0),
            Comma,
            UnsignedIntegerLiteral(0),
            Comma,
            Register(Register::R1),
            Opcode(3),
            Register(Register::R1),
            Comma,
            UnsignedIntegerLiteral(98),
            Opcode(15),
            Register(Register::R0),
            Comma,
            UnsignedIntegerLiteral(1),
            Comma,
            Register(Register::R1),
            Opcode(67),
            Identifier,
            Opcode(11),
            Register(Register::ROU),
            Opcode(11),
            Register(Register::R0),
            Opcode(69),
            Identifier,
            Colon,
            Opcode(5),
            Register(Register::R3),
            Comma,
            Register(Register::R1),
            Opcode(2),
            Register(Register::R9),
            Comma,
            UnsignedIntegerLiteral(255),
            Opcode(49),
            Register(Register::R3),
            Comma,
            Register(Register::R3),
            Comma,
            Register(Register::R9),
            Opcode(22),
            Register(Register::R1),
            Comma,
            Register(Register::R0),
            Opcode(9),
            Register(Register::R2),
            Comma,
            Register(Register::R1),
            Opcode(3),
            Register(Register::R4),
            Comma,
            UnsignedIntegerLiteral(0),
            Identifier,
            Colon,
            Opcode(52),
            Register(Register::R4),
            Comma,
            Register(Register::R1),
            Opcode(58),
            Identifier,
            Opcode(17),
            Register(Register::R5),
            Comma,
            Register(Register::R0),
            Comma,
            Register(Register::R4),
            Opcode(51),
            Register(Register::R5),
            Comma,
            Register(Register::R5),
            Comma,
            Register(Register::R3),
            Opcode(13),
            Register(Register::R2),
            Comma,
            Register(Register::R4),
            Comma,
            Register(Register::R5),
            Opcode(3),
            Register(Register::R9),
            Comma,
            UnsignedIntegerLiteral(1),
            Opcode(31),
            Register(Register::R4),
            Comma,
            Register(Register::R4),
            Comma,
            Register(Register::R9),
            Opcode(55),
            Identifier,
            Identifier,
            Colon,
            Opcode(5),
            Register(Register::ROU),
            Comma,
            Register(Register::R0),
            Opcode(68)
        ]
    );
}
