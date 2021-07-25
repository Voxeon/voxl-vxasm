use std::io::Read;
use std::string::String;

use hashbrown::{HashMap, HashSet};
use std::cmp::min;
use voxl_instruction_set::instruction::Instruction;
use voxl_instruction_set::instruction_arguments::{Address, Immediate, Register};
use vxasm::lexer::Lexer;
use vxasm::parser::Parser;
use vxasm::pre_processor::PreProcessor;
use vxasm::text_mapping::FileInfoManager;

#[test]
fn test_parse_sample_vsm() {
    let mut input = String::new();
    std::fs::File::open("sample.vsm")
        .unwrap()
        .read_to_string(&mut input)
        .unwrap();

    let mut f_man = FileInfoManager::new();

    let flags = HashSet::new();
    let mut tokens = HashMap::new();

    let f = f_man.new_file("sample.vsm".to_string(), input.clone());

    let lexer_output = Lexer::tokenize(f.clone()).unwrap();
    tokens.insert(f.clone(), lexer_output);

    let preprocessor = PreProcessor::new(tokens, flags);
    let preprocessor_output = preprocessor.run(&f).unwrap();

    let parser_output = Parser::with_tokens(preprocessor_output).parse().unwrap();

    let expected_output = vec![
        Instruction::Ldi(Immediate::from(10u64), Register::R0),
        Instruction::Malloc(Register::R0, Register::R0),
        Instruction::Ldi(Immediate::from(0x61u64), Register::R1),
        Instruction::Isetb(Immediate::from(0u64), Register::R0, Register::R1),
        Instruction::Ldi(Immediate::from(0x62u64), Register::R1),
        Instruction::Isetb(Immediate::from(1u64), Register::R0, Register::R1),
        Instruction::Call(Address::from(10u64)),
        Instruction::Free(Register::ROU),
        Instruction::Free(Register::R0),
        Instruction::Halt,
        // encode_message
        Instruction::Mov(Register::R3, Register::R1),
        Instruction::Ldb(Immediate::from(0xffu8), Register::R9),
        Instruction::And(Register::R3, Register::R3, Register::R9),
        Instruction::Length(Register::R1, Register::R0),
        Instruction::Malloc(Register::R2, Register::R1),
        Instruction::Ldi(Immediate::from(0u8), Register::R4),
        // encode_message_loop
        Instruction::Cmp(Register::R4, Register::R1),
        Instruction::Jge(Address::from(24u64)),
        Instruction::Getb(Register::R5, Register::R0, Register::R4),
        Instruction::Xor(Register::R5, Register::R5, Register::R3),
        Instruction::Setb(Register::R2, Register::R4, Register::R5),
        Instruction::Ldi(Immediate::from(1u64), Register::R9),
        Instruction::Addu(Register::R4, Register::R4, Register::R9),
        Instruction::Jmp(Address::from(16u64)),
        // end_encode_message_loop
        Instruction::Mov(Register::ROU, Register::R0),
        Instruction::Ret,
    ];
    let m = min(parser_output.len(), expected_output.len());

    for i in 0..m {
        assert_eq!(expected_output[i], parser_output[i]);
    }

    assert_eq!(parser_output.len(), expected_output.len());
}
