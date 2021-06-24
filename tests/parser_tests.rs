use std::io::Read;
use std::string::String;

use voxl_instruction_set::Register;
use vxasm::lexer::Lexer;
use vxasm::instruction_parser::InstructionParser;
use vxasm::parser::Parser;
use vxasm::text_mapping::FileInfoManager;
use vxasm::token::TokenType;
use vxasm::token::TokenType::*;
use vxasm::pre_processor::PreProcessor;
use hashbrown::{HashSet, HashMap};

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

    let lexer_output = Lexer::tokenize(input.chars().collect(), f.clone()).unwrap();
    tokens.insert(f.clone(), lexer_output);

    let mut preprocessor = PreProcessor::new(tokens, flags);
    let preprocessor_output = preprocessor.run(&f).unwrap();

    let mut parser = InstructionParser::new();
    parser.parse(preprocessor_output).unwrap();

    let parser_output = parser.into_instructions();

    assert_eq!(parser_output, Vec::new());
}
