use alloc::vec::Vec;
use core::convert::TryInto;
use digest::Digest;
use voxl_instruction_set::instruction::Instruction;
use voxl_instruction_set::vxl_file::{VXLFile, VXLHeader};

pub struct Assembler {
    instructions: Vec<Instruction>,
    starting_offset: usize,
    sha2: bool,
}

impl Assembler {
    const VERSION: u8 = 0x0;

    pub fn new() -> Self {
        return Self {
            instructions: Vec::new(),
            starting_offset: 0,
            sha2: false,
        };
    }

    pub fn add_instructions(mut self, mut instructions: Vec<Instruction>) -> Self {
        if self.instructions.is_empty() {
            self.instructions = instructions;
        } else {
            self.instructions.append(&mut instructions);
        }

        return self;
    }

    pub fn with_starting_offset(mut self, starting_offset: usize) -> Self {
        self.starting_offset = starting_offset;

        return self;
    }

    pub fn set_sha2(mut self) -> Self {
        self.sha2 = true;

        return self;
    }

    pub fn set_sha3(mut self) -> Self {
        self.sha2 = false;

        return self;
    }

    fn raw_bytes(self) -> Vec<u8> {
        let mut output = Vec::new();

        for instruction in self.instructions {
            output.append(&mut instruction.into());
        }

        return output;
    }

    pub fn assemble_vxl_file(self) -> VXLFile {
        let starting_offset = self.starting_offset as u64;
        let flag = if self.sha2 { 0 } else { 1 };
        let output = self.raw_bytes();

        let checksum;

        if flag & 0b1 == 0 {
            checksum = Self::calculate_checksum(sha2::Sha224::new(), &output);
        } else {
            checksum = Self::calculate_checksum(sha3::Sha3_224::new(), &output);
        }

        let header = VXLHeader::new(
            Self::VERSION,
            output.len() as u64,
            starting_offset,
            flag,
            checksum,
        );

        return VXLFile::new(header, output);
    }

    pub fn dump_raw_bytes(self) -> Vec<u8> {
        return self.raw_bytes();
    }

    fn calculate_checksum<D: Digest>(mut digest: D, bytes: &Vec<u8>) -> [u8; 28] {
        digest.update(bytes.clone());
        let output: Vec<u8> = digest.finalize().to_vec();

        return output.try_into().unwrap();
    }
}
