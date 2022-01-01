mod cli_args;

use cli_args::{AssemblyFormat, CLIArgs};

use clap::StructOpt;
use std::fs::File;

use hashbrown::{HashMap, HashSet};
use std::io::{ErrorKind, Read, Write};
use std::iter::FromIterator;
use vxlasm::error::VXASMError;
use vxlasm::processing::{Assembler, Lexer, Parser, PreProcessor};
use vxlasm::text_mapping::FileInfoManager;

fn main() {
    let cli_options = CLIArgs::parse();

    let flags = HashSet::from_iter(cli_options.flags);

    let mut file_manager = FileInfoManager::new();
    let mut files = HashSet::new();
    let mut tokens = HashMap::new();

    for path in cli_options.input_files {
        let mut f = match File::open(&path) {
            Ok(f) => f,
            Err(e) => match e.kind() {
                ErrorKind::NotFound => {
                    eprintln!("The file {} was not found.", path);
                    return;
                }
                _ => {
                    eprintln!("Failed to open {}, OS reason: {}", path, e.to_string());
                    return;
                }
            },
        };

        let mut content = String::new();

        if let Err(e) = f.read_to_string(&mut content) {
            eprintln!("Failed to read {}, OS reason: {}", path, e.to_string());
            return;
        }

        if files.contains(&path) {
            eprintln!("The file {} has been repeated multiple times.", path);
            return;
        }

        let f = file_manager.new_file(path.clone(), content);
        files.insert(path);

        tokens.insert(
            f.clone(),
            match Lexer::tokenize(f) {
                Ok(t) => t,
                Err(e) => {
                    report_error(&e);

                    return;
                }
            },
        );
    }

    let root_f;

    if let Some(r) = cli_options.root_file {
        if let Some(f) = file_manager.get_file_info(&r) {
            root_f = f;
        } else {
            eprintln!("The file {} has not been included.", r);
            return;
        }
    } else {
        root_f = file_manager.get_file_info_refs()[0].clone();
    }

    let (processed_tokens, offset) = {
        if let Some(entry_point) = cli_options.entry_point {
            match PreProcessor::new(tokens, flags).run_with_tracking(&root_f, &entry_point) {
                Ok(t) => t,
                Err(e) => {
                    report_error(&e);
                    return;
                }
            }
        } else {
            (
                match PreProcessor::new(tokens, flags).run(&root_f) {
                    Ok(t) => t,
                    Err(e) => {
                        report_error(&e);
                        return;
                    }
                },
                0,
            )
        }
    };

    let instructions = match Parser::with_tokens(processed_tokens).parse() {
        Ok(i) => i,
        Err(e) => {
            report_error(&e);
            return;
        }
    };

    let mut assembler = Assembler::new()
        .add_instructions(instructions)
        .with_starting_offset(offset as usize);

    if cli_options.sha2 {
        assembler = assembler.set_sha2();
    } else {
        assembler = assembler.set_sha3();
    }

    let output: Vec<u8> = match cli_options.format {
        AssemblyFormat::Raw => assembler.dump_raw_bytes(),
        AssemblyFormat::Xvl => assembler.assemble_vxl_file().into(),
    };

    match File::create(cli_options.output_file) {
        Ok(mut f) => {
            if let Err(e) = f.write_all(&output) {
                eprintln!("Failed to write to output file. OS Error: {}", e);
                return;
            }
        }
        Err(e) => {
            eprintln!("Failed to create output file. OS Error: {}", e);
            return;
        }
    }
}

fn report_error(err: &dyn VXASMError) {
    eprintln!("{}", err.reportable_format());
}
