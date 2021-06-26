use clap::{App, Arg};
use std::fs::File;

use hashbrown::{HashMap, HashSet};
use std::io::{ErrorKind, Read, Write};
use std::iter::FromIterator;
use vxasm::assembler::Assembler;
use vxasm::error::VXASMError;
use vxasm::lexer::Lexer;
use vxasm::parser::Parser;
use vxasm::pre_processor::PreProcessor;
use vxasm::text_mapping::FileInfoManager;

const DEFAULT_OUTPUT_FILE: &str = "out.xvl";
const DEFAULT_FORMAT: &str = "xvl";

fn main() {
    let matches = App::new("vxasm")
        .about("Assembles .vsm files into files executable by the voxeol virtual machine.")
        .arg(
            Arg::with_name("format")
                .required(true)
                .default_value(DEFAULT_FORMAT)
                .takes_value(true)
                .short("f")
                .long("format")
                .possible_values(&["raw", "xvl"]),
        )
        .arg(
            Arg::with_name("input-files")
                .takes_value(true)
                .min_values(1)
                .multiple(true)
                .required(true),
        )
        .arg(
            Arg::with_name("output-file")
                .takes_value(true)
                .multiple(false)
                .default_value(DEFAULT_OUTPUT_FILE)
                .short("o"),
        )
        .arg(
            Arg::with_name("root-file")
                .takes_value(true)
                .multiple(false)
                .short("r"),
        )
        .arg(
            Arg::with_name("flags")
                .takes_value(true)
                .multiple(true)
                .short("F"),
        )
        .arg(Arg::with_name("sha2").takes_value(false).long("sha2"))
        .arg(Arg::with_name("sha3").takes_value(false).long("sha3"))
        .arg(
            Arg::with_name("entry-point")
                .takes_value(true)
                .multiple(false)
                .long("entry"),
        )
        .get_matches();

    let input_files: Vec<&str>;
    let output_file = matches
        .value_of("output-file")
        .unwrap_or(DEFAULT_OUTPUT_FILE);
    let format = matches.value_of("format").unwrap_or(DEFAULT_FORMAT);
    let flags = matches
        .values_of("flags")
        .map(|v| HashSet::from_iter(v.map(|v| v.to_string())))
        .unwrap_or(HashSet::new());
    let entry_point = matches.value_of("entry-point").map(|s| s.to_string());

    if let Some(inp) = matches.values_of("input-files") {
        input_files = inp.into_iter().collect();
    } else {
        eprintln!("No input files were supplied.");
        return;
    }

    if input_files.is_empty() {
        eprintln!("No input files were supplied.");
        return;
    }

    let mut file_manager = FileInfoManager::new();
    let mut files = HashSet::new();
    let mut tokens = HashMap::new();

    for path in input_files {
        let mut f = match File::open(path) {
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

        if files.contains(path) {
            eprintln!("The file {} has been repeated multiple times.", path);
            return;
        }

        let f = file_manager.new_file(path.to_string(), content);
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

    if let Some(r) = matches.value_of("root-file") {
        if let Some(f) = file_manager.get_file_info(r) {
            root_f = f;
        } else {
            eprintln!("The file {} has not been included.", r);
            return;
        }
    } else {
        root_f = file_manager.get_file_info_refs()[0].clone();
    }

    let (processed_tokens, offset) = {
        if let Some(entry_point) = entry_point {
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

    if matches.is_present("sha2") {
        assembler = assembler.set_sha2();
    } else {
        assembler = assembler.set_sha3();
    }

    let output: Vec<u8>;

    if format == "xvl" {
        output = assembler.assemble_vxl_file().into();
    } else if format == "raw" {
        output = assembler.dump_raw_bytes();
    } else {
        eprintln!("Unknown format {}", format);
        return;
    }

    File::create(output_file)
        .unwrap()
        .write_all(&output)
        .unwrap();
}

fn report_error(err: &dyn VXASMError) {
    eprintln!("{}", err.reportable_format());
}
