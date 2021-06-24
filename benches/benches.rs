use std::fs::File;
use std::io::Read;

use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use hashbrown::{HashMap, HashSet};
use vxasm::lexer::Lexer;
use vxasm::pre_processor::PreProcessor;
use vxasm::text_mapping::FileInfoManager;
use vxasm::instruction_parser::InstructionParser;
use vxasm::parser::Parser;
use vxasm::assembler::Assembler;

fn benchmark_lexer(c: &mut Criterion) {
    c.bench_function("sample_program_unsigned", |b| {
        b.iter_batched(
            || {
                let input = "ldi 0u32, $r1\nmalloc $r0, $r1\nmalloc $r0, $r1\nfree 0u0\nfree 0u1\n";
                let mut f_man = FileInfoManager::new();
                let f = f_man.new_file(String::new(), input.to_string());

                (input.chars().into_iter().collect(), f)
            },
            |(chars, f)| {
                Lexer::tokenize(chars, f).unwrap();
            },
            BatchSize::SmallInput,
        );
    });

    c.bench_function("sample_program_hex", |b| {
        b.iter_batched(
            || {
                let input = "ldi 0x32, $r1\nmalloc $r0, $r1\nmalloc $r0, $r1\nfree h0\nfree 0x1\n";
                let mut f_man = FileInfoManager::new();
                let f = f_man.new_file(String::new(), input.to_string());

                (input.chars().into_iter().collect(), f)
            },
            |(chars, f)| {
                Lexer::tokenize(chars, f).unwrap();
            },
            BatchSize::SmallInput,
        );
    });

    c.bench_function("sample_program_bin", |b| {
        b.iter_batched(
            || {
                let input =
                    "ldi 0b110010, $r1\nmalloc $r0, $r1\nmalloc $r0, $r1\nfree 0b0\nfree b1\n";
                let mut f_man = FileInfoManager::new();
                let f = f_man.new_file(String::new(), input.to_string());

                (input.chars().into_iter().collect(), f)
            },
            |(chars, f)| {
                Lexer::tokenize(chars, f).unwrap();
            },
            BatchSize::SmallInput,
        );
    });

    c.bench_function("sample_program_float", |b| {
        b.iter_batched(
            || {
                let input =
                    "ldf 0f123.44, $r1\nmalloc $r0, $r1\nmalloc $r0, $r1\nfree 0b0\nfree 0b1\n";
                let mut f_man = FileInfoManager::new();
                let f = f_man.new_file(String::new(), input.to_string());

                (input.chars().into_iter().collect(), f)
            },
            |(chars, f)| {
                Lexer::tokenize(chars, f).unwrap();
            },
            BatchSize::SmallInput,
        );
    });

    c.bench_function("sample_program_large_float", |b| {
        b.iter_batched(
            || {
                let input =
                    "ldf 0f12373847.442738297489273498729, $r1\nmalloc $r0, $r1\nmalloc $r0, $r1\nfree 0b0\nfree 0b1\n";
                let mut f_man = FileInfoManager::new();
                let f = f_man.new_file(String::new(), input.to_string());

                (input.chars().into_iter().collect(), f)
            },
            |(chars, f)| {
                Lexer::tokenize(chars, f).unwrap();
            },
            BatchSize::SmallInput,
        );
    });

    let mut input = String::new();
    File::open("sample.vsm")
        .unwrap()
        .read_to_string(&mut input)
        .unwrap();

    c.bench_function("lexer_sample.vsm", |b| {
        b.iter_batched(
            || {
                let mut f_man = FileInfoManager::new();
                let f = f_man.new_file(String::new(), input.clone());

                (input.clone().chars().into_iter().collect(), f)
            },
            |(chars, f)| {
                Lexer::tokenize(chars, f).unwrap();
            },
            BatchSize::SmallInput,
        );
    });
}

fn benchmark_preprocessor(c: &mut Criterion) {
    c.bench_function("preprocessor_no_changes", |b| {
        b.iter_batched(
            || {
                let input = "ldi 0u32, $r1\nmalloc $r0, $r1\nmalloc $r0, $r1\nfree 0u0\nfree 0u1\n";
                let mut f_man = FileInfoManager::new();
                let f = f_man.new_file(String::new(), input.to_string());
                let tokens_out =
                    Lexer::tokenize(input.chars().into_iter().collect(), f.clone()).unwrap();
                let mut tokens = HashMap::new();

                tokens.insert(f.clone(), tokens_out);

                (tokens, HashSet::new(), f)
            },
            |(tokens, flags, f)| {
                let processor = PreProcessor::new(tokens, flags);
                processor.run(&f).unwrap();
            },
            BatchSize::SmallInput,
        );
    });

    c.bench_function("preprocessor_single_successful_if", |b| {
        b.iter_batched(
            || {
                let input = "%if FLAG\nldi 0u32, $r1\nmalloc $r0, $r1\nmalloc $r0, $r1\nfree 0u0\nfree 0u1\n%end_if";
                let mut f_man = FileInfoManager::new();
                let f = f_man.new_file(String::new(), input.to_string());

                let mut flags = HashSet::new();
                flags.insert("FLAG".to_string());

                let tokens_out = Lexer::tokenize(input.chars().into_iter().collect(), f.clone()).unwrap();
                let mut tokens = HashMap::new();

                tokens.insert(f.clone(), tokens_out);

                (
                    tokens,
                    flags,
                    f,
                )
            },
            |(tokens, flags, f)| {
                let processor = PreProcessor::new(tokens, flags);
                processor.run(&f).unwrap();
            },
            BatchSize::SmallInput,
        );
    });

    c.bench_function("preprocessor_single_failed_if", |b| {
        b.iter_batched(
            || {
                let input = "%if FLAG\nldi 0u32, $r1\nmalloc $r0, $r1\nmalloc $r0, $r1\nfree 0u0\nfree 0u1\n%end_if";
                let mut f_man = FileInfoManager::new();
                let f = f_man.new_file(String::new(), input.to_string());
                
                let flags = HashSet::new();

                let tokens_out = Lexer::tokenize(input.chars().into_iter().collect(), f.clone()).unwrap();
                let mut tokens = HashMap::new();

                tokens.insert(f.clone(), tokens_out);

                (
                    tokens,
                    flags,
                    f,
                )
            },
            |(tokens, flags, f)| {
                let processor = PreProcessor::new(tokens, flags);
                processor.run(&f).unwrap();
            },
            BatchSize::SmallInput,
        );
    });

    c.bench_function("preprocessor_small_repeat", |b| {
        b.iter_batched(
            || {
                let input = "%repeat 2\nldi 0u32, $r1\nmalloc $r0, $r1\nmalloc $r0, $r1\nfree 0u0\nfree 0u1\n%end_repeat";
                let mut f_man = FileInfoManager::new();
                let f = f_man.new_file(String::new(), input.to_string());
                
                let flags = HashSet::new();

                let tokens_out = Lexer::tokenize(input.chars().into_iter().collect(), f.clone()).unwrap();
                let mut tokens = HashMap::new();

                tokens.insert(f.clone(), tokens_out);

                (
                    tokens,
                    flags,
                    f,
                )
            },
            |(tokens, flags, f)| {
                let processor = PreProcessor::new(tokens, flags);
                processor.run(&f).unwrap();
            },
            BatchSize::SmallInput,
        );
    });

    c.bench_function("preprocessor_large_repeat", |b| {
        b.iter_batched(
            || {
                let input = "%repeat 150\nldi 0u32, $r1\nmalloc $r0, $r1\nmalloc $r0, $r1\nfree 0u0\nfree 0u1\n%end_repeat";
                let mut f_man = FileInfoManager::new();
                let f = f_man.new_file(String::new(), input.to_string());
                
                let flags = HashSet::new();
                let tokens_out = Lexer::tokenize(input.chars().into_iter().collect(), f.clone()).unwrap();
                let mut tokens = HashMap::new();

                tokens.insert(f.clone(), tokens_out);

                (
                    tokens,
                    flags,
                    f,
                )
            },
            |(tokens, flags, f)| {
                let processor = PreProcessor::new(tokens, flags);
                processor.run(&f).unwrap();
            },
            BatchSize::SmallInput,
        );
    });

    c.bench_function("preprocessor_import", |b| {
        b.iter_batched(
            || {
                let mut f_man = FileInfoManager::new();

                let flags = HashSet::new();
                let mut tokens = HashMap::new();

                let mut root_input = String::new();

                for i in 0..50 {
                    let input =
                        "ldi 0u32, $r1\nmalloc $r0, $r1\nmalloc $r0, $r1\nfree 0u0\nfree 0u1\n";
                    let f = f_man.new_file(i.to_string(), input.to_string());

                    let tokens_out =
                        Lexer::tokenize(input.chars().into_iter().collect(), f.clone()).unwrap();
                    tokens.insert(f.clone(), tokens_out);
                    root_input.push_str(&format!("%import \"{}\"\n", i));
                }

                let root_f = f_man.new_file("root".to_string(), root_input.clone());
                let tokens_out =
                    Lexer::tokenize(root_input.chars().into_iter().collect(), root_f.clone())
                        .unwrap();
                tokens.insert(root_f.clone(), tokens_out);

                (tokens, flags, root_f)
            },
            |(tokens, flags, f)| {
                let processor = PreProcessor::new(tokens, flags);
                processor.run(&f).unwrap();
            },
            BatchSize::SmallInput,
        );
    });

    let mut input = String::new();
    File::open("sample.vsm")
        .unwrap()
        .read_to_string(&mut input)
        .unwrap();

    c.bench_function("preprocessor_sample.vsm", |b| {
        b.iter_batched(
            || {
                let mut f_man = FileInfoManager::new();

                let flags = HashSet::new();
                let mut tokens = HashMap::new();

                let root_f = f_man.new_file("sample.vsm".to_string(), input.clone());
                let tokens_out =
                    Lexer::tokenize(input.clone().chars().into_iter().collect(), root_f.clone())
                        .unwrap();
                tokens.insert(root_f.clone(), tokens_out);

                (tokens, flags, root_f)
            },
            |(tokens, flags, f)| {
                let processor = PreProcessor::new(tokens, flags);
                processor.run(&f).unwrap();
            },
            BatchSize::SmallInput,
        );
    });
}

fn benchmark_parser(c: &mut Criterion) {
    let mut input = String::new();
    File::open("sample.vsm")
        .unwrap()
        .read_to_string(&mut input)
        .unwrap();

    c.bench_function("parser_sample.vsm", |b| {
        b.iter_batched(
            || {
                let mut f_man = FileInfoManager::new();

                let flags = HashSet::new();
                let mut tokens = HashMap::new();

                let root_f = f_man.new_file("sample.vsm".to_string(), input.clone());
                let tokens_out =
                    Lexer::tokenize(input.clone().chars().into_iter().collect(), root_f.clone())
                        .unwrap();
                tokens.insert(root_f.clone(), tokens_out);

                let processor = PreProcessor::new(tokens, flags);
                processor.run(&root_f).unwrap()
            },
            |tokens| {
                let mut parser = InstructionParser::new();
                parser.parse(tokens).unwrap();
            },
            BatchSize::SmallInput,
        );
    });
}

fn benchmark_assembler(c: &mut Criterion) {
    let mut input = String::new();
    File::open("sample.vsm")
        .unwrap()
        .read_to_string(&mut input)
        .unwrap();

    c.bench_function("assembler_sample.vsm", |b| {
        b.iter_batched(
            || {
                let mut f_man = FileInfoManager::new();

                let flags = HashSet::new();
                let mut tokens = HashMap::new();

                let root_f = f_man.new_file("sample.vsm".to_string(), input.clone());
                let tokens_out =
                    Lexer::tokenize(input.clone().chars().into_iter().collect(), root_f.clone())
                        .unwrap();
                tokens.insert(root_f.clone(), tokens_out);

                let processor = PreProcessor::new(tokens, flags);
                let tokens = processor.run(&root_f).unwrap();
                let mut parser = InstructionParser::new();
                parser.parse(tokens).unwrap();

                parser.into_instructions()
            },
            |instructions| {
                let f = Assembler::new().add_instructions(instructions).set_sha3().assemble_vxl_file();
            },
            BatchSize::SmallInput,
        );
    });
}

criterion_group!(benches, benchmark_lexer, benchmark_preprocessor, benchmark_parser, benchmark_assembler);

criterion_main!(benches);
