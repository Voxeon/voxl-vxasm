use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use vxasm::lexer::Lexer;
use vxasm::text_mapping::FileInfoManager;

fn benchmark_sample_program(c: &mut Criterion) {
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
}

criterion_group!(benches, benchmark_sample_program,);

criterion_main!(benches);
