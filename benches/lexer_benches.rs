use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use vxasm::lexer::Lexer;

fn benchmark_sample_program(c: &mut Criterion) {
    c.bench_function("sample_program_dec", |b| {
        b.iter_batched(
            || {
                "ldi 32, $r1\nmalloc $r0, $r1\nmalloc $r0, $r1\nfree 0\nfree 1\n"
                    .chars()
                    .into_iter()
                    .collect()
            },
            |chars| {
                Lexer::tokenize(chars, String::new()).unwrap();
            },
            BatchSize::SmallInput,
        );
    });

    c.bench_function("sample_program_hex", |b| {
        b.iter_batched(
            || {
                "ldi h32, $r1\nmalloc $r0, $r1\nmalloc $r0, $r1\nfree h0\nfree h1\n"
                    .chars()
                    .into_iter()
                    .collect()
            },
            |chars| {
                Lexer::tokenize(chars, String::new()).unwrap();
            },
            BatchSize::SmallInput,
        );
    });

    c.bench_function("sample_program_bin", |b| {
        b.iter_batched(
            || {
                "ldi b110010, $r1\nmalloc $r0, $r1\nmalloc $r0, $r1\nfree b0\nfree b1\n"
                    .chars()
                    .into_iter()
                    .collect()
            },
            |chars| {
                Lexer::tokenize(chars, String::new()).unwrap();
            },
            BatchSize::SmallInput,
        );
    });
}

criterion_group!(benches, benchmark_sample_program,);

criterion_main!(benches);
