use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use vxasm::lexer::Lexer;

fn benchmark_sample_program(c: &mut Criterion) {
    c.bench_function("sample_program_1", |b| {
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
}

criterion_group!(benches, benchmark_sample_program,);

criterion_main!(benches);
