use criterion::{black_box, criterion_group, criterion_main, Criterion};
use game::{parse_level};

fn criterion_benchmark(c: &mut Criterion) {
    let s = "levels/4-forest-of-fall/10-not-there.txt";
    c.bench_function("parse level 4-10", |b| b.iter(|| parse_level(black_box(&s))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
