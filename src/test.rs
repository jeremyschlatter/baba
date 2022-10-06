use std::io::Write;

pub async fn record_golden(level: &str) {
    let output = std::fs::File::create("replay.ron.br").unwrap();
    let history = crate::game::main(Some(level)).await;
    let serialized = ron::to_string(&history).unwrap();

    brotli::CompressorWriter::new(output, 4096, 9, 20).write_all(serialized.as_bytes()).unwrap();
}
