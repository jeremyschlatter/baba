pub async fn record_golden(level: &str) {
    let _history = crate::game::main(Some(level)).await;
    println!("golden {level}");
}
