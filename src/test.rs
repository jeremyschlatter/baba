pub async fn record_golden(level: &str) {
    let history = crate::game::main(Some(level)).await;
    println!("golden {level}");
}
