pub async fn record_golden(level: &str) {
    let history = crate::game::main(Some(level)).await;
    crate::game::save_replay(&history, "replay.ron.br").unwrap();
}
