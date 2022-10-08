pub async fn record_golden(level: &str, output: &str) {
    let history = crate::game::main(Some(level)).await;
    crate::game::save(&history, &format!("goldens/{output}.ron.br")).unwrap();
}
