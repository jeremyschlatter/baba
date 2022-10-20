pub async fn record_golden(level: &str, output: &str) {
    let sprites = game::load_sprite_map().await;
    let result = game::play_level(&sprites, level).await;
    if let game::LevelResult::Win(history) = result {
        game::save(&history, &format!("goldens/{output}.ron.br")).unwrap();
    }
}
