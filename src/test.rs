pub async fn record_golden(level: &str, output: &str) {
    let sprites = crate::game::load_sprite_map().await;
    let result = crate::game::play_level(&sprites, level).await;
    if let crate::game::LevelResult::Win(history) = result {
        crate::game::save(&history, &format!("goldens/{output}.ron.br")).unwrap();
    }
}
