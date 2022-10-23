pub async fn record_golden(level: &str, output: &str) {
    let sprites = game::load_sprite_map();
    let (result, _) = game::play_level(&sprites, (0., None), level, None).await;
    if let game::LevelResult::Win(history) = result {
        game::save(&history, &format!("goldens/{output}.ron.br")).unwrap();
    }
}
