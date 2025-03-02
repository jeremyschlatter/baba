use clap::{Parser, Subcommand};

#[derive(Parser)]
struct Cli {
    #[clap(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    #[command(about = "Play a specific level")]
    Level {
        level: String,
    },
}
use Commands::*;

#[macroquad::main("Baba Is Clone")]
async fn main() {
    let cli = Cli::parse();

    match cli.command {
        None => { game::play_overworld("levels/").await; },
        Some(c) => match c {
            Level { level } => { game::play_overworld(&level).await; },
        }
    }
}
