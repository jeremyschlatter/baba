mod game;
mod test;

use clap::{Parser, Subcommand};

#[derive(Parser)]
struct Cli {
    #[clap(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    #[command(about = "Record a golden session")]
    Golden {
        level: String,
    },
    #[command(about = "Play a specific level")]
    Level {
        level: String,
    },
}

#[macroquad::main("Baba Is Clone")]
async fn main() {
    let cli = Cli::parse();

    match cli.command {
        None => { game::main(None).await; },
        Some(Commands::Level { level }) => { game::main(Some(&level)).await; },
        Some(Commands::Golden{ level }) => test::record_golden(&level).await,
    }
}
