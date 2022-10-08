#[macro_use]
extern crate lazy_static;

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
        output: String,
    },
    #[command(about = "Play a specific level")]
    Level {
        level: String,
    },
    #[command(about = "View a replay file")]
    Replay {
        replay: String,
    },
    #[command(about = "Render a diff (for testing)")]
    RenderDiff {
        diff: String,
    },
}
use Commands::*;

#[macroquad::main("Baba Is Clone")]
async fn main() {
    let cli = Cli::parse();

    match cli.command {
        None => { game::main(None).await; },
        Some(c) => match c {
            Level { level } => { game::main(Some(&level)).await; },
            Golden{ level, output } => test::record_golden(&level, &output).await,
            Replay { replay } => game::replay(&replay).await,
            RenderDiff { diff } => game::render_diff(&diff).await,
        }
    }
}
