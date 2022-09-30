mod game;

use clap::{Parser, Subcommand};

#[derive(Parser)]
struct Cli {
    #[clap(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    #[command(about = "Record a golden session")]
    Golden,
}

#[macroquad::main("Baba Is Clone")]
async fn main() {
    let cli = Cli::parse();

    game::main(
        match cli.command {
            Some(Commands::Golden) => game::Mode::Golden,
            None => game::Mode::Normal,
        }
    ).await
}
