use clap::{Parser, Subcommand};
use std::sync::mpsc;
use std::thread;

#[derive(Parser)]
struct Cli {
    #[clap(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    #[command(about = "Record a golden session")]
    Golden { level: String, output: String },
    #[command(about = "Play a specific level")]
    Level {
        level: String,
        #[arg(long, help = "Enable HTTP API for LLM-controlled gameplay")]
        llm_api: bool,
        #[arg(long, default_value = "8080", help = "Port for LLM API server")]
        llm_port: u16,
    },
    #[command(about = "View a replay file")]
    Replay { replay: String },
    #[command(about = "Render a diff (for testing)")]
    RenderDiff { diff: String },
    #[command(about = "Render a level in LLM-optimized text format")]
    RenderLlm { level: String },
}
use Commands::*;

#[macroquad::main("Baba Is Clone")]
async fn main() {
    let cli = Cli::parse();

    match cli.command {
        None => {
            game::play_overworld("levels/", None).await;
        }
        Some(c) => match c {
            Level { level, llm_api, llm_port } => {
                let llm_ctx = if llm_api {
                    let (cmd_tx, cmd_rx) = mpsc::channel();
                    let (resp_tx, resp_rx) = mpsc::channel();
                    let (startup_tx, startup_rx) = mpsc::channel();
                    thread::spawn(move || game::run_llm_server(cmd_tx, resp_rx, llm_port, startup_tx));
                    if let Err(e) = startup_rx.recv().expect("Server thread died unexpectedly") {
                        eprintln!("{}", e);
                        std::process::exit(1);
                    }
                    Some(game::LlmContext::new(cmd_rx, resp_tx))
                } else {
                    None
                };
                game::play_overworld(&level, llm_ctx).await;
            }
            Golden { level, output } => game::record_golden(&level, &output).await,
            Replay { replay } => game::replay(&replay).await,
            RenderDiff { diff } => game::render_diff(&diff).await,
            RenderLlm { level } => {
                let output = game::render_level_for_llm(&level);
                print!("{}", output);
            }
        },
    }
}
