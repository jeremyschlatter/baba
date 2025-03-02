use std::process::{Command, ExitCode, Stdio};

use clap::{Parser, Subcommand};
use futures_util::Future;

#[derive(Parser)]
#[command(about = "Dev-only game entry points")]
struct Cli {
    #[clap(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    #[command(about = "Record a golden session")]
    Golden {
        level: String,
        output: String,
    },
    #[command(about = "Render a diff")]
    Diff {
        diff: String,
    },
    #[command(about = "View a replay file")]
    Replay {
        replay: String,
    },
    #[command(about = "Run the main binary")]
    Run {
        #[arg(trailing_var_arg = true)]
        args: Vec<String>,
    },
}
use Commands::*;

fn with_window(f: impl Future<Output = ()> + 'static) -> u8 {
    macroquad::Window::from_config(
        macroquad::prelude::Conf {
            ..Default::default()
        },
        f,
    );
    0
}

fn main() -> ExitCode {
    match Cli::parse().command {
        Golden{ level, output } => with_window(game::record_golden(level, output)),
        Diff { diff } => with_window(game::render_diff(diff)),
        Replay { replay } => with_window(game::replay(replay)),
        Run { args } =>
            Command::new("run")
                .args(args)
                .stdin(Stdio::inherit())
                .stdout(Stdio::inherit())
                .stderr(Stdio::inherit())
                .status()
                .map_or(1, |status| status.code().unwrap_or(1) as u8),
    }.into()
}
