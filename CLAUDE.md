# Baba Is Clone

This is a homemade clone of the video game Baba Is You.

I started building it a few years ago as a challenge to test the depth of my
understanding of the game mechanics. I worked on it in a flurry for about a
month, building 4 of the game's overworld sections, 82 individual levels, and
22 of the game logic properties (You, Stop, Push, Win, etc).

I stalled out when it came time to implement Swap. I was unsatisfied with how
complicated the movement logic had become, and had an intuition that I could do
it in a better way. I started working on a major refactor of the movement logic,
but never completed it.

I keep wanting to return to this project, but feel stuck on it.

I am otherwise super happy with this project. My recreation is graphically
nearly identical with the original (relatively easy to do because of the sprite-
and grid-based rendering), the mechanics of the first 82 levels are likewise
identical, and I've even fixed a couple rendering issues in the original that
had always bothered me. It loads instantly, I have excellent replay tests, and
I have a very satisfying text-based level editing system where each level is
a simple text file.

## Build & Run Commands
- Build: `cargo build`
- Run: `cargo run`
- Play specific level: `cargo run -- level levels/path/to/level.txt`
- Run tests: `cargo test`
- Run single test: `cargo test test_name`
- Run benchmarks: `cargo bench`
- Run specific benchmark: `cargo bench -- bench_name`

## Code Style Guidelines
- Use 4-space indentation
- Prefer enum-based pattern matching over if/else chains
- Group imports logically, separate std imports from external crates
- Use the `#[derive_the_basics]` macro for common trait implementations
- Implement error handling using anyhow::Result
- Use `#[derive(Serialize, Deserialize)]` for serializable types
- Prefer newtype patterns over primitive types
- Name conventions: snake_case for variables/functions, CamelCase for types
- Prefer Result return types over panics
- Prefer statically typed values over stringly typed interfaces

## Project Architecture
- Main library in src/game.rs (~3100 lines)
- Core gameplay in src/main.rs
- Level files stored in levels/ directory

### Core Types (src/game.rs)
- `Level` = `Vec<Vec<Cell>>` — 2D grid of cells
- `Cell` = `Vec<LiveEntity>` — stack of entities at one position
- `LiveEntity` — entity with direction and unique ID
- `Entity` — either `Noun(Noun)` or `Text(Text)`
- `Noun` — 70+ game objects (Baba, Wall, Key, Flag, etc.)
- `Adjective` — properties (You, Stop, Push, Win, Sink, etc.)
- `Input` — `Go(Direction)`, `Wait`, or `Undo`

### Key Functions
- `step(level, input, n) -> (Level, bool)` — pure game logic, returns new state + win status
- `parse_level(path)` — loads level from text file
- `render_for_llm(level) -> String` — LLM-optimized text renderer
- `play_level()` — main game loop (async, uses macroquad)

### Runtime
- Uses macroquad for windowing, graphics, and async runtime
- Single-threaded event loop with `next_frame().await`
- Input debouncing via `debounce()` function
