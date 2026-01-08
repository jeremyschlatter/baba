---
name: play-baba
description: Play Baba Is You autonomously through an HTTP API.
---

# Playing Baba Is You via LLM API

This skill enables you to play Baba Is You autonomously through an HTTP API.

## Starting the Game

First, start the game with the LLM API enabled:

```bash
# Start from the overworld (recommended)
cargo run -- level levels/ --llm-api --llm-port 8080

# Or start a specific level directly
cargo run -- level levels/0-baba-is-you.txt --llm-api --llm-port 8080
```

The game window will open (for visual observation) and the API will listen on the specified port.

## API Commands

Use the `baba` wrapper script (available in the nix dev shell):

**Get current state:**
```bash
baba state
# or just: baba
```

**Make a move:**
```bash
baba ACTION
```

Valid actions:
- `up`, `down`, `left`, `right` - movement
- `wait` - pass turn
- `undo` - revert last move
- `enter` - enter level (in overworld)

Set `BABA_PORT` environment variable if using a non-default port (default: 8080).

## Understanding the Output

The API returns a text representation of the game state:

```
Legend:
  Entities: ba=baba, fl=flag, wa=wall
  Text nouns: BA=BABA, FL=FLAG, WA=WALL
  Properties: St=STOP, Wi=WIN, Yo=YOU
  Operators: ===IS

Grid:
     01  02  03
 01  ba→ ... fl→
 02  BA→ ==→ Yo→

Stacks (top to bottom):
  (none)

Active rules:
  BABA IS YOU

Status: playing
```

### Grid Notation
- `...` = empty cell
- Two-letter codes = entities (see Legend)
- Arrow suffix (→←↑↓) = entity's facing direction
- Coordinates are (row, column)

### Status Values
- `playing` - normal gameplay, you can move
- `stuck (nothing is YOU - try undo)` - no controllable entity

### Level Transitions
When you complete a level or enter a new one:
- `Level Complete!` prefix when winning
- `Entered: LevelName` prefix when entering a level

## Game Rules

Baba Is You is a puzzle game where **the rules are part of the level**:

1. Rules are formed by text blocks: `NOUN IS PROPERTY`
   - Example: `BABA IS YOU` means you control Baba
   - Example: `FLAG IS WIN` means touching the flag wins

2. You can push text blocks to change rules!
   - Push `YOU` away from `BABA IS YOU` and you can't move
   - Push `WIN` to `ROCK IS WIN` and rocks become the goal

3. Key properties:
   - `YOU` - entity you control
   - `WIN` - touching it wins the level
   - `STOP` - blocks movement
   - `PUSH` - can be pushed
   - `DEFEAT` - destroys YOU on contact
   - `SINK` - destroys anything that enters (including itself)

## Strategy Tips

1. First, read the active rules to understand what you control and what wins
2. Look at text blocks in the grid - can you rearrange them?
3. If stuck, `undo` is your friend
4. Sometimes the solution requires breaking "obvious" rules
