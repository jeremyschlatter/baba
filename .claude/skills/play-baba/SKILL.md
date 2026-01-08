---
name: play-baba
description: Play Baba Is You autonomously through an HTTP API.
---

# Playing Baba Is You via LLM API

This skill enables you to play Baba Is You autonomously through an HTTP API.

## Picking a level

You can browse available levels in the levels/ directory. I recommend one of the first eight:

levels/0-baba-is-you.txt
levels/1-where-do-i-go.txt
levels/2-now-what-is-this.txt
levels/3-out-of-reach.txt
levels/4-forest-of-fall/
levels/5-deep-forest/
levels/6-off-limits.txt
levels/7-grass-yard.txt

## Starting the Game

Start the game with the LLM API enabled:

```bash
start-baba <path/to/level.txt>
```

This builds and launches the game from the overworld with the API listening on port 8080 (or `$BABA_PORT` if set). The game window will open for visual observation.

This command waits for the game to start. When it returns, the game is immediately playable.

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
- `enter` - enter level (only works in an overworld)

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
