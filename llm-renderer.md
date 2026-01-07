# LLM-Optimized Renderer for Baba Is You Clone

## Goal
Create a text-based renderer optimized for LLM perception of game state, enabling accurate understanding of grid positions, object types, stacked objects, and active rules.

## Design Decisions

### Character-Space Alignment
- Grid aligned in character space (not token space)
- Training data (ASCII art, tables, code) was character-aligned, so LLMs likely have better 2D perception in character space

### Cell Format: 3 Characters
- 2-char code + 1-char direction arrow (→←↑↓)
- Every entity has a direction; default is → (right)
- Empty cells: `...`

### Code Case Conventions
| Type | Case | Examples |
|------|------|----------|
| Entity | lowercase | `ba`, `wa`, `ro` |
| Text-noun | UPPERCASE | `BA`, `WA`, `RO` |
| Text-property | Mixed Case | `Yo`, `St`, `Pu`, `Wi` |
| Operator | Symbols | `==`, `&&`, `~~`, `!!` |

### Row/Column Headers
- Both use 2-digit numbers: 01-99
- Supports grids up to 99×99

### Legend
- Always included
- Maps 2-char codes to full names
- Grouped by type (entities, text-nouns, properties, operators)

### Stacks
- Listed below grid for cells with 2+ objects
- Format: `(row,col): code→, code→, ...` (top to bottom)

### Rules
- Active rules listed in plain English (e.g., "BABA IS YOU")
- Canceled rules listed separately

---

## Output Format Specification

```
Legend:
  Entities: ba=baba, wa=wall, fl=flag, ti=tile
  Text nouns: BA=BABA, WA=WALL, FL=FLAG
  Properties: Yo=YOU, St=STOP, Pu=PUSH, Wi=WIN
  Operators: ==IS, &&=AND, ~~=HAS, !!=NOT

Grid:
     01  02  03  04  05  06
 01  ... wa→ wa→ wa→ wa→ ...
 02  ... ... ... ... ... ...
 03  BA→ ==→ Yo→ ... ... fl→
 04  ... ... ba→ ... ... ...
 05  WA→ ==→ St→ ... ... ...

Stacks (top to bottom):
  (03,01): BA→, ti→

Active rules:
  BABA IS YOU
  WALL IS STOP

Canceled rules:
  (none)
```

---

## Implementation Plan

### Files to Modify/Create
- `src/game.rs` - Add LLM renderer function(s)

### Implementation Steps

1. **Create code mapping utilities**
   - Function to generate 2-char codes for entities present in a level
   - Handle conflicts by varying second character
   - Separate mappings for entities, text-nouns, properties, operators

2. **Create legend generator**
   - Input: set of entities/text objects in current level
   - Output: formatted legend string grouped by type

3. **Create grid renderer**
   - Input: game state (grid of cells with objects)
   - Generate row/column headers
   - Render each cell as 3-char code (handling empty as `...`)
   - Handle direction arrows for all entities

4. **Create stack renderer**
   - Iterate through grid to find cells with 2+ objects
   - Format as `(row,col): obj1, obj2, ...` (top to bottom)

5. **Create rules renderer**
   - Extract active rules from game state
   - Extract canceled rules from game state
   - Format as plain English rule strings

6. **Main render function**
   - Combine all sections into final output string
   - `fn render_for_llm(&self) -> String`

### Code Structure Sketch

```rust
impl GameState {
    pub fn render_for_llm(&self) -> String {
        let code_map = self.build_code_map();
        let mut output = String::new();

        output.push_str(&self.render_legend(&code_map));
        output.push_str(&self.render_grid(&code_map));
        output.push_str(&self.render_stacks(&code_map));
        output.push_str(&self.render_active_rules());
        output.push_str(&self.render_canceled_rules());

        output
    }
}
```

### Testing Strategy
- Render a few existing levels and manually verify output
- Use Anthropic token counting API to analyze tokenization
- Test with LLM to verify perception accuracy (can it correctly identify object positions, count objects, understand rules?)
