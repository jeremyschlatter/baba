use macroquad::prelude::*;

use std::collections::HashSet;

const SPRITE_SIZE : i16 = 37;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Noun {
    Baba,
    Keke,
    Wall,
    Door,
    Key,
    Flag,
    Rock,
    Tile,
}
use Noun::*;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Adjective {
    You,
    Stop,
    Push,
    Win,
}
use Adjective::*;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Text {
    Is,
    And,
    Object(Noun),
    Adjective(Adjective),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Entity {
    Noun(Noun),
    Text(Text),
}

type Cell = Vec<Entity>;
type Level = Vec<Vec<Cell>>;

fn parse_level(name: &str) -> Level {
    let s = std::fs::read_to_string("./levels/".to_owned() + name).unwrap();
    let lines: Vec<&str> = s.lines().skip_while(|l| l.starts_with("#")).collect();
    let max = lines.iter().map(|l| l.len()).max().unwrap_or_default();
    lines.iter()
        .map(
            |l| l.chars()
                .map(|c| match (match c.to_string().to_lowercase().as_str() {
                    "b" => Some(Baba),
                    "r" => Some(Rock),
                    "t" => Some(Tile),
                    "w" => Some(Wall),
                    "f" => Some(Flag),
                    "k" => Some(Keke),
                    "d" => Some(Door),
                    "x" => Some(Key),
                    _ => None,
                }, match c {
                    'y' => Some(You),
                    's' => Some(Stop),
                    'p' => Some(Push),
                    'v' => Some(Win),
                    _ => None
                }) {
                    (Some(noun), _) => if c.is_lowercase() {
                        vec![Entity::Text(Text::Object(noun))]
                    } else {
                        vec![Entity::Noun(noun)]
                    }
                    (_, Some(adj)) => vec![Entity::Text(Text::Adjective(adj))],
                    _ => match c {
                        'i' => vec![Entity::Text(Text::Is)],
                        'a' => vec![Entity::Text(Text::And)],
                        _ => vec![],
                    }
                })
                .chain(std::iter::repeat(vec![]))
                .take(max)
                .collect::<Vec<Vec<Entity>>>())
        .collect()
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Predicate {
    IsNoun(Noun),
    IsAdjective(Adjective),
}
use Predicate::*;

type Rule = (Noun, Predicate);

fn scan_rules_line<'a, I>(rules: &mut Vec<Rule>, line: I)
where
    I: Iterator<Item = &'a Cell>,
{
    enum ScanState {
        HaveSubject(Noun),
        HaveSubjectIs(Noun),
    }
    use ScanState::*;
    let mut state = None;
    for cell in line {
        let text = cell.iter()
            .filter_map(|e| match e {
                Entity::Text(text) => Some(text),
                _ => None,
             })
            .next();
        state = match text {
            Some(text) => match (state, text) {
                (None, Text::Object(noun)) => Some(HaveSubject(*noun)),
                (None, _) => None,
                (Some(HaveSubject(noun)), Text::Is) => Some(HaveSubjectIs(noun)),
                (Some(HaveSubject(_)), Text::Object(noun)) => Some(HaveSubject(*noun)),
                (Some(HaveSubject(_)), _) => None,
                (Some(HaveSubjectIs(noun)), Text::Object(obj)) => {
                    rules.push((noun, IsNoun(*obj)));
                    Some(HaveSubject(noun))
                },
                (Some(HaveSubjectIs(noun)), Text::Adjective(adj)) => {
                    rules.push((noun, IsAdjective(*adj)));
                    None
                },
                (Some(HaveSubjectIs(_)), _) => None,
            },
            None => None,
        }
    }
}

fn scan_rules(l: &Level) -> Vec<Rule> {
    let mut rules = vec![];

    if l.len() > 0 {
        for row in l {
            scan_rules_line(&mut rules, row.iter());
        }
        for col in 0..l[0].len() {
            let mut row = -1;
            scan_rules_line(
                &mut rules,
                std::iter::from_fn(|| {
                    row += 1;
                    if row as usize == l.len() {
                        None
                    } else {
                        Some(&l[row as usize][col])
                    }
                })
            );
        }
    }

    rules
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Input {
    Right,
    Left,
    Up,
    Down,
    Wait,
    Undo,
}
use Input::*;

fn step(l: &Level, input: Input) -> Level {
    let mut level = l.clone();
    let rules = scan_rules(&level);

    let width = level[0].len();
    let height = level.len();

    let adjs = |adj|
        rules.iter()
             .filter_map(|r| match r {
                 (subj, IsAdjective(a)) if *a == adj => Some(subj),
                 _ => None,
              })
             .collect::<HashSet<&Noun>>();

    fn clip(level: &Level, x: i16, y: i16) -> (usize, usize) {
        let width = level[0].len() as i16;
        let height = level.len() as i16;

        (0.max(x).min(width as i16 - 1) as usize,
         0.max(y).min(height as i16 - 1) as usize)
    }

    fn contains(level: &Level, x: usize, y: usize, set: &HashSet<&Noun>) -> bool {
        select(level, x, y, set).count() > 0
    }

    fn select<'a>(level: &'a Level, x: usize, y: usize, set: &'a HashSet<&Noun>) -> impl Iterator<Item=(usize, Entity)> + 'a {
        level[y][x].iter().enumerate().filter(|(_, e)| match e {
            Entity::Noun(n) if set.contains(n) => true,
            _ => false,
        }).map(|(i, e)| (i, *e))
    }

    let stops  = adjs(Stop);
    let pushes = adjs(Push);

    // Move you.
    match match input {
        Left  => Some((-1, 0)),
        Up    => Some((0, -1)),
        Right => Some((1, 0)),
        Down  => Some((0, 1)),
        Wait  => None,
        Undo  => None,
    } {
        None => (),
        Some((dx, dy)) => {
            // Find all nouns that are you.
            let yous = adjs(You);
            // Iterate through all object entities, move the ones that are you.
            let mut movers: Vec<((usize, usize, usize), Entity)> = vec![];
            for y in 0..level.len() {
                'cell_loop: for x in 0..level[y].len() {
                    if !contains(&level, x, y, &yous) {
                        continue;
                    }

                    // attempt to push
                    {
                        let (mut x, mut y) = (x, y);
                        let mut all_pushed = vec![];
                        loop {
                            // stop if adjacent to stop or edge
                            let (x_, y_) = clip(&level, x as i16 + dx, y as i16 + dy);
                            if x == x_ && y == y_ || contains(&level, x_, y_, &stops) {
                                continue 'cell_loop;
                            }
                            let pushed = select(&level, x_, y_, &pushes)
                                .map(|(i, e)| ((x_, y_, i), e))
                                .collect::<Vec<((usize, usize, usize), Entity)>>();
                            if pushed.len() == 0 {
                                movers.extend(all_pushed);
                                break
                            }
                            all_pushed.extend(pushed);
                            x = x_;
                            y = y_;
                        }
                    }

                    for (i, e) in level[y][x].iter().enumerate() {
                        match e {
                            Entity::Noun(noun) if yous.contains(&noun) =>
                                movers.push(((x, y, i), *e)),
                            _ => (),
                        }
                    }
                }
            }

            // remove all movers from their current position
            let mut removals = vec![vec![vec![]; width]; height];
            for ((x, y, i), _) in &movers {
                removals[*y][*x].push(*i);
            }
            for x in 0..width {
                for y in 0..height {
                    removals[y][x].sort();
                    for (i, r) in removals[y][x].iter().enumerate() {
                        level[y][x].remove(r - i);
                    }
                }
            }

            // add them to their new position
            for ((x, y, _), e) in movers {
                let (x, y) = clip(&level, x as i16 + dx, y as i16 + dy);
                level[y][x].push(e);
            }
        }
    }

    level
}

#[macroquad::main("Baba Is Clone")]
async fn main() {
    let level = parse_level("0-baba-is-you.txt");

    let width = level[0].len();
    let height = level.len();

    let sprites: Texture2D = load_texture("sprites.png").await.unwrap();

    let sprite_map = |e| match e {
        Entity::Noun(noun) => match noun {
            Baba => (0, 1, Some((0, 1, 0, 0))),
            Keke => (0, 2, None),
            Flag => (0, 3, None),
            Rock => (0, 4, None),
            Wall => (0, 5, Some((9, 10, 18, 18))),

            Key  => (3, 1, None),
            Door => (3, 2, None),
            Tile => (3, 7, Some((2, 2, 0, 0))),
        }
        Entity::Text(text) => match text {
            Text::Is => (1, 0, Some((1, 0, 1, 1))),
            Text::And => (2, 0, None),
            Text::Object(noun) => match noun {
                Baba => (1, 1, None),
                Keke => (1, 2, None),
                Flag => (1, 3, None),
                Rock => (1, 4, None),
                Wall => (1, 5, None),

                Key  => (4, 1, None),
                Door => (4, 2, None),
                Tile => (4, 7, None),
            },
            Text::Adjective(adj) => match adj {
                You => (2, 1, None),
                Win => (2, 3, None),
                Push => (2, 4, Some((0, 1, 0, 0))),
                Stop => (2, 5, Some((0, 1, 0, 0))),
            },
        }
    };

    let draw_sprite = |x, y, w, h, noun| {
        let sprite = sprite_map(noun);
        let off = sprite.2.unwrap_or_default();
        draw_texture_ex(
            sprites, x, y, WHITE, DrawTextureParams {
                dest_size: Some(Vec2{x: w, y: h}),
                source: Some(Rect{
                    x: (sprite.0 * SPRITE_SIZE + off.0) as f32,
                    y: (sprite.1 * SPRITE_SIZE + off.1) as f32,
                    w: (SPRITE_SIZE - off.2) as f32,
                    h: (SPRITE_SIZE - off.3) as f32,
                }),
                rotation: 0.0,
                flip_x: false,
                flip_y: false,
                pivot: None,
            },
        );
    };

    let mut last_input: (f64, Option<(KeyCode, Input)>) = (0., None);

    let mut history = vec![level.clone()];
    let mut current_state = &history[0];

    loop {
        // update
        let now = get_time();
        let current_input = match last_input {
            (_, None) => {
                [
                    (KeyCode::Right, Right),
                    (KeyCode::Left, Left),
                    (KeyCode::Up, Up),
                    (KeyCode::Down, Down),
                    (KeyCode::A, Wait),
                    (KeyCode::Z, Undo),
                ].iter()
                    .filter(|(k, _)| is_key_down(*k))
                    .map(|(k, i)| {last_input = (now, Some((*k, *i))); *i})
                    .next()
            },
            (t, Some(x)) => {
                if now - t > 0.15 || x.1 == Undo && now - t > 0.075 {
                    last_input = (now, Some(x));
                    Some(x.1)
                } else {
                    if !is_key_down(x.0) {
                        last_input = (now, None);
                    }
                    None
                }
            },
        };
        match current_input {
            None => (),
            Some(Undo) => if history.len() > 1 {
                history.pop();
            },
            Some(i) => {
                history.push(step(&current_state, i));
            },
        };
        current_state = &history[history.len() - 1];

        // render
        {
            clear_background(LIGHTGRAY);

            let sq_size = ((screen_width() - 20.) / width as f32).min((screen_height() - 20.) / height as f32);
            let game_width = sq_size * width as f32;
            let game_height = sq_size * height as f32;
            let offset_x = (screen_width() - game_width) / 2.;
            let offset_y = (screen_height() - game_height) / 2.;

            draw_rectangle(offset_x, offset_y, game_width, game_height, BLACK);

            for row in 0..height {
                for col in 0..width {
                    for e in &current_state[row][col] {
                        draw_sprite(
                            offset_x + sq_size * col as f32,
                            offset_y + sq_size * row as f32,
                            sq_size,
                            sq_size,
                            *e,
                        );
                    }
                }
            }

        }

        next_frame().await
    }
}
