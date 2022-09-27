use macroquad::prelude::*;

const SPRITE_SIZE : i16 = 37;


// next steps:
//
// represent the starting level



// objects:
//
// baba
// keke
// wall
// door
// key
// flag
//
// statuses:
//
// you
// win
// defeat
// stop
// shut
// open
// push
//
// bare words:
// is
// and

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

fn scan_rules_line<'a, I>(line: I) -> Vec<Rule>
where
    I: Iterator<Item = &'a Cell>,
{
    let mut rules = vec![];
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
    rules
}

fn scan_rules(l: &Level) -> Vec<Rule> {
    l.iter()
     .map(|row| Box::new(row.iter()) as Box<dyn Iterator<Item = &Cell>>)
     .chain(Cols::new(&l).map(|x| Box::new(x) as Box<dyn Iterator<Item = &Cell>>))
     .map(|line| scan_rules_line(line))
     .flatten()
     .collect()
}

struct Cols<'a> {
    col: usize,
    level: &'a Level,
}

impl <'a> Cols<'a> {
    fn new(level: &'a Level) -> Cols<'a> {
        Cols {
            col: 0,
            level: level,
        }
    }
}

impl <'a> Iterator for Cols<'a> {
    type Item = ColIter<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.col += 1;
        if self.level.len() > 0 && self.col <= self.level[0].len() {
            Some(ColIter::new(self.level, self.col - 1))
        } else {
            None
        }
    }
}

struct ColIter<'a> {
    row: usize,
    col: usize,
    level: &'a Level,
}

impl <'a> ColIter<'a> {
    fn new(level: &'a Level, col: usize) -> ColIter<'a> {
        ColIter {
            row: 0,
            col: col,
            level: level,
        }
    }
}

impl <'a> Iterator for ColIter<'a> {
    type Item = &'a Cell;
    fn next(&mut self) -> Option<Self::Item> {
        self.row += 1;
        if self.row <= self.level.len() {
            Some(&self.level[self.row - 1][self.col])
        } else {
            None
        }
    }
}

#[macroquad::main("Baba Is Clone")]
async fn main() {
    let mut level = parse_level("0-baba-is-you.txt");
    let mut rules = scan_rules(&level);

    println!("the rules:");
    for rule in &rules {
        println!("\t{:?}", rule);
    }

    let width = level[0].len();
    let height = level.len();

    let clip = |x, y| (
        0.max(x).min(width as i16 - 1) as usize,
        0.max(y).min(height as i16 - 1) as usize);

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

    let mut last_input: Option<KeyCode> = None;

    // #[derive(PartialEq, Eq)]
    enum Input {
        Right,
        Left,
        Up,
        Down,
        Wait,
    }
    use Input::*;

    loop {

        // update
        let current_input = match last_input {
            None => {
                [
                    (KeyCode::Right, Right),
                    (KeyCode::Left, Left),
                    (KeyCode::Up, Up),
                    (KeyCode::Down, Down),
                    (KeyCode::A, Wait),
                ].iter()
                    .filter(|(k, i)| is_key_down(*k))
                    .map(|(k, i)| {last_input = Some(*k); i})
                    .next()
            },
            Some(i) => {
                if !is_key_down(i) {
                    last_input = None;
                }
                None
            },
        };
        match current_input {
            None => (),
            Some(i) => {
                // Move you.
                match (match i {
                    Left  => Some((-1, 0)),
                    Up    => Some((0, -1)),
                    Right => Some((1, 0)),
                    Down  => Some((0, 1)),
                    Wait  => None,
                }) {
                    None => (),
                    Some((dx, dy)) => {
                        // Find all nouns that are you.
                        let yous: std::collections::HashSet<&Noun> = rules.iter()
                             .filter_map(|r| match r {
                                 (subj, IsAdjective(You)) => Some(subj),
                                 _ => None,
                              })
                             .collect();

                        // Iterate through all object entities, move the ones that are you.
                        let mut movers: Vec<((usize, usize), Entity)> = vec![];
                        for y in 0..level.len() {
                            for x in 0..level[y].len() {
                                let (go, stay) = level[y][x].iter().partition(|e| match e {
                                    Entity::Noun(noun) if yous.contains(noun) => true,
                                    _ => false,
                                });
                                level[y][x] = stay;
                                movers.extend(
                                    go.iter()
                                      .map(|e| ((x, y), *e))
                                      .collect::<Vec<((usize, usize), Entity)>>());
                            }
                        }

                        for ((x, y), e) in movers {
                            let (x, y) = clip(x as i16 + dx, y as i16 + dy);
                            level[y][x].push(e);
                        }
                    }
                }
            },
        };

//             if is_key_down(KeyCode::Right) && snake.dir != left {
//                 snake.dir = right;
//             } else if is_key_down(KeyCode::Left) && snake.dir != right {
//                 snake.dir = left;
//             } else if is_key_down(KeyCode::Up) && snake.dir != down {
//                 snake.dir = up;
//             } else if is_key_down(KeyCode::Down) && snake.dir != up {
//                 snake.dir = down;
//             }
//             if get_time() - last_update > speed {
//                 last_update = get_time();
//                 snake.body.push_front(snake.head);
//                 snake.head = (snake.head.0 + snake.dir.0, snake.head.1 + snake.dir.1);
//                 if snake.head == fruit {
//                     fruit = (rand::gen_range(0, SQUARES), rand::gen_range(0, SQUARES));
//                     score += 100;
//                     speed *= 0.9;
//                 } else {
//                     snake.body.pop_back();
//                 }
//                 if snake.head.0 < 0
//                     || snake.head.1 < 0
//                     || snake.head.0 >= SQUARES
//                     || snake.head.1 >= SQUARES
//                 {
//                     game_over = true;
//                 }
//                 for (x, y) in &snake.body {
//                     if *x == snake.head.0 && *y == snake.head.1 {
//                         game_over = true;
//                     }
//                 }
//             }

        // render
        {
            clear_background(LIGHTGRAY);

            let sq_size = ((screen_width() - 20.) / width as f32).min((screen_height() - 20.) / height as f32);
            let game_width = sq_size * width as f32;
            let game_height = sq_size * height as f32;
            let offset_x = (screen_width() - game_width) / 2.;
            let offset_y = (screen_height() - game_height) / 2.;

    //             println!("
    //                 game_width={game_width}
    //                 game_height={game_height}
    //                 offset_x={offset_x}
    //                 offset_y={offset_y}
    //                 sq_size={sq_size}
    //                 width={width}
    //                 height={height}
    //             ");

            draw_rectangle(offset_x, offset_y, game_width, game_height, BLACK);

            for row in 0..height {
                for col in 0..width {
                    for e in &level[row][col] {
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

    //             draw_text(
    //                 format!("SCORE: {}", score).as_str(),
    //                 10.,
    //                 10.,
    //                 20.,
    //                 DARKGRAY,
    //             );

        }

        next_frame().await
    }
}
