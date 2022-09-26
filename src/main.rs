use macroquad::prelude::*;

use std::collections::LinkedList;

const SQUARES: i16 = 10;

const SPRITE_SIZE : i16 = 37;

type Point = (i16, i16);

struct Snake {
    head: Point,
    body: LinkedList<Point>,
    dir: Point,
}


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

type Level = Vec<Vec<Option<Entity>>>;

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
                        Some(Entity::Text(Text::Object(noun)))
                    } else {
                        Some(Entity::Noun(noun))
                    }
                    (_, Some(adj)) => Some(Entity::Text(Text::Adjective(adj))),
                    _ => match c {
                        'i' => Some(Entity::Text(Text::Is)),
                        _ => None,
                    }
                })
                .chain(std::iter::repeat(None))
                .take(max)
                .collect::<Vec<Option<Entity>>>())
        .collect()
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Predicate {
    IsNoun(Noun),
    IsAdjective(Adjective),
}
use Predicate::*;

type Rule = (Noun, Predicate);

fn scan_rules(l: Level) -> Vec<Rule> {
    [].into()
}

#[macroquad::main("Snake")]
async fn main() {
    let level = parse_level("0-baba-is-you.txt");

    let width = level[0].len();
    let height = level.len();
    println!("{}x{}", width, height);

    let sprites: Texture2D = load_texture("sprites.png").await.unwrap();

    let sprite_map = |e| match e {
        Entity::Noun(noun) => match noun {
            Baba => (0, 1, Some((0, 1, 0, 0))),
            Keke => (0, 2, None),
            Flag => (0, 3, None),
            Rock => (0, 4, None),
            Wall => (0, 5, None),

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

    loop {

        // update
        {

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

        }

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

            draw_rectangle(offset_x, offset_y, game_width, game_height, WHITE);

            for i in 1..height {
                draw_line(
                    offset_x,
                    offset_y + sq_size * i as f32,
                    screen_width() - offset_x,
                    offset_y + sq_size * i as f32,
                    2.,
                    LIGHTGRAY,
                );
            }

            for i in 1..width {
                draw_line(
                    offset_x + sq_size * i as f32,
                    offset_y,
                    offset_x + sq_size * i as f32,
                    screen_height() - offset_y,
                    2.,
                    LIGHTGRAY,
                );
            }

            for row in 0..height {
                for col in 0..width {
                    match level[row][col] {
                        Some(e) => draw_sprite(
                            offset_x + sq_size * col as f32,
                            offset_y + sq_size * row as f32,
                            sq_size,
                            sq_size,
                            e,
                        ),
                        None => draw_rectangle(
                            offset_x + sq_size * col as f32,
                            offset_y + sq_size * row as f32,
                            sq_size,
                            sq_size,
                            BLACK,
                        ),
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
