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

fn parse_level(name: &str) -> Vec<Vec<Option<Entity>>> {
    let s = std::fs::read_to_string("./levels/".to_owned() + name).unwrap();
    let lines: Vec<&str> = s.lines().skip_while(|l| l.starts_with("# ")).collect();
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
                        Some(Entity::Noun(noun))
                    } else {
                        Some(Entity::Text(Text::Object(noun)))
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

#[macroquad::main("Snake")]
async fn main() {
    let level = parse_level("0-baba-is-you.txt");

    let mut snake = Snake {
        head: (0, 0),
        dir: (1, 0),
        body: LinkedList::new(),
    };
    let mut fruit: Point = (rand::gen_range(0, SQUARES), rand::gen_range(0, SQUARES));
    let mut score = 0;
    let mut speed = 0.3;
    let mut last_update = get_time();
    let mut game_over = false;

    let up = (0, -1);
    let down = (0, 1);
    let right = (1, 0);
    let left = (-1, 0);

    let sprites: Texture2D = load_texture("sprites.png").await.unwrap();

    let sprite_map = |e| match e {
        Entity::Noun(noun) => match noun {
            Baba => (0, 1),
            Keke => (0, 2),
            Flag => (0, 3),
            Rock => (0, 4),
            Wall => (0, 5),

            Key  => (3, 1),
            Door => (3, 2),
            Tile => (3, 7),
        }
        Entity::Text(text) => match text {
            Text::Is => (1, 0),
            Text::And => (2, 0),
            Text::Object(noun) => match noun {
                Baba => (1, 1),
                Keke => (1, 2),
                Flag => (1, 3),
                Rock => (1, 4),
                Wall => (1, 5),

                Key  => (4, 1),
                Door => (4, 2),
                Tile => (4, 7),
            },
            Text::Adjective(adj) => match adj {
                You => (2, 1),
                Win => (2, 3),
                Push => (2, 4),
                Stop => (2, 5),
            },
        }
    };

    let draw_sprite = |x, y, w, h, noun, text| {
        let sprite = sprite_map(noun);
        draw_texture_ex(
            sprites, x, y, WHITE, DrawTextureParams {
                dest_size: Some(Vec2{x: w, y: h}),
                source: Some(Rect{
                    x: (sprite.0 * SPRITE_SIZE) as f32,
                    y: (sprite.1 * SPRITE_SIZE) as f32,
                    w: SPRITE_SIZE as f32,
                    h: SPRITE_SIZE as f32,
                }),
                rotation: 0.0,
                flip_x: false,
                flip_y: false,
                pivot: None,
            },
        );
    };

    return;

    loop {
        if !game_over {
            if is_key_down(KeyCode::Right) && snake.dir != left {
                snake.dir = right;
            } else if is_key_down(KeyCode::Left) && snake.dir != right {
                snake.dir = left;
            } else if is_key_down(KeyCode::Up) && snake.dir != down {
                snake.dir = up;
            } else if is_key_down(KeyCode::Down) && snake.dir != up {
                snake.dir = down;
            }

            if get_time() - last_update > speed {
                last_update = get_time();
                snake.body.push_front(snake.head);
                snake.head = (snake.head.0 + snake.dir.0, snake.head.1 + snake.dir.1);
                if snake.head == fruit {
                    fruit = (rand::gen_range(0, SQUARES), rand::gen_range(0, SQUARES));
                    score += 100;
                    speed *= 0.9;
                } else {
                    snake.body.pop_back();
                }
                if snake.head.0 < 0
                    || snake.head.1 < 0
                    || snake.head.0 >= SQUARES
                    || snake.head.1 >= SQUARES
                {
                    game_over = true;
                }
                for (x, y) in &snake.body {
                    if *x == snake.head.0 && *y == snake.head.1 {
                        game_over = true;
                    }
                }
            }
        }
        if !game_over {
            clear_background(LIGHTGRAY);

            let game_size = screen_width().min(screen_height());
            let offset_x = (screen_width() - game_size) / 2. + 10.;
            let offset_y = (screen_height() - game_size) / 2. + 10.;
            let sq_size = (screen_height() - offset_y * 2.) / SQUARES as f32;

            draw_rectangle(offset_x, offset_y, game_size - 20., game_size - 20., WHITE);

            for i in 1..SQUARES {
                draw_line(
                    offset_x,
                    offset_y + sq_size * i as f32,
                    screen_width() - offset_x,
                    offset_y + sq_size * i as f32,
                    2.,
                    LIGHTGRAY,
                );
            }

            for i in 1..SQUARES {
                draw_line(
                    offset_x + sq_size * i as f32,
                    offset_y,
                    offset_x + sq_size * i as f32,
                    screen_height() - offset_y,
                    2.,
                    LIGHTGRAY,
                );
            }

            draw_rectangle(
                offset_x + snake.head.0 as f32 * sq_size,
                offset_y + snake.head.1 as f32 * sq_size,
                sq_size,
                sq_size,
                DARKGREEN,
            );

            for (x, y) in &snake.body {
                draw_rectangle(
                    offset_x + *x as f32 * sq_size,
                    offset_y + *y as f32 * sq_size,
                    sq_size,
                    sq_size,
                    LIME,
                );
            }

            draw_rectangle(
                offset_x + fruit.0 as f32 * sq_size,
                offset_y + fruit.1 as f32 * sq_size,
                sq_size,
                sq_size,
                GOLD,
            );

            draw_text(
                format!("SCORE: {}", score).as_str(),
                10.,
                10.,
                20.,
                DARKGRAY,
            );

            draw_sprite(offset_x, offset_y, sq_size, sq_size, Entity::Noun(Baba), true);

        } else {
            clear_background(WHITE);
            let text = "Game Over. Press [enter] to play again.";
            let font_size = 30.;
            let text_size = measure_text(text, None, font_size as _, 1.0);

            draw_text(
                text,
                screen_width() / 2. - text_size.width / 2.,
                screen_height() / 2. - text_size.height / 2.,
                font_size,
                DARKGRAY,
            );

            if is_key_down(KeyCode::Enter) {
                snake = Snake {
                    head: (0, 0),
                    dir: (1, 0),
                    body: LinkedList::new(),
                };
                fruit = (rand::gen_range(0, SQUARES), rand::gen_range(0, SQUARES));
                score = 0;
                speed = 0.3;
                last_update = get_time();
                game_over = false;
            }
        }
        next_frame().await
    }
}
