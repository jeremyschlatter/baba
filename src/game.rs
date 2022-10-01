use macroquad::prelude::*;
use miniquad::graphics::*;
use serde::{Serialize, Deserialize};
use strum::{EnumIter, IntoEnumIterator};

use std::{iter, collections::{HashMap, HashSet}};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, Deserialize, EnumIter)]
enum Noun {
    Baba,
    Keke,
    Wall,
    Door,
    Key,
    Flag,
    Rock,
    Tile,
    Grass,
    Water,
    Skull,
    Lava,
    Brick,
    Flower,
    // Ice,
    // Jelly,
}
use Noun::*;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, Deserialize, EnumIter)]
enum Adjective {
    You,
    Stop,
    Push,
    Win,
    Sink,
    Defeat,
    Hot,
    Melt,
}
use Adjective::*;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
enum Text {
    Is,
    And,
    Object(Noun),
    Adjective(Adjective),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
enum Entity {
    Noun(Noun),
    Text(Text),
}

fn all_entities() -> impl Iterator<Item=Entity> {
    Noun::iter().map(Entity::Noun)
        .chain(iter::once(Entity::Text(Text::Is)))
        .chain(iter::once(Entity::Text(Text::And)))
        .chain(Noun::iter().map(|n| Entity::Text(Text::Object(n))))
        .chain(Adjective::iter().map(|a| Entity::Text(Text::Adjective(a))))
}

type Cell = Vec<Entity>;
type Level = Vec<Vec<Cell>>;

fn parse_level(name: &str) -> Level {
    let s = std::fs::read_to_string("./levels/".to_owned() + name).unwrap();
    let lines: Vec<&str> = s.lines().skip_while(|l| l.starts_with("#")).collect();
    let max = lines.iter().map(|l| l.chars().count()).max().unwrap_or_default();
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
                    "g" => Some(Grass),
                    "a" => Some(Water),
                    "s" => Some(Skull),
                    "l" => Some(Lava),
                    "p" => Some(Flower),
                    "c" => Some(Brick),
                    // "i" => Some(Ice),
                    // "j" => Some(Jelly),
                    _ => None,
                }, match c {
                    '✥' => Some(You),
                    '⊘' => Some(Stop),
                    '↦' => Some(Push),
                    '✓' => Some(Win),
                    '≉' => Some(Sink),
                    '⩍' => Some(Defeat),
                    '⌇' => Some(Hot),
                    '⌢' => Some(Melt),
                    _ => None
                }) {
                    (Some(noun), _) => if c.is_uppercase() {
                        vec![Entity::Text(Text::Object(noun))]
                    } else {
                        vec![Entity::Noun(noun)]
                    }
                    (_, Some(adj)) => vec![Entity::Text(Text::Adjective(adj))],
                    _ => match c {
                        '=' => vec![Entity::Text(Text::Is)],
                        '&' => vec![Entity::Text(Text::And)],
                        _ => vec![],
                    }
                })
                .chain(iter::repeat(vec![]))
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
                    Some(HaveSubject(*obj))
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
                iter::from_fn(|| {
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

fn step(l: &Level, input: Input) -> (Level, bool) {
    let mut level = l.clone();
    let mut rules = scan_rules(&level); // TODO: sus that it's mut

    let width = level[0].len();
    let height = level.len();

    fn adjs(rules: &Vec<Rule>, adj: Adjective) -> HashSet<&Noun> {
        rules.iter()
             .filter_map(|r| match r {
                 (subj, IsAdjective(a)) if *a == adj => Some(subj),
                 _ => None,
              })
             .collect::<HashSet<&Noun>>()
    }

    fn clip(level: &Level, x: i16, y: i16) -> (usize, usize) {
        let width = level[0].len() as i16;
        let height = level.len() as i16;

        (0.max(x).min(width as i16 - 1) as usize,
         0.max(y).min(height as i16 - 1) as usize)
    }

    fn contains(level: &Level, x: usize, y: usize, set: &HashSet<&Noun>) -> bool {
        level[y][x].iter().any(|e| match e {
            Entity::Noun(n) if set.contains(n) => true,
            _ => false,
        })
    }

    fn select_pushed<'a>(level: &'a Level, x: usize, y: usize, set: &'a HashSet<&Noun>) -> impl Iterator<Item=(usize, Entity)> + 'a {
        level[y][x].iter().enumerate().filter(|(_, e)| match e {
            Entity::Noun(n) => set.contains(n),
            Entity::Text(_) => true,
        }).map(|(i, e)| (i, *e))
    }

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
            let yous   = adjs(&rules, You);
            let stops  = adjs(&rules, Stop);
            let pushes = adjs(&rules, Push);
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
                            let pushed = select_pushed(&level, x_, y_, &pushes)
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

    // change things into other things
    rules = scan_rules(&level); // TODO: sus to rescan here
    {
        let changers =
            rules.iter()
                 .filter_map(|r| match r {
                     (from, IsNoun(to)) => Some((*from, *to)),
                     _ => None,
                 })
                 .collect::<HashMap<Noun, Noun>>();
        for x in 0..width {
            for y in 0..height {
                for i in 0..level[y][x].len() {
                    match level[y][x][i] {
                        Entity::Noun(old) => match changers.get(&old) {
                            Some(new) => level[y][x][i] = Entity::Noun(*new),
                            None => (),
                        },
                        Entity::Text(_) => (),
                    }
                }
            }
        }
    }

    // sink things
    {
        let sinks = adjs(&rules, Sink);
        for x in 0..width {
            for y in 0..height {
                if level[y][x].len() > 1 && contains(&level, x, y, &sinks) {
                    level[y][x] = vec![];
                }
            }
        }
    }

    // defeat things
    {
        let defeats = adjs(&rules, Defeat);
        let yous = adjs(&rules, You);
        for x in 0..width {
            for y in 0..height {
                if contains(&level, x, y, &defeats) {
                    level[y][x] = level[y][x].iter().filter_map(|e| match e {
                        Entity::Noun(n) if yous.contains(n) => None,
                        _ => Some(*e),
                    }).collect();
                }
            }
        }
    }

    // melt things
    {
        let hots = adjs(&rules, Hot);
        let melts = adjs(&rules, Melt);
        for x in 0..width {
            for y in 0..height {
                if contains(&level, x, y, &hots) {
                    level[y][x] = level[y][x].iter().filter_map(|e| match e {
                        Entity::Noun(n) if melts.contains(n) => None,
                        _ => Some(*e),
                    }).collect();
                }
            }
        }
    }

    // check for win
    let wins = adjs(&rules, Win);
    let yous = adjs(&rules, You);
    for x in 0..width {
        for y in 0..height {
            if contains(&level, x, y, &wins) && contains(&level, x, y, &yous) {
                return (level, true);
            }
        }
    }


    (level, false)
}

pub enum Mode {
    Normal,
    Golden,
}

pub async fn main(_mode: Mode) {
    // let level = parse_level("0-baba-is-you.txt");
    // let level = parse_level("1-where-do-i-go.txt");
    // let level = parse_level("2-now-what-is-this.txt");
    // let level = parse_level("3-out-of-reach.txt");
    // let level = parse_level("4-still-out-of-reach.txt");
    // let level = parse_level("5-volcano.txt");
    // let level = parse_level("6-off-limits.txt");
    let level = parse_level("7-grass-yard.txt");

    // println!("{}", ron::to_string(&level).unwrap());

    let width = level[0].len();
    let height = level.len();

    let sprites: HashMap<Entity, Texture2D> = {
        async fn load(e: Entity) -> (Entity, Texture2D) {
            let name = match e {
                Entity::Noun(Lava) => "water".to_string(),
                Entity::Noun(n) => format!("{:?}", n),
                Entity::Text(t) => match t {
                    Text::Adjective(a) => format!("text_{:?}", a),
                    Text::Object(o) => format!("text_{:?}", o),
                    Text::Is => "text_is".to_string(),
                    Text::And => "text_and".to_string(),
                },
            }.to_lowercase();
            (e, load_texture(&format!("resources/Data/Sprites/{name}_0_1.png")).await.unwrap())
        }
        futures::future::join_all(all_entities().map(load)).await.into_iter().collect()
    };

    let congrats: Texture2D = load_texture("congratulations.png").await.unwrap();

    let draw_sprite = |x, y, w, h, entity| {
        let sprite = sprites[&entity];
        draw_texture_ex(
            sprite, x, y, WHITE, DrawTextureParams {
                dest_size: Some(Vec2{x: w, y: h}),
                ..Default::default()
            },
        );
    };

    let mut last_input: (f64, Option<(KeyCode, Input)>) = (0., None);

    let mut history = vec![level.clone()];
    let mut current_state = &history[0];

    let mask = load_material(
        VERTEX_SHADER,
        FRAGMENT_SHADER,
        MaterialParams {
            uniforms: vec![("radius".to_string(), UniformType::Float1)],
            pipeline_params: PipelineParams {
                color_blend: Some(BlendState::new(
                    Equation::Add,
                    BlendFactor::Value(BlendValue::SourceAlpha),
                    BlendFactor::OneMinusValue(BlendValue::SourceAlpha))
                ),
                ..Default::default()
            },
            ..Default::default()
        },
    ).unwrap();

    let anim_time = 2.0;
    let mut win_time = None;
    let border_color = Color::from_rgba(0x15, 0x18, 0x1F, 0xFF);

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
                let (next, win) = step(&current_state, i);
                if win && win_time.is_none() {
                    win_time = Some(get_time());
                }
                history.push(next);
            },
        };
        current_state = &history[history.len() - 1];

        // render
        {
            clear_background(border_color);

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

            // draw congratulations when you win
            match win_time {
                None => (),
                Some(anim_start) => {
                    gl_use_material(mask);
                    mask.set_uniform(
                        "radius",
                        (((get_time() - anim_start) / anim_time) as f32).min(0.5_f32.sqrt()),
                    );
                    let scale = (game_width * 0.65) / congrats.width();
                    draw_texture_ex(
                        congrats,
                        offset_x + (game_width - congrats.width() * scale) / 2.,
                        offset_y + (game_height - congrats.height() * scale) / 2.,
                        WHITE,
                        DrawTextureParams {
                            dest_size: Some(Vec2{
                                x: congrats.width() * scale,
                                y: congrats.height() * scale,
                            }),
                            ..Default::default()
                        },
                    );
                    gl_use_default_material();
                },
            }
        }

        next_frame().await
    }
}

const FRAGMENT_SHADER: &'static str = "#version 100
precision lowp float;

varying vec2 uv;

uniform sampler2D Texture;
uniform float radius;

void main() {
    float mask = distance(uv, vec2(0.5, 0.5)) > radius ? -1.0 : 0.0;
    gl_FragColor = texture2D(Texture, uv) + vec4(0, 0, 0, mask);
}
";

const VERTEX_SHADER: &'static str = "#version 100
precision lowp float;

attribute vec3 position;
attribute vec2 texcoord;

varying vec2 uv;

uniform mat4 Model;
uniform mat4 Projection;

void main() {
    gl_Position = Projection * Model * vec4(position, 1);
    uv = texcoord;
}
";
