use macroquad::prelude::*;
use miniquad::graphics::*;
use serde::{Serialize, Deserialize};
use strum::{EnumIter, EnumProperty, EnumString, IntoEnumIterator};

use std::{iter, collections::{HashMap, HashSet}, str::FromStr};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, Deserialize, EnumIter, EnumString, EnumProperty)]
#[strum(serialize_all = "snake_case")]
pub enum Noun {
    #[strum(props(color = "0 3", text_color = "4 0", text_color_active = "4 1"))]
    Baba,
    #[strum(props(color = "2 2", text_color = "2 1", text_color_active = "2 2"))]
    Keke,
    #[strum(props(color = "1 1", text_color = "1 1", text_color_active = "0 1"))]
    Wall,
    #[strum(props(color = "2 2", text_color = "2 1", text_color_active = "2 2"))]
    Door,
    #[strum(props(color = "2 4", text_color = "6 1", text_color_active = "2 4"))]
    Key,
    #[strum(props(color = "2 4", text_color = "6 1", text_color_active = "2 4"))]
    Flag,
    #[strum(props(color = "6 2", text_color = "6 0", text_color_active = "6 1"))]
    Rock,
    #[strum(props(color = "0 0", text_color = "1 1", text_color_active = "0 1"))]
    Tile,
    #[strum(props(color = "5 0", text_color = "5 1", text_color_active = "5 3"))]
    Grass,
    #[strum(props(color = "1 3", text_color = "1 2", text_color_active = "1 3"))]
    Water,
    #[strum(props(color = "2 1", text_color = "2 0", text_color_active = "2 1"))]
    Skull,
    #[strum(props(color = "2 3", text_color = "2 2", text_color_active = "2 3"))]
    Lava,
    #[strum(props(color = "6 3", text_color = "6 0", text_color_active = "6 1"))]
    Brick,
    #[strum(props(color = "3 3", text_color = "3 2", text_color_active = "3 3"))]
    Flower,
    #[strum(props(color = "1 2", text_color = "1 2", text_color_active = "1 3"))]
    Ice,
    #[strum(props(color = "1 4", text_color = "1 3", text_color_active = "1 4"))]
    Jelly,
    #[strum(props(color = "2 2", text_color = "2 1", text_color_active = "2 2"))]
    Crab,
    #[strum(props(color = "2 3", text_color = "2 2", text_color_active = "2 3"))]
    Seastar,
    #[strum(props(color = "5 2", text_color = "5 0", text_color_active = "5 1"))]
    Algae,
    #[strum(props(color = "4 2", text_color = "4 1", text_color_active = "4 2"))]
    Love,
    #[strum(props(color = "0 1", text_color = "1 1", text_color_active = "0 1"))]
    Pillar,
}
use Noun::*;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, Deserialize, EnumIter, EnumString, EnumProperty)]
#[strum(serialize_all = "snake_case")]
pub enum Adjective {
    #[strum(props(text_color = "4 0", text_color_active = "4 1"))]
    You,
    #[strum(props(text_color = "5 0", text_color_active = "5 1"))]
    Stop,
    #[strum(props(text_color = "6 0", text_color_active = "6 1"))]
    Push,
    #[strum(props(text_color = "6 1", text_color_active = "2 4"))]
    Win,
    #[strum(props(text_color = "1 2", text_color_active = "1 3"))]
    Sink,
    #[strum(props(text_color = "2 0", text_color_active = "2 1"))]
    Defeat,
    #[strum(props(text_color = "2 2", text_color_active = "2 3"))]
    Hot,
    #[strum(props(text_color = "1 2", text_color_active = "1 3"))]
    Melt,
    #[strum(props(text_color = "5 1", text_color_active = "5 3"))]
    Move,
}
use Adjective::*;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, Deserialize, EnumProperty)]
pub enum Text {
    #[strum(props(text_color = "0 1", text_color_active = "0 3"))]
    Is,
    #[strum(props(text_color = "0 1", text_color_active = "0 3"))]
    And,
    Object(Noun),
    Adjective(Adjective),
}
impl FromStr for Text {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "is" => Ok(Text::Is),
            "and" => Ok(Text::And),
            _ => Err(format!("can't parse this as Text: '{s}'")),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, Deserialize, EnumString, EnumIter)]
#[strum(serialize_all = "snake_case")]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}
use Direction::*;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
pub enum Entity {
    Noun(Direction, Noun),
    Text(Direction, Text),
}

fn default_color(e: Entity, palette: &Image) -> Color {
    let ixs = match e {
        Entity::Noun(_, n) => n.get_str("color"),
        Entity::Text(_, t) => match t {
            Text::Object(n) => n.get_str("text_color_active"),
            Text::Adjective(a) => a.get_str("text_color_active"),
            Text::Is => t.get_str("text_color_active"),
            Text::And => t.get_str("text_color_active"),
        },
    }.unwrap();

    palette.get_pixel(
        ixs[0..1].parse().unwrap(),
        ixs[2..3].parse().unwrap(),
    )
}

fn all_entities() -> impl Iterator<Item=Entity> {
    Direction::iter().flat_map(|d|
        Noun::iter().map(move |n| Entity::Noun(d, n))
            .chain(iter::once(Entity::Text(d, Text::Is)))
            .chain(iter::once(Entity::Text(d, Text::And)))
            .chain(Noun::iter().map(move |n| Entity::Text(d, Text::Object(n))))
            .chain(Adjective::iter().map(move |a| Entity::Text(d, Text::Adjective(a)))))
}

type Cell = Vec<Entity>;
type Level = Vec<Vec<Cell>>;

fn parse_level(name: &str) -> (Level, String) {
    let s = std::fs::read_to_string(name).unwrap();
    let metas: HashMap<&str, &str> =
        s.lines()
             .take_while(|&s| s != "---")
             .map(|s| {
                 let mut x = s.split(" = ");
                 (x.next().unwrap(), x.next().unwrap())
             })
             .collect();
    let right_pad: usize =
        metas.get("right pad").and_then(|s| s.parse().ok()).unwrap_or_default();
    enum LegendValue {
        Abbreviation(Noun),
        FullCell(Cell),
    }
    use LegendValue::*;
    let legend: HashMap<String, LegendValue> =
        metas.iter()
             .filter(|(&k, _)| k.len() == 1)
             .map(|(&k, &c)| (
                k.to_string(), {
                    if let Ok(n) = Noun::from_str(c) {
                        Abbreviation(n)
                    } else {
                        FullCell(
                            c.split(" on ")
                             .collect::<Vec<&str>>()
                             .iter()
                             .rev()
                             .map(|s| {
                                 let mut x = s.split(" ");
                                 let n = x.next().unwrap();
                                 let d = x.next().and_then(|s| Direction::from_str(s).ok()).unwrap_or(Right);
                                 Entity::Noun(d, Noun::from_str(n).unwrap())
                             })
                             .collect()
                        )
                    }
                }
             ))
             .collect();
    let map: Vec<&str> = s.lines().skip_while(|s| *s != "---").skip(1).collect();

    let max = map.iter().map(|l| l.chars().count()).max().unwrap_or_default() + right_pad;
    let level = map.iter()
        .map(
            |l| l.chars()
                .map(|c| match (legend.get(c.to_string().to_lowercase().as_str())
                , match c {
                    '✥' => Some(You),
                    '⊘' => Some(Stop),
                    '↦' => Some(Push),
                    '✓' => Some(Win),
                    '≉' => Some(Sink),
                    '⩍' => Some(Defeat),
                    '⌇' => Some(Hot),
                    '⌢' => Some(Melt),
                    '→' => Some(Move),
                    _ => None
                }) {
                    (Some(FullCell(cell)), _) => cell.clone(),
                    (Some(Abbreviation(noun)), _) => if c.is_uppercase() {
                        vec![Entity::Text(Right, Text::Object(*noun))]
                    } else {
                        vec![Entity::Noun(Right, *noun)]
                    }
                    (_, Some(adj)) => vec![Entity::Text(Right, Text::Adjective(adj))],
                    _ => match c {
                        '=' => vec![Entity::Text(Right, Text::Is)],
                        '&' => vec![Entity::Text(Right, Text::And)],
                        _ => vec![],
                    }
                })
                .chain(iter::repeat(vec![]))
                .take(max)
                .collect::<Vec<Vec<Entity>>>())
        .collect();

    (level, metas.get("palette").unwrap_or(&"default").to_string())
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Predicate {
    IsNoun(Noun),
    IsAdjective(Adjective),
}
use Predicate::*;

type Rule = (Noun, Predicate);

#[cfg(test)]
mod tests {
    use crate::game::*;
    #[test]
    fn scan_rules() {
        let is_a = IsAdjective;
        let is = IsNoun;
        let tests = [
            (vec!["baba", "is", "you"], vec![(Baba, is_a(You))]),
            (
                vec!["baba", "is", "wall", "is", "push"],
                vec![
                    (Baba, is(Wall)),
                    (Wall, is_a(Push)),
                ]
            ),
            (vec!["baba", "and"], vec![]),
            (vec!["wall", "and", "keke", "is", "push"], vec![(Wall, is_a(Push)), (Keke, is_a(Push))]),
            (vec!["wall", "is", "push", "and", "stop"], vec![(Wall, is_a(Push)), (Wall, is_a(Stop))]),
            (vec!["crab", "and", "", "baba", "is", "you"], vec![(Baba, is_a(You))]),
            (
                vec!["baba", "and", "keke", "is", "rock", "and", "wall", "is", "door"],
                vec![
                    (Baba, is(Rock)),
                    (Baba, is(Wall)),
                    (Keke, is(Rock)),
                    (Keke, is(Wall)),
                    // notably absent: (Rock, is(Door))
                    (Wall, is(Door)),
                ]
            ),

            // TODO
            // (
            //     vec!["baba", "is", "not", "wall", "is", "push"],
            //     vec![
            //         (Baba is not wall),
            //         (not wall is push),
            //     ]
            // ),

        ];
        for (input, output) in tests {
            let mut result = vec![];
            let input_ =
                input.iter()
                     .map(|s| match (s, Text::from_str(s), Noun::from_str(s), Adjective::from_str(s)) {
                         (&"", _, _, _)    => vec![],
                         (_, Ok(t), _, _) => vec![t],
                         (_, _, Ok(n), _) => vec![Text::Object(n)],
                         (_, _, _, Ok(a)) => vec![Text::Adjective(a)],
                         _ => panic!("unrecognized word: '{}'", s),
                     })
                     .map(|t| t.iter().map(|t| Entity::Text(*t)).collect::<Vec<Entity>>())
                     .collect::<Vec<Vec<Entity>>>();
            scan_rules_line(&mut result, input_.iter());
            assert_eq!(
                output.into_iter().collect::<HashSet<Rule>>(),
                result.into_iter().collect::<HashSet<Rule>>(),
                "input: {:?}", input,
            );
        }
    }
}

fn scan_rules_text(rules: &mut Vec<Rule>, text: &[Text]) {
    #[derive(Debug)]
    enum ListState {
        Complete,
        Incomplete,
    }
    use ListState::*;
    #[derive(Debug)]
    enum ScanState {
        Subjects(ListState, Vec<Noun>),
        Predicates(ListState, Vec<Noun>, Option<Predicate>),
    }
    use ScanState::*;
    use Text::*;
    let zero = |mut v: Vec<Noun>| { v.clear(); Subjects(Incomplete, v) };
    let mut state = zero(vec![]);
    let mut i = 0;
    while i < text.len() {
        let t = text[i];
        i += 1;
        state = match state {
            Subjects(s, mut subjs) => match s {
                Incomplete => match t {
                    Object(noun) => { subjs.push(noun); Subjects(Complete, subjs) },
                    _ => zero(subjs),
                },
                Complete => match t {
                    Is => Predicates(Incomplete, subjs, None),
                    And => Subjects(Incomplete, subjs),
                    _ => zero(subjs),
                },
            },
            Predicates(s, subjs, preds) => match s {
                Incomplete => match match t {
                    Object(noun) => Some(IsNoun(noun)),
                    Adjective(adj) => Some(IsAdjective(adj)),
                    _ => None,
                } {
                    Some(pred) => {
                        for s in &subjs {
                            rules.push((*s, pred));
                        }
                        Predicates(Complete, subjs, Some(pred))
                    },
                    None => zero(subjs),
                },
                Complete => match t {
                    And => Predicates(Incomplete, subjs, preds),
                    _ => {
                        i -= 2; // to pick up the last subject word, if present
                        zero(subjs)
                    },
                },
            },
        };
    }
}

fn scan_rules_line<'a, I>(rules: &mut Vec<Rule>, line: I)
where
    I: Iterator<Item = &'a Cell>,
{
    fn chunks(i: Vec<Vec<Text>>) -> Vec<Vec<Vec<Text>>> {
        i.iter().fold(vec![vec![]], |mut acc: Vec<Vec<Vec<Text>>>, v: &Vec<Text>| {
            if v.len() == 0 {
                acc.push(vec![]);
            } else {
                let l = acc.len();
                acc[l - 1].push(v.clone());
            }
            acc
        })
    }

    fn branches(input: Vec<Vec<Text>>) -> Vec<Vec<Text>> {
        input.iter()
         .fold(vec![vec![]], |acc: Vec<Vec<usize>>, v: &Vec<Text>| {
             (0..v.len()).flat_map(|j| {
                 let mut x = acc.clone();
                 for xx in &mut x {
                     xx.push(j);
                 }
                 x
             }).collect()
         })
         .iter()
         .map(|ixs| ixs.iter()
                       .enumerate()
                       .map(|(i, &j)| input[i][j])
                       .collect::<Vec<Text>>())
         .collect()
    }

    let x: Vec<Vec<Text>> =
       line.map(|c| c.iter()
                     .filter_map(|&e| match e {
                         Entity::Text(_, t) => Some(t),
                         _ => None,
                     }).collect()
           ).collect();

    for chunk in chunks(x) {
        for branch in branches(chunk) {
            scan_rules_text(rules, branch.as_slice());
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
                    if row as usize >= l.len() {
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
    Go(Direction),
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
            Entity::Noun(_, n) if set.contains(n) => true,
            _ => false,
        })
    }

    fn select_pushed<'a>(level: &'a Level, x: usize, y: usize, set: &'a HashSet<&Noun>) -> impl Iterator<Item=(usize, Entity)> + 'a {
        level[y][x].iter().enumerate().filter(|(_, e)| match e {
            Entity::Noun(_, n) => set.contains(n),
            Entity::Text(_, _) => true,
        }).map(|(i, e)| (i, *e))
    }

    // Move you.
    match match input {
        Go(Left)  => Some((-1, 0)),
        Go(Up)    => Some((0, -1)),
        Go(Right) => Some((1, 0)),
        Go(Down)  => Some((0, 1)),
        Wait => None,
        Undo => None,
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
                            Entity::Noun(_, noun) if yous.contains(&noun) =>
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

    // change your direction
    if let Go(d) = input {
        let yous   = adjs(&rules, You);
        for x in 0..width {
            for y in 0..height {
                for i in 0..level[y][x].len() {
                    match level[y][x][i] {
                        // TODO: incorrect when Text Is You
                        Entity::Noun(_, n) => if yous.contains(&n) {
                            level[y][x][i] = Entity::Noun(d, n);
                        },
                        _ => (),
                    }
                }
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
                        Entity::Noun(d, old) => match changers.get(&old) {
                            Some(new) => level[y][x][i] = Entity::Noun(d, *new),
                            None => (),
                        },
                        Entity::Text(_, _) => (),
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
                        Entity::Noun(_, n) if yous.contains(n) => None,
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
                        Entity::Noun(_, n) if melts.contains(n) => None,
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

pub async fn main(level: Option<&str>) -> Vec<Level> {
    let (level, palette_name) = parse_level(level.unwrap_or(
        // "levels/0-baba-is-you.txt"
        // "levels/1-where-do-i-go.txt"
        // "levels/2-now-what-is-this.txt"
        // "levels/3-out-of-reach.txt"
        // "levels/4-still-out-of-reach.txt"
        // "levels/5-volcano.txt"
        // "levels/6-off-limits.txt"
        // "levels/7-grass-yard.txt"
        // "levels/1-the-lake/1-icy-waters.txt"
        "levels/1-the-lake/2-turns.txt"
    ));

    // println!("{}", ron::to_string(&level).unwrap());

    let width = level[0].len();
    let height = level.len();

    let sprites: HashMap<Entity, Texture2D> = {
        async fn load(e: Entity) -> (Entity, Texture2D) {
            let name = match e {
                Entity::Noun(_, Lava) => "water".to_string(),
                Entity::Noun(_, n) => format!("{:?}", n),
                Entity::Text(_, t) => "text_".to_string() + &(match t {
                    Text::Adjective(a) => format!("{:?}", a),
                    Text::Object(Seastar) => "star".to_string(),
                    Text::Object(o) => format!("{:?}", o),
                    Text::Is => "is".to_string(),
                    Text::And => "and".to_string(),
                }),
            }.to_lowercase();
            let filename = &format!(
                "resources/Data/Sprites/{name}_{}_1.png",
                match match e { Entity::Noun(d, _) => d, Entity::Text(d, _) => d } {
                    Right => "0",
                    Up => "8",
                    Left => "16",
                    Down => "24",
                }
            );
            let fallback = &format!("resources/Data/Sprites/{name}_0_1.png");
            (e, load_texture(
                    if std::path::Path::new(filename).exists() {
                        filename
                    } else {
                        fallback
                    }
            ).await.unwrap())
        }
        futures::future::join_all(all_entities().map(load)).await.into_iter().collect()
    };

    let congrats: Texture2D = load_texture("congratulations.png").await.unwrap();

    #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
    enum UIInput {
        Control(Input),
        Pause,
    }
    use UIInput::*;

    let mut last_input: (f64, Option<(KeyCode, UIInput)>) = (0., None);

    let mut history = vec![level.clone()];
    let mut current_state = &history[0];

    let blend_alpha = Some(
        BlendState::new(
            Equation::Add,
            BlendFactor::Value(BlendValue::SourceAlpha),
            BlendFactor::OneMinusValue(BlendValue::SourceAlpha),
        )
    );

    let mask = load_material(
        DEFAULT_VERTEX_SHADER,
        MASK_FRAGMENT_SHADER,
        MaterialParams {
            uniforms: vec![("radius".to_string(), UniformType::Float1)],
            pipeline_params: PipelineParams {
                color_blend: blend_alpha,
                ..Default::default()
            },
            ..Default::default()
        },
    ).unwrap();

    let sprites_material = load_material(
        DEFAULT_VERTEX_SHADER,
        SPRITE_FRAGMENT_SHADER,
        MaterialParams {
            uniforms: vec![("color".to_string(), UniformType::Float3)],
            pipeline_params: PipelineParams {
                color_blend: blend_alpha,
                ..Default::default()
            },
            ..Default::default()
        },
    ).unwrap();

    let palette = load_image(&format!("resources/Data/Palettes/{palette_name}.png")).await.unwrap();

    let draw_sprite = |x, y, w, h, entity| {
        let sprite = sprites[&entity];
        let c = default_color(entity, &palette);
        sprites_material.set_uniform("color", [c.r, c.g, c.b]);
        draw_texture_ex(
            sprite, x, y, WHITE, DrawTextureParams {
                dest_size: Some(Vec2{x: w, y: h}),
                ..Default::default()
            },
        );
    };

    let anim_time = 2.0;
    let border_color = palette.get_pixel(1, 0);
    let pause_color = Color::new(border_color.r, border_color.g, border_color.b, 0.5);
    // let button_color = Color::from_rgba(0x1B, 0x36, 0x44, 0xFF);
    // let button_hilight_color = Color::from_rgba(0x3B, 0x77, 0x97, 0xFF);

    let mut win_time = None;
    let mut paused = false;

    loop {
        // update
        let now = get_time();
        let current_input = match last_input {
            (_, None) => {
                [
                    (KeyCode::Right, Control(Go(Right))),
                    (KeyCode::Left, Control(Go(Left))),
                    (KeyCode::Up, Control(Go(Up))),
                    (KeyCode::Down, Control(Go(Down))),
                    (KeyCode::A, Control(Wait)),
                    (KeyCode::Z, Control(Undo)),
                    (KeyCode::Escape, Pause),
                ].iter()
                    .filter(|(k, _)| is_key_down(*k))
                    .map(|(k, i)| {last_input = (now, Some((*k, *i))); *i})
                    .next()
            },
            (t, Some(x)) => {
                if (now - t > 0.15 || x.1 == Control(Undo) && now - t > 0.075) && x.1 != Pause {
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
        if paused {
            if let Some(Pause) = current_input {
                paused = !paused;
            }
        } else if win_time.is_none() {
            match current_input {
                None => (),
                Some(Control(Undo)) => if history.len() > 1 {
                    history.pop();
                },
                Some(Control(i)) => {
                    let (next, win) = step(&current_state, i);
                    if win && win_time.is_none() {
                        win_time = Some(get_time());
                    }
                    history.push(next);
                },
                Some(Pause) => paused = !paused,
            };
            current_state = &history[history.len() - 1];
        }

        // render
        {
            clear_background(border_color);

            let sq_size = ((screen_width() - 20.) / width as f32).min((screen_height() - 20.) / height as f32);
            let game_width = sq_size * width as f32;
            let game_height = sq_size * height as f32;
            let offset_x = (screen_width() - game_width) / 2.;
            let offset_y = (screen_height() - game_height) / 2.;

            draw_rectangle(offset_x, offset_y, game_width, game_height, palette.get_pixel(0, 4));

            gl_use_material(sprites_material);
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
            gl_use_default_material();

            // draw pause menu
            if paused {
                draw_rectangle(
                    offset_x,
                    offset_y,
                    game_width,
                    game_height,
                    pause_color,
                );
            }

            // draw congratulations when you win
            match win_time {
                None => (),
                Some(anim_start) => {
                    if (get_time() - anim_start) > anim_time + 0.5 {
                        return history;
                    }
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

const SPRITE_FRAGMENT_SHADER: &'static str = "#version 100
precision lowp float;

varying vec2 uv;

uniform sampler2D Texture;
uniform vec3 color;

void main() {
    vec4 color_ = vec4(color.x, color.y, color.z, 1.0);
    gl_FragColor = color_ + texture2D(Texture, uv) - vec4(1.0);
}
";

const MASK_FRAGMENT_SHADER: &'static str = "#version 100
precision lowp float;

varying vec2 uv;

uniform sampler2D Texture;
uniform float radius;

void main() {
    float mask = distance(uv, vec2(0.5, 0.5)) > radius ? -1.0 : 0.0;
    gl_FragColor = texture2D(Texture, uv) + vec4(0, 0, 0, mask);
}
";

const DEFAULT_VERTEX_SHADER: &'static str = "#version 100
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
