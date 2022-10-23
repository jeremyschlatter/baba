#[macro_use]
extern crate lazy_static;

use anyhow::{anyhow, Result};
use itertools::Itertools;
use macroquad::prelude::*;
use miniquad::graphics::*;
use serde::{Serialize, Deserialize};
use strum::{EnumCount, EnumIter, EnumProperty, EnumString, IntoEnumIterator};

use std::{io::{Read, Write}, iter, collections::{HashMap, HashSet, VecDeque}, str::FromStr};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize, EnumIter, EnumString, EnumProperty)]
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
    #[strum(props(color = "1 4", text_color = "1 3", text_color_active = "1 4"))]
    Bubble,
    #[strum(props(color = "5 1", text_color = "5 0", text_color_active = "5 1"))]
    Hedge,
    #[strum(props(color = "0 1", text_color = "0 1", text_color_active = "0 2"))]
    Cog,
    #[strum(props(color = "1 1", text_color = "1 1", text_color_active = "0 1"))]
    Pipe,
    #[strum(props(color = "0 1", text_color = "1 1", text_color_active = "0 1"))]
    Robot,
    #[strum(props(color = "2 4", text_color = "2 3", text_color_active = "2 4"))]
    Bolt,
    #[strum(props(color = "6 2", text_color = "6 1", text_color_active = "6 2"))]
    Reed,
    #[strum(props(color = "5 1", text_color = "5 1", text_color_active = "5 3"))]
    Bog,
    #[strum(props(color = "6 2", text_color = "6 0", text_color_active = "6 1"))]
    Box,
    #[strum(props(color = "6 1", text_color = "6 0", text_color_active = "6 2"))]
    Stump,
    #[strum(props(color = "6 1", text_color = "5 1", text_color_active = "6 1"))]
    Husk,
    #[strum(props(color = "5 2", text_color = "5 1", text_color_active = "5 2"))]
    Tree,
    #[strum(props(color = "4 2", text_color = "4 1", text_color_active = "4 2"))]
    Ghost,
    #[strum(props(color = "6 1", text_color = "6 0", text_color_active = "6 1"))]
    Fence,
    #[strum(props(color = "6 0", text_color = "2 2", text_color_active = "2 3"))]
    Foliage,
    #[strum(props(color = "2 4", text_color = "6 1", text_color_active = "2 4"))]
    Leaf,
    #[strum(props(color = "6 1", text_color = "6 0", text_color_active = "6 2"))]
    Fungi,
    #[strum(props(color = "6 1", text_color = "6 0", text_color_active = "6 1"))]
    Fungus,
    #[strum(props(color = "4 2", text_color = "2 3", text_color_active = "2 4"))]
    Cursor,
    #[strum(props(color = "0 1", text_color = "0 1", text_color_active = "0 2"))]
    Statue,
    #[strum(props(color = "2 2", text_color = "2 1", text_color_active = "2 2"))]
    Fruit,

    #[strum(props(color = "0 3", text_color = "0 2", text_color_active = "0 3"))]
    Line(u8),

    #[strum(props(color = "0 0", text_color = "4 0", text_color_active = "4 1"))]
    Level(LevelName),
}
use Noun::*;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize)]
pub enum LevelName {
    Number(u8),
    Letter(char),
    Extra(u8),
    SubWorld(u8, usize),
    Parent,
}
use LevelName::*;

impl Default for LevelName {
    fn default() -> LevelName {
        Number(0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize, EnumIter, EnumString, EnumProperty, EnumCount)]
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
    #[strum(props(text_color = "2 1", text_color_active = "2 2"))]
    Shut,
    #[strum(props(text_color = "6 1", text_color_active = "2 4"))]
    Open,
    #[strum(props(text_color = "1 2", text_color_active = "1 4"))]
    Float,
    #[strum(props(text_color = "1 1", text_color_active = "1 2"))]
    Weak,
    #[strum(props(text_color = "1 2", text_color_active = "1 4"))]
    Tele,
    #[strum(props(text_color = "6 1", text_color_active = "6 2"))]
    Pull,
}
use Adjective::*;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, Deserialize, EnumProperty)]
pub enum Text {
    #[strum(props(text_color = "0 1", text_color_active = "0 3"))]
    Is,
    #[strum(props(text_color = "0 1", text_color_active = "0 3"))]
    And,
    #[strum(props(text_color = "4 0", text_color_active = "4 1"))]
    Text,
    #[strum(props(text_color = "0 1", text_color_active = "0 3"))]
    Has,
    #[strum(props(text_color = "2 1", text_color_active = "2 2"))]
    Not,
    Object(Noun),
    Adjective(Adjective),
}
impl FromStr for Text {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "is" => Ok(Text::Is),
            "and" => Ok(Text::And),
            "has" => Ok(Text::Has),
            "not" => Ok(Text::Not),
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
impl Direction {
    fn reverse(&self) -> Direction {
        match self {
            Up => Down,
            Down => Up,
            Left => Right,
            Right => Left,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
pub enum Entity {
    Noun(Direction, Noun),
    Text(Direction, Text),
}

impl Entity {
    fn direction(&self) -> Direction {
        match self {
            Entity::Noun(d, _) => *d,
            Entity::Text(d, _) => *d,
        }
    }
    fn to_text_or_noun(&self) -> TextOrNoun {
        match self {
            Entity::Noun(_, n) => TextOrNoun::Noun(*n),
            Entity::Text(_, _) => TextOrNoun::Text,
        }
    }
}

fn default_color(e: Entity, active: bool) -> (u32, u32) {
    let text_prop = if active { "text_color_active" } else { "text_color" };
    let ixs = match e {
        Entity::Noun(_, n) => n.get_str("color"),
        Entity::Text(_, t) => match t {
            Text::Object(n) => n.get_str(text_prop),
            Text::Adjective(a) => a.get_str(text_prop),
            _ => t.get_str(text_prop),
        },
    }.unwrap();

    (
        ixs[0..1].parse().unwrap(),
        ixs[2..3].parse().unwrap(),
    )
}

fn all_entities() -> impl Iterator<Item=Entity> {
    [3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15].into_iter().map(move |i|
            Entity::Noun(Right, Line(i)))
    .chain(iter::once(Entity::Text(Right, Text::Is)))
    .chain(iter::once(Entity::Text(Right, Text::Not)))
    .chain(iter::once(Entity::Text(Right, Text::And)))
    .chain(iter::once(Entity::Text(Right, Text::Has)))
    .chain(iter::once(Entity::Text(Right, Text::Text)))
    .chain(Noun::iter().map(move |n| Entity::Text(Right, Text::Object(n))))
    .chain(Adjective::iter().map(move |a| Entity::Text(Right, Text::Adjective(a))))
    .chain(Direction::iter().flat_map(|d|
        Noun::iter().map(move |n| Entity::Noun(d, n))))
}

fn canon(e: &Entity) -> Entity {
    match e {
        Entity::Text(_, t) => Entity::Text(Right, *t),
        Entity::Noun(_, n) => match n {
            Line(x) => Entity::Noun(Right, Line(*x)),
            Level(_) => Entity::Noun(Right, Level(LevelName::default())),
            _ => *e,
        }
    }
}

type Cell = Vec<Entity>;
type Level = Vec<Vec<Cell>>;

pub fn parse_level<P>(name: P) -> (Level, String, Vec<String>, HashMap<Noun, (u32, u32)>)
where
    P: AsRef<std::path::Path>
{
    let s = std::fs::read_to_string(name).unwrap();
    let metas: HashMap<&str, &str> =
        s.lines()
             .take_while(|&s| s != "---" && s != "+++")
             .map(|s| {
                 let mut x = s.split(" = ");
                 (x.next().unwrap(), x.next().unwrap())
             })
             .collect();
    let right_pad: usize =
        metas.get("right pad").and_then(|s| s.parse().ok()).unwrap_or_default();
    let backgrounds = metas.get("background")
        .unwrap_or(&"")
        .split(" ")
        .map(|x| x.to_string())
        .filter(|x| x != "")
        .collect::<Vec<String>>();
    enum LegendValue {
        Abbreviation(Noun),
        FullCell(Cell),
    }
    let icons = level_icon_names();
    use LegendValue::*;
    let legend: HashMap<String, LegendValue> =
        metas.iter()
             .filter(|(&k, _)| k.chars().count() == 1)
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
                                 if n == "map" {
                                     let n = x.next().unwrap().parse().unwrap();
                                     let icon = x.next().unwrap();
                                     let icon = icons.iter().position(|i| i == icon).unwrap();
                                     Entity::Noun(Right, Noun::Level(SubWorld(n, icon)))
                                 } else {
                                     let d = x.next()
                                         .and_then(|s| Direction::from_str(s).ok())
                                         .unwrap_or(Right);
                                     Entity::Noun(d, Noun::from_str(n).expect(n))
                                }
                             })
                             .collect()
                        )
                    }
                }
             ))
             .collect();
    let map: Vec<&str> = s.lines().skip_while(|s| *s != "---").skip(1).collect();

    enum Thing {
        Adj(self::Adjective),
        Txt(self::Text),
        Line(u8),
        Level(LevelName),
        Blank,
    }
    use Thing::*;
    use self::Text::*;

    fn to_cell(legend: &HashMap<String, LegendValue>, c: char) -> Vec<Entity> {
        match (
            legend.get(c.to_string().to_lowercase().as_str())
            , match c {
                '✥' => Adj(You),
                '⊘' => Adj(Stop),
                '↦' => Adj(Push),
                '✓' => Adj(Win),
                '≉' => Adj(Sink),
                '⩍' => Adj(Defeat),
                '⌇' => Adj(Hot),
                '⌢' => Adj(Melt),
                '→' => Adj(Move),
                '⨶' => Adj(Shut),
                '⧜' => Adj(Open),
                '⚲' => Adj(Float),
                '_' => Adj(Weak),
                '*' => Adj(Tele),
                '↣' => Adj(Pull),

                '=' => Txt(Is),
                '¬' => Txt(Not),
                '&' => Txt(And),
                '~' => Txt(Has),
                '@' => Txt(Text),

                '└' => Line(3),
                '─' => Line(5),
                '┘' => Line(6),
                '┴' => Line(7),
                '┌' => Line(9),
                '│' => Line(10),
                '├' => Line(11),
                '┐' => Line(12),
                '┬' => Line(13),
                '┤' => Line(14),
                '┼' => Line(15),

                '𝟙' => Level(Extra(1)),
                '𝟚' => Level(Extra(2)),
                '𝟛' => Level(Extra(3)),
                '𝟜' => Level(Extra(4)),
                '𝟝' => Level(Extra(5)),
                '𝟞' => Level(Extra(6)),

                '𝔸' => Level(Letter('a')),
                '𝔹' => Level(Letter('b')),
                'ℂ' => Level(Letter('c')),
                '𝔻' => Level(Letter('d')),
                '𝔼' => Level(Letter('e')),

                '0' => Level(Number(0)),
                '1' => Level(Number(1)),
                '2' => Level(Number(2)),
                '3' => Level(Number(3)),
                '4' => Level(Number(4)),
                '5' => Level(Number(5)),
                '6' => Level(Number(6)),
                '7' => Level(Number(7)),
                '8' => Level(Number(8)),
                '9' => Level(Number(9)),
                '𝟎' => Level(Number(10)),
                '𝟏' => Level(Number(11)),
                '𝟐' => Level(Number(12)),
                '𝟑' => Level(Number(13)),

                '•' => Level(Parent),

                _ => Blank
            })
        {
            (Some(FullCell(cell)), _) => if c.is_uppercase() {
                if let Entity::Noun(_, n) = cell[cell.len() - 1] {
                    vec![Entity::Text(Right, Object(n))]
                } else {
                    vec![]
                }
            } else {
                cell.clone()
            }
            (Some(Abbreviation(noun)), _) => if c.is_uppercase() {
                vec![Entity::Text(Right, Object(*noun))]
            } else {
                vec![Entity::Noun(Right, *noun)]
            }
            (_, Adj(adj)) => vec![Entity::Text(Right, Adjective(adj))],
            (_, Txt(txt)) => vec![Entity::Text(Right, txt)],
            (_, Line(x)) => vec![Entity::Noun(Right, Noun::Line(x))],
            (_, Level(x)) => vec![Entity::Noun(Right, Noun::Level(x))],
            (_, Blank) => vec![],
        }
    }

    let max = map.iter().map(|l| l.chars().count()).max().unwrap_or_default() + right_pad;
    let level = map.split(|s| *s == "---")
        .map(|layer| layer.iter().map(
            |line| line.chars()
                .map(|c| to_cell(&legend, c))
                .chain(iter::repeat(vec![]))
                .take(max)
                .collect::<Vec<Vec<Entity>>>()).collect::<Vec<Vec<Vec<Entity>>>>())
        .fold(vec![], |level: Vec<Vec<Vec<Entity>>>, layer: Vec<Vec<Vec<Entity>>>|
            level.into_iter()
                 .zip_longest(layer.into_iter())
                 .map(|x| x.or_default())
                 .map(|(level_line, layer_line)|
                     level_line.into_iter()
                        .zip_longest(layer_line.into_iter())
                        .map(|x| x.or_default())
                        .map(|(mut level_cell, layer_cell)| {
                            level_cell.extend(layer_cell);
                            level_cell
                        })
                        .collect::<Vec<Vec<Entity>>>()
                     )
                 .collect::<Vec<Vec<Vec<Entity>>>>());

    (
        level,
        metas.get("palette").unwrap_or(&"default").to_string(),
        backgrounds,
        {
            let mut color_overrides = HashMap::new();
            for (k, v) in metas.iter() {
                if k.starts_with("+ ") {
                    // println!("{v}, {}, {}", v[0..1], v[2);
                    let v: (u32, u32) = (v[0..1].parse().unwrap(), v[2..3].parse().unwrap());
                    for k in k[2..].split(",") {
                        if k != "" {
                            let c = to_cell(&legend, k.chars().next().unwrap());
                            if let Entity::Noun(_, n) = c[0] {
                                color_overrides.insert(n, v);
                            }
                        }
                    }
                }
            }
            color_overrides
        }
    )
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Predicate {
    HasNoun(TextOrNoun),
    IsNoun(TextOrNoun),
    IsAdjective(Adjective),
}
use Predicate::*;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
enum TextOrNoun {
    Text,
    Noun(Noun),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
enum Negatable<T> {
    Yes(T),
    No(T),
}
use Negatable::*;
impl<T> Negatable<T> {
    fn elim(self) -> (bool, T) {
        match self {
            Yes(t) => (true, t),
            No(t) => (false, t),
        }
    }
}

type Subject = Negatable<TextOrNoun>;
type Rule = (Subject, Negatable<Predicate>);

#[cfg(test)]
mod tests {
    use crate::*;
    #[test]
    fn scan_rules() {
        let is_a = |a| Yes(IsAdjective(a));
        let is = |n| Yes(IsNoun(TextOrNoun::Noun(n)));
        let n = |n| Yes(TextOrNoun::Noun(n));
        let tests = [
            (vec!["baba", "is", "you"], vec![(n(Baba), is_a(You))]),
            (
                vec!["baba", "is", "wall", "is", "push"],
                vec![
                    (n(Baba), is(Wall)),
                    (n(Wall), is_a(Push)),
                ]
            ),
            (vec!["baba", "and"], vec![]),
            (vec!["wall", "and", "keke", "is", "push"], vec![(n(Wall), is_a(Push)), (n(Keke), is_a(Push))]),
            (vec!["wall", "is", "push", "and", "stop"], vec![(n(Wall), is_a(Push)), (n(Wall), is_a(Stop))]),
            (vec!["crab", "and", "", "baba", "is", "you"], vec![(n(Baba), is_a(You))]),
            (
                vec!["baba", "and", "keke", "is", "rock", "and", "wall", "is", "door"],
                vec![
                    (n(Baba), is(Rock)),
                    (n(Baba), is(Wall)),
                    (n(Keke), is(Rock)),
                    (n(Keke), is(Wall)),
                    // notably absent: (Rock, is(Door))
                    (n(Wall), is(Door)),
                ]
            ),
            (vec!["baba", "flag", "is", "win"], vec![(n(Flag), is_a(Win))]),

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
                     .map(|t| t.iter()
                               .map(|t| Entity::Text(Right, *t))
                               .collect::<Vec<Entity>>())
                     .map(|c| ((0, 0), c))
                     .collect::<Vec<((usize, usize), Vec<Entity>)>>();
            scan_rules_line(&mut result, input_.iter().map(|(i, c)| (*i, c)));
            assert_eq!(
                output.into_iter().collect::<HashSet<Rule>>(),
                result.into_iter().map(|x| x.1).collect::<HashSet<Rule>>(),
                "input: {:?}", input,
            );
        }
    }

    #[test]
    fn replay_tests() {
        let pool = threadpool::ThreadPool::new(
            std::thread::available_parallelism().map(|n| n.get()).unwrap_or(10));
        let (tx, rx) = std::sync::mpsc::channel::<
            (String, Result<(), (Diff, Replay)>)
        >();

        let goldens = walkdir::WalkDir::new("goldens")
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| !e.file_type().is_dir())
            .collect::<Vec<_>>();
        let n = goldens.len();

        for entry in goldens {
            let tx = tx.clone();
            pool.execute(move|| {
                let (screens, inputs, palette_name) = load::<_, Replay>(entry.path()).unwrap();

                let mut n = 1;
                for i in 0..screens.len() - 1 {
                    if inputs[i] == Undo {
                        if n > 0 { n -= 1 };
                        continue;
                    }
                    let (got, _) = step(&screens[i], inputs[i], n);
                    n += 1;
                    if got != screens[i + 1] {
                        tx.send((
                            format!("{}", entry.path().display()),
                            Err((
                                (
                                    screens[i].clone(),
                                    screens[i+1].clone(),
                                    got.clone(),
                                    palette_name.to_string(),
                                    inputs[i],
                                ),
                                (
                                    {
                                        let mut screens = vec![screens[0].clone()];
                                        let mut i = 0;
                                        for input in &inputs {
                                            if *input == Undo {
                                                if i > 0 { i -= 1 }
                                                screens.push(screens[i].clone());
                                                continue;
                                            }
                                            screens.push(step(&screens[i], *input, i as u32).0);
                                            i = screens.len() - 1;
                                        }
                                        screens
                                    },
                                    inputs,
                                    palette_name.to_string(),
                                ),
                            ))
                        )).unwrap();
                        return;
                    }
                }

                tx.send((format!("{}", entry.path().display()), Ok(()))).unwrap();
            });
        }

        let results = rx.into_iter().take(n).collect::<Vec<_>>();

        for r in results {
            if let (s, Err((diff, replay))) = r {
                println!("{s}");
                save::<_, Diff>(&diff, "diff.ron.br").unwrap();
                save::<_, Replay>(&replay, "replay.ron.br").unwrap();
                assert!(std::process::Command::new("cargo")
                    .args(&["run", "render-diff", "diff.ron.br"])
                    .status()
                    .unwrap()
                    .success());
                assert!(false, "replay mismatch. saved diff and new replay");
            }
        }
    }
}

fn scan_rules_text(rules: &mut Vec<(Vec<Index>, Rule)>, text: &[(Index, Text)]) {
    #[derive(Debug)]
    enum ListState {
        Complete,
        Incomplete(Option<Index>),
    }
    use ListState::*;
    #[derive(Debug)]
    enum IsOrHas { Is, Has }
    #[derive(Debug)]
    enum ScanState {
        Subjects(ListState, Vec<(Vec<Index>, Subject)>, bool),
        Predicates(IsOrHas, ListState, bool, Vec<(Vec<Index>, Subject)>, Option<(Vec<Index>, Predicate)>),
    }
    use ScanState::*;
    use Text::{Is, And, Not, Has, Object, Adjective};
    let zero = |mut v: Vec<(Vec<Index>, Subject)>| {
        v.clear();
        Subjects(Incomplete(None), v, true)
    };
    let mut state = zero(vec![]);
    let mut i = 0;
    fn cat_ixs(head: Option<Index>, tail: Index) -> Vec<Index> {
        if let Some(ix) = head { vec![ix, tail] } else { vec![tail] }
    }
    fn from_bool<T>(b: bool, t: T) -> Negatable<T> {
        if b { Yes(t) } else { No(t) }
    }
    while i < text.len() {
        let (ix, t) = text[i];
        i += 1;
        state = match state {
            Subjects(s, mut subjs, yes_or_no) => match s {
                Incomplete(opt_ix) => match t {
                    Not => Subjects(s, subjs, !yes_or_no),
                    Object(noun) => {
                        subjs.push((cat_ixs(opt_ix, ix), from_bool(yes_or_no, TextOrNoun::Noun(noun))));
                        Subjects(Complete, subjs, true)
                    },
                    Text::Text => {
                        subjs.push((cat_ixs(opt_ix, ix), from_bool(yes_or_no, TextOrNoun::Text)));
                        Subjects(Complete, subjs, true)
                    },
                    _ => zero(subjs),
                },
                Complete => match t {
                    Is => Predicates(IsOrHas::Is, Incomplete(Some(ix)), true, subjs, None),
                    Has => Predicates(IsOrHas::Has, Incomplete(Some(ix)), true, subjs, None),
                    And => Subjects(Incomplete(Some(ix)), subjs, true),
                    _ => { i -= 1; zero(subjs) },
                },
            },
            Predicates(is_or_has, s, yes_or_no, subjs, pred) => match s {
                Incomplete(opt_ix) => match (pred, t) {
                    (_, Not) => Predicates(is_or_has, s, !yes_or_no, subjs, None),
                    (Some(_), Is) =>
                        Predicates(IsOrHas::Is, Incomplete(Some(ix)), true, subjs, None),
                    (Some(_), Has) =>
                        Predicates(IsOrHas::Has, Incomplete(Some(ix)), true, subjs, None),
                    _ => match match is_or_has {
                        IsOrHas::Is => match t {
                            Object(noun) => Some(IsNoun(TextOrNoun::Noun(noun))),
                            Adjective(adj) => Some(IsAdjective(adj)),
                            Text::Text => Some(IsNoun(TextOrNoun::Text)),
                            _ => None,
                        },
                        IsOrHas::Has => match t {
                            Object(noun) => Some(HasNoun(TextOrNoun::Noun(noun))),
                            Text::Text => Some(HasNoun(TextOrNoun::Text)),
                            _ => None,
                        }
                    } {
                        Some(pred) => {
                            for (ixs, s) in &subjs {
                                let mut ixs = ixs.clone();
                                ixs.extend(cat_ixs(opt_ix, ix));
                                rules.push((ixs, (*s, from_bool(yes_or_no, pred))));
                            }
                            Predicates(is_or_has, Complete, true, subjs, Some((cat_ixs(opt_ix, ix), pred)))
                        },
                        None => { i -= 1; zero(subjs) },
                    },
                },
                Complete => match t {
                    And => Predicates(is_or_has, Incomplete(Some(ix)), true, subjs, pred),
                    _ => {
                        i -= 2; // to pick up the last subject word, if present
                        zero(subjs)
                    },
                },
            },
        };
    }
}

fn scan_rules_line<'a, I>(rules: &mut Vec<(Vec<Index>, Rule)>, line: I)
where
    I: Iterator<Item = ((usize, usize), &'a Cell)>,
{
    fn chunks(i: Vec<Vec<(Index, Text)>>) -> Vec<Vec<Vec<(Index, Text)>>> {
        i.iter().fold(vec![vec![]], |mut acc: Vec<Vec<Vec<(Index, Text)>>>, v: &Vec<(Index, Text)>| {
            if v.len() == 0 {
                acc.push(vec![]);
            } else {
                let l = acc.len();
                acc[l - 1].push(v.clone());
            }
            acc
        })
    }

    fn branches(input: Vec<Vec<(Index, Text)>>) -> Vec<Vec<(Index, Text)>> {
        input.iter()
         .fold(vec![vec![]], |acc: Vec<Vec<usize>>, v: &Vec<(Index, Text)>| {
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
                       .collect::<Vec<(Index, Text)>>())
         .collect()
    }

    let x: Vec<Vec<(Index, Text)>> =
       line.map(|((x, y), c)| c.iter()
                     .enumerate()
                     .filter_map(|(i, &e)| match e {
                         Entity::Text(_, t) => Some(((x, y, i), t)),
                         _ => None,
                     }).collect()
           ).collect();

    for chunk in chunks(x) {
        for branch in branches(chunk) {
            scan_rules_text(rules, branch.as_slice());
        }
    }
}

type Index = (usize, usize, usize);

fn scan_rules(l: &Level) -> Vec<(Vec<Index>, Rule)> {
    let mut rules = vec![];

    if l.len() > 0 {
        for (y, row) in l.iter().enumerate() {
            scan_rules_line(
                &mut rules,
                row.iter()
                   .enumerate()
                   .map(|(x, cell)| ((x, y), cell)));
        }
        for col in 0..l[0].len() {
            let mut row: i32 = -1;
            scan_rules_line(
                &mut rules,
                iter::from_fn(|| {
                    row += 1;
                    if row as usize >= l.len() {
                        None
                    } else {
                        Some(((col, row as usize), &l[row as usize][col]))
                    }
                })
            );
        }
    }

    rules
}

fn scan_rules_no_index(l: &Level) -> Vec<Rule> {
    let rules = scan_rules(l);
    let (rules, _) = partition_overridden_rules(rules);
    rules.into_iter().map(|x| x.1).collect()
}

fn partition_overridden_rules<A>(rules: Vec<(A, Rule)>)
    -> (Vec<(A, Rule)>, Vec<(A, Rule)>)
{
    let (no, yes) = {
        let mut no = vec![];
        let mut yes = vec![];
        for (a, x) in rules {
            match x {
                (s, No(p)) => no.push((a, (s, p))),
                (s, Yes(p)) => yes.push((a, (s, p))),
            }
        }
        (no, yes)
    };

    let (vetoed, the_rest): (Vec<_>, _) = yes.into_iter().partition(|(_, (s, p))|
        no.iter().any(|(_, (ns, np))| np == p && {
            // need to yield true iff ns is a superset of s
            match (s, ns) {
                (Yes(s), No(ns)) => s != ns,
                (s, ns) => s == ns,
            }
        })
    );

    // x is x
    let (x_is_x, the_rest): (Vec<_>, _) = the_rest.into_iter().partition(|r| match r.1 {
        (Yes(x), IsNoun(y)) => x == y,
        _ => false,
    });

    let unchanging = x_is_x.iter()
        .map(|(_, (x, _))| match x { Yes(x) => *x, No(_) => panic!("logic error") })
        .collect::<HashSet<TextOrNoun>>();

    let (overridden, the_rest) = the_rest.into_iter().partition(|r| match r.1 {
        (Yes(x), IsNoun(_)) => unchanging.contains(&x),
        _ => false,
    });

    fn reapply<A, C>(v: Vec<(A, (Subject, Predicate))>, c: C) -> Vec<(A, Rule)>
    where
        C: Fn(Predicate) -> Negatable<Predicate>
    {
        v.into_iter().map(|(a, (s, p))| (a, (s, c(p)))).collect()
    }

    let mut active = reapply(no, No);
    active.extend(reapply(x_is_x, Yes));
    active.extend(reapply(the_rest, Yes));

    let mut overridden = reapply(overridden, Yes);
    overridden.extend(reapply(vetoed, Yes));

    (active, overridden)
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
pub enum Input {
    Go(Direction),
    Wait,
    Undo,
}
use Input::*;

fn step(l: &Level, input: Input, n: u32) -> (Level, bool) {
    let mut level = l.clone();
    let rules = scan_rules_no_index(&level);

    let width = level[0].len();
    let height = level.len();

    fn delta(level: &Level, d: Direction, x: usize, y: usize) -> (usize, usize) {
        let width = level[0].len();
        let height = level.len();
        let (dx, dy) = match d {
            Up    => (0, -1),
            Down  => (0,  1),
            Left  => (-1, 0),
            Right => ( 1, 0),
        };
        (0.max(x as i16 + dx).min(width as i16 - 1) as usize,
         0.max(y as i16 + dy).min(height as i16 - 1) as usize)
    }

    fn entities(level: &Level) -> impl Iterator<Item=((usize, usize, usize), Entity)> + '_ {
        level.iter()
             .enumerate()
             .flat_map(move |(y, row)|
                 row.iter()
                    .enumerate()
                    .flat_map(move |(x, cell)|
                        cell.iter()
                            .enumerate()
                            .map(move |(i, e)| ((x, y, i), *e))))
    }

    type RulesCache = (
        [[Vec<Subject>; 2]; Adjective::COUNT], // is adjective
        [Vec<(Subject, TextOrNoun)>; 2], // has noun
        [Vec<(Subject, TextOrNoun)>; 2], // is noun
    );

    fn cache_rules(rules: &Vec<Rule>) -> RulesCache {
        let mut result: RulesCache = (
            core::array::from_fn(|_| [vec![], vec![]]),
            [vec![], vec![]],
            [vec![], vec![]],
        );
        result.0[Push as usize][0].push(Yes(TextOrNoun::Text));
        for (s, p) in rules {
            let (yes, p) = p.elim();
            let ix = !yes as usize;
            match p {
                IsAdjective(adj) => result.0[adj as usize][ix].push(*s),
                HasNoun(n) => result.1[ix].push((*s, n)),
                IsNoun(n) => result.2[ix].push((*s, n)),
            }
        }
        result
    }

    fn subject_match(level: &Level, x: usize, y: usize, i: usize, s: &Subject) -> bool {
        let t_or_n = level[y][x][i].to_text_or_noun();
        match (*s, t_or_n) {
            (Yes(y), _) => y == t_or_n,
            (No(n), TextOrNoun::Noun(_)) => n != t_or_n,
            (No(_), TextOrNoun::Text) => false,
        }
    }

    fn array_map_ref<T, F, U>(a: &[T; 2], f: F) -> [U; 2]
    where
        F: Fn(&T) -> U
    {
        [f(&a[0]), f(&a[1])]
    }

    fn is(level: &Level, x: usize, y: usize, i: usize, rules: &RulesCache, quality: Adjective) -> bool {
        let [yes, no] = array_map_ref(&rules.0[quality as usize],
            |v: &Vec<Subject>| v.iter().any(|s| subject_match(level, x, y, i, s)));
        yes && !no
    }

    fn is_or_has(level: &Level, x: usize, y: usize, i: usize, rules: &[Vec<(Subject, TextOrNoun)>; 2]) -> Vec<Entity> {
        let [yes, no] = array_map_ref(&rules,
            |v| v.iter()
                 .filter(|(s, _)| subject_match(level, x, y, i, s))
                 .map(|(_, e)| e)
                 .copied()
                 .collect::<HashSet<TextOrNoun>>());

        let mut result = yes.difference(&no).copied().collect::<Vec<TextOrNoun>>();
        result.sort();
        let e = level[y][x][i];
        let d = e.direction();
        result
          .iter()
          .map(|x| match x {
              TextOrNoun::Noun(n) => Entity::Noun(d, *n),
              TextOrNoun::Text => Entity::Text(d, match e {
                  Entity::Noun(_, n) => Text::Object(n),
                  Entity::Text(_, _) => Text::Text,
              }),
          })
          .collect()
    }

    fn has(level: &Level, x: usize, y: usize, i: usize, rules: &RulesCache) -> Vec<Entity> {
        is_or_has(level, x, y, i, &rules.1)
    }

    fn is_noun(level: &Level, x: usize, y: usize, i: usize, rules: &RulesCache) -> Vec<Entity> {
        let v = is_or_has(level, x, y, i, &rules.2);
        if v.contains(&level[y][x][i]) {
            vec![] // x is x
        } else {
            v
        }
    }

    // move things
    fn move_things(level: &mut Level, rules: &Vec<Rule>, movers: Vec<(usize, usize, usize, Direction, bool)>) {
        let rules_cache = cache_rules(rules);
        let width = level[0].len();
        let height = level.len();

        #[derive(PartialEq, Eq, Hash)]
        enum Status {
            Pending { flipped: bool },
            Resolved { moving: bool },
        }
        #[derive(PartialEq, Eq, Hash)]
        struct Arrow {
            dir: Direction,
            status: Status,
            is_move: bool,
        }

        let try_move = |queue: &mut VecDeque<(usize, usize, usize)>, movements: &mut HashMap<(usize, usize, usize), Arrow>, x, y, i, d, m| {
            let arrow = Arrow {
                dir: d,
                status: Status::Pending { flipped: false },
                is_move: m,
            };
            queue.push_back((x, y, i));
            movements.insert((x, y, i), arrow);
        };
        let mut queue = VecDeque::new();
        let mut movements = HashMap::new();
        let mut tombstones = HashSet::new();
        let is = |x, y, i, q| is(&level, x, y, i, &rules_cache, q);

        for (x, y, i, d, b) in movers {
            try_move(&mut queue, &mut movements, x, y, i, d, b);
        }

        // until the queue is empty:
        //   pull arrow from queue
        //   match (can move to adjacent cell):
        //     Yes => mark resolved yes
        //     No => mark resolved no (or delete it?)
        //     Depends On arrows => throw it to the back of the queue
        //                    // proof of termination:
        //                    // arrows must be in queue since they are unresolved
        //                    // so will process them before we return to this one
        //                    // they cannot depend on this one because can only
        //                    // depend on arrows in front moving same direction.
        while let Some((x, y, i)) = queue.pop_front() {
            if tombstones.contains(&(x, y, i)) {
                continue;
            }

            // check for push
            {
                let a = &movements[&(x, y, i)];
                let (x_, y_) = delta(&level, a.dir, x, y);
                if !(x == x_ && y == y_) {
                    let d = a.dir;
                    let mut pushed = false;
                    for i in 0..level[y_][x_].len() {
                        if tombstones.contains(&(x_, y_, i)) {
                            continue;
                        }
                        if is(x_, y_, i, Push) {
                            if !movements.contains_key(&(x_, y_, i)) {
                                try_move(&mut queue, &mut movements, x_, y_, i, d, false);
                                pushed = true;
                            }
                        }
                    }
                    if pushed {
                        queue.push_back((x, y, i));
                        continue;
                    }
                }
            }

            let a = &movements[&(x, y, i)];

            let (x_, y_) = delta(&level, a.dir, x, y);

            let at_unmoving_stop = {
                let mut result = Some(false);
                if x == x_ && y == y_ {
                    // at edge
                    result = Some(true);
                }
                for j in 0..level[y_][x_].len() {
                    if tombstones.contains(&(x_, y_, j)) {
                        continue;
                    }
                    let open = is(x_, y_, j, Open) && is(x, y, i, Shut)
                            || is(x_, y_, j, Shut) && is(x, y, i, Open);
                    if open {
                        // add tombstones to both
                        tombstones.insert((x, y, i));
                        tombstones.insert((x_, y_, j));
                        continue;
                    }
                    let stop = is(x_, y_, j, Stop);
                    let push = is(x_, y_, j, Push);
                    let pull = is(x_, y_, j, Pull);
                    let weak = is(x_, y_, j, Weak);
                    if weak || result == Some(true) || !push && !stop && !pull {
                        continue;
                    }
                    result = match movements.get(&(x_, y_, j)) {
                        Some(b) if a.dir == b.dir => match b.status {
                            Status::Resolved { moving } =>
                                if moving { result } else { Some(true) },
                            _ => None,
                        },
                        _ => if stop || pull { Some(true) } else { result },
                    }
                }
                result
            };

            let flipped =
                if let Status::Pending { flipped } = a.status {
                    flipped
                } else {
                    continue; // should be impossible
                };

            match at_unmoving_stop {
                Some(true) => {
                    let a = movements.get_mut(&(x, y, i)).unwrap();
                    if is(x, y, i, Weak) && !a.is_move {
                        tombstones.insert((x, y, i));
                        continue;
                    }
                    if a.is_move && !flipped {
                        a.dir = match a.dir {
                            Left  => Right,
                            Right => Left,
                            Up    => Down,
                            Down  => Up,
                        };
                        a.status = Status::Pending { flipped: true };
                        queue.push_back((x, y, i));
                    } else {
                        a.status = Status::Resolved { moving: false };
                    }
                    continue;
                },
                None => {
                    queue.push_back((x, y, i));
                    continue;
                },
                Some(false) => (),
            }

            // check for pull
            {
                let dir = movements.get(&(x, y, i)).unwrap().dir;
                let (x_, y_) = delta(&level, dir.reverse(), x, y);
                if !(x == x_ && y == y_) {
                    for i in 0..level[y_][x_].len() {
                        if is(x_, y_, i, Pull) {
                            if !movements.contains_key(&(x_, y_, i)) {
                                try_move(&mut queue, &mut movements, x_, y_, i, dir, false);
                            }
                        }
                    }
                }
            }

            let a = movements.get_mut(&(x, y, i)).unwrap();
            a.status = Status::Resolved { moving: true };
        }

        // flush movements along with the deletions and insertions they generated
        {
            // remove all movers from their current position
            // and remove all tombstoned things
            let mut removals = vec![vec![vec![]; width]; height];
            for &(x, y, i) in tombstones.iter() {
                removals[y][x].push((i, None));
            }
            for (&(x, y, i), a) in movements.iter() {
                if !tombstones.contains(&(x, y, i)) {
                    if a.status == (Status::Resolved { moving: true }) {
                        removals[y][x].push((i, Some((level[y][x][i], a.dir))));
                    }
                }
            }
            for x in 0..width {
                for y in 0..height {
                    removals[y][x].sort_by(|a, b| a.0.cmp(&b.0));
                    let mut n_inserts = 0;
                    for (i, (r, _)) in removals[y][x].iter().enumerate() {
                        let ix = n_inserts + r - i;
                        let inserts = if tombstones.contains(&(x, y, ix)) {
                            has(&level, x, y, ix, &rules_cache)
                        } else { vec![] };
                        level[y][x].remove(ix);
                        for e in &inserts {
                            level[y][x].insert(ix, *e)
                        }
                        n_inserts += inserts.len();
                    }
                }
            }

            // add them to their new position
            for (y, row) in removals.iter().enumerate() {
                for (x, cell) in row.iter().enumerate() {
                    for &(_, o) in cell {
                        if let Some((e, d)) = o {
                            let (x, y) = delta(&level, d, x, y);
                            level[y][x].push(match e {
                                Entity::Text(_, t) => Entity::Text(d, t),
                                Entity::Noun(_, n) => Entity::Noun(d, n),
                            });
                        }
                    }
                }
            }
        }
    }

    let rules_cache = cache_rules(&rules);

    // move you
    if let Go(d) = input {
        let m = entities(&level)
            .filter_map(|((x, y, i), _)| match is(&level, x, y, i, &rules_cache, You) {
                true => Some((x, y, i, d, false)),
                false => None,
            }).collect();
        move_things(&mut level, &rules, m);
    }

    // move cursor
    if let Go(d) = input {
        'top:
        for col in 0..level.len() {
            for row in 0..level[0].len() {
                for i in 0..level[col][row].len() {
                    if let Entity::Noun(_, Noun::Cursor) = level[col][row][i] {
                        let (x, y) = delta(&level, d, row, col);
                        for j in 0..level[y][x].len() {
                            if let Entity::Noun(_, n) = level[y][x][j] {
                                if match n { Line(_) => true, Level(_) => true, _ => false } {
                                    level[col][row].remove(i);
                                    level[y][x].push(Entity::Noun(d, Noun::Cursor));
                                    break 'top;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // move move
    {
        let m = entities(&level)
            .filter_map(|((x, y, i), e)| match is(&level, x, y, i, &rules_cache, Move) {
                true => Some((x, y, i, e.direction(), true)),
                false => None,
            }).collect();
        move_things(&mut level, &rules, m);
    }

    type SelectT = ((usize, usize), Vec<usize>, Vec<usize>);
    fn select(
        level: &Level, rules_cache: &RulesCache, a: Adjective, b: Option<Adjective>,
    ) -> Vec<SelectT> {
        let mut result = vec![];
        let is = |x, y, i, q| is(&level, x, y, i, rules_cache, q);
        for y in 0..level.len() {
            for x in 0..level[0].len() {
                for float in [true, false] {
                    let mut as_ = vec![];
                    let mut bs = vec![];
                    for i in 0..level[y][x].len() {
                        if is(x, y, i, Float) == float {
                            if is(x, y, i, a) {
                                as_.push(i);
                            }
                            if b.map(|b| is(x, y, i, b)).unwrap_or(true) {
                                bs.push(i);
                            }
                        }
                    }
                    if as_.len() > 0 && bs.len() > 0 {
                        result.push(((x, y), as_, bs));
                    }
                }
            }
        }
        result
    }

    // rescan since we might have just moved some new rules into place
    let rules = scan_rules_no_index(&level);
    let rules_cache = cache_rules(&rules);

    // change things into other things
    for x in 0..width {
        for y in 0..height {
            for i in (0..level[y][x].len()).rev() {
                let changed = is_noun(&level, x, y, i, &rules_cache);
                if changed.len() > 0 {
                    level[y][x].remove(i);
                    for new in changed.into_iter().rev() {
                        level[y][x].insert(i, new);
                    }
                }
            }
        }
    }

    // delete things
    {
        let mut deletions: Vec<Vec<Vec<usize>>> = vec![vec![vec![]; width]; height];
        let mut delete_all = |(x, y): (usize, usize), ixs: Vec<usize>| {
            for i in ixs {
                if !deletions[y][x].contains(&i) {
                    deletions[y][x].push(i);
                }
            }
        };
        let select_occupied = |a| {
            let mut r = select(&level, &rules_cache, a, None);
            r.retain(|(_, _, bs)| bs.len() > 1);
            r
        };
        let select_intersecting = |a, b| select(&level, &rules_cache, a, Some(b));

        // sink
        for (xy, _, ixs) in select_occupied(Sink) { delete_all(xy, ixs) }

        // defeat
        for (xy, _, yous) in select_intersecting(Defeat, You) { delete_all(xy, yous) }

        // melt
        for (xy, _, melts) in select_intersecting(Hot, Melt) { delete_all(xy, melts) }

        // open and shut
        for (xy, mut opens, mut shuts) in select_intersecting(Open, Shut) {
            let n = opens.len().min(shuts.len());
            opens.truncate(n);
            shuts.truncate(n);
            delete_all(xy, opens);
            delete_all(xy, shuts);
        }

        // weak
        for (xy, weaks, _) in select_occupied(Weak) { delete_all(xy, weaks) }

        // flush deletions
        for y in 0..height {
            for x in 0..width {
                let d = &mut deletions[y][x];
                d.sort();
                let mut n_inserts = 0;
                for i in 0..d.len() {
                    let ix = n_inserts + d[i] - i;
                    let inserts = has(&level, x, y, ix, &rules_cache);
                    level[y][x].remove(ix);
                    for e in &inserts {
                        level[y][x].insert(ix, *e)
                    }
                    n_inserts += inserts.len();
                }
            }
        }
    }

    // teleport things
    {
        let mut rng = oorandom::Rand32::new(n as u64);
        fn tele_dest(rng: &mut oorandom::Rand32, pads: &Vec<(usize, usize)>, x: usize, y: usize) -> (usize, usize) {
            let dests = pads.iter()
                .filter(|xy| **xy != (x, y))
                .collect::<Vec<_>>();
            *dests[rng.rand_range(0..dests.len() as u32) as usize]
        }
        let teles = select(&level, &rules_cache, Tele, None);
        let mut pads = teles.iter().map(|x| x.0).collect::<Vec<(usize, usize)>>();
        pads.dedup();
        if pads.len() > 1 {
            let mut travelers: HashMap<(usize, usize), Vec<(usize, Entity, (usize, usize))>>
                = HashMap::new();
            for ((x, y), _, all) in teles {
                for i in all {
                    if !is(&level, x, y, i, &rules_cache, Tele) {
                        travelers.entry((x, y)).or_insert(vec![]).push((
                            i, level[y][x][i],
                            tele_dest(&mut rng, &pads, x, y),
                        ));
                    }
                }
            }
            for ((x, y), ts) in travelers.iter_mut() {
                ts.sort_by(|a, b| a.0.cmp(&b.0));
                for i in 0..ts.len() {
                    level[*y][*x].remove(ts[i].0 - i);
                }
            }
            for (_, ts) in travelers.into_iter() {
                for (_, e, (x, y)) in ts {
                    level[y][x].push(e);
                }
            }
        }
    }

    // check for win
    let win = select(&level, &rules_cache, You, Some(Win)).len() > 0;
    (level, win)
}

type Diff = (Level, Level, Level, String, Input);

pub async fn render_diff(path: &str) {
    let (start, good, bad, palette_name, input) = load::<_, Diff>(path).unwrap();
    let palette = load_image(&format!("resources/original/Data/Palettes/{palette_name}.png")).await.unwrap();
    let sprites = load_sprite_map();
    let render = |s, x, y, w, h| render_level(s, &palette, &sprites, &[], &HashMap::new(), Rect::new(x, y, w, h), 20.);
    let font_size = 20.;
    let font_color = WHITE;
    loop {
        let width = screen_width();
        let height = screen_height();

        // update
        if is_key_down(KeyCode::Escape) {
            return;
        }

        // render
        {
            clear_background(palette.get_pixel(1, 0));

            // before
            {
                let bounds = render(&start, 0., 0., width / 2., height);
                draw_text(
                    &format!("input: {}", match input {
                        Go(Down) => "down",
                        Go(Up) => "up",
                        Go(Left) => "left",
                        Go(Right) => "right",
                        Wait => "wait",
                        Undo => "undo",
                    }),
                    width / 4. - 40.,
                    bounds.y + bounds.h + 20.,
                    font_size,
                    font_color,
                );
            }

            // after (good)
            let good_bounds = render(&good, width / 2., 0., width / 2., height / 2.);
            draw_text(
                "good:",
                width / 2. - 50.,
                10. + good_bounds.y,
                font_size,
                font_color,
            );

            // after (maybe bad)
            let bad_bounds = render(&bad, width / 2., height / 2., width / 2., height / 2.);
            draw_text(
                "bad:",
                width / 2. - 50.,
                bad_bounds.y + bad_bounds.h - (font_size / 2.),
                font_size,
                font_color,
            );
            for y in 0..bad.len() {
                for x in 0..bad[0].len() {
                    if bad[y][x] != good[y][x] {
                        let sq_w = bad_bounds.w / bad[0].len() as f32;
                        let sq_h = bad_bounds.h / bad.len() as f32;
                        draw_rectangle(
                            good_bounds.x + x as f32 * sq_w,
                            good_bounds.y + y as f32 * sq_h,
                            sq_w,
                            sq_h,
                            Color::new(0., 1., 0., 0.2),
                        );
                        draw_rectangle(
                            bad_bounds.x + x as f32 * sq_w,
                            bad_bounds.y + y as f32 * sq_h,
                            sq_w,
                            sq_h,
                            Color::new(1., 0., 0., 0.2),
                        );
                    }
                }
            }
        }

        next_frame().await;
    }
}

pub async fn replay(path: &str) {
    let (screens, _, palette_name) = load::<_, Replay>(path).unwrap();
    let palette = load_image(&format!("resources/original/Data/Palettes/{palette_name}.png")).await.unwrap();
    let sprites = load_sprite_map();

    let mut last_input: (f64, Option<KeyCode>) = (0., None);
    let mut i = 0;

    loop {
        // update
        match debounce(
            &mut last_input,
            &[
                (KeyCode::Right, Right),
                (KeyCode::Left, Left),
            ],
            |_, t| t > 0.15,
        ) {
            Some(Right) => if i + 1 < screens.len() { i += 1 },
            Some(Left) => if i > 0 { i -= 1 },
            _ => (),
        }

        // render
        clear_background(palette.get_pixel(1, 0));
        render_level(
            &screens[i],
            &palette,
            &sprites,
            &[],
            &HashMap::new(),
            Rect::new(0., 0., screen_width(), screen_height()),
            20.,
        );

        next_frame().await
    }
}

pub type Replay = (Vec<Level>, Vec<Input>, String);

pub fn load<P: AsRef<std::path::Path>, T>(path: P) -> Result<T>
where
    T: for<'a> Deserialize<'a>
{
    let mut scratch = String::new();
    brotli::Decompressor::new(
        std::fs::File::open(path)?, 4096,
    ).read_to_string(&mut scratch)?;
    Ok(ron::from_str(&scratch)?)
}

pub fn save<P: AsRef<std::path::Path>, T>(x: &T, path: P) -> Result<()>
where
    T: ?Sized + Serialize
{
    Ok(
        brotli::CompressorWriter::new(
            std::fs::File::create(path)?, 4096, 9, 20,
        ).write_all(ron::to_string(x)?.as_bytes())?
    )
}

fn debounce<A, R>(prev: &mut (f64, Option<KeyCode>), keymap: &[(KeyCode, A)], repeat: R) -> Option<A>
where
    A: Copy,
    R: Fn(A, f64) -> bool
{
    let now = get_time();
    if let (_, Some(k)) = prev {
        if !is_key_down(*k) {
            *prev = (now, None);
        }
    }
    let can_repeat = |prev: &(f64, Option<KeyCode>), k: KeyCode, a: A|
        match prev {
            (t, Some(x)) => *x != k || repeat(a, now - *t),
            (_, None) => true,
        };
    keymap.iter()
          .filter(|(k, a)| is_key_down(*k) && can_repeat(prev, *k, *a))
          .next()
          .map(|&(k, a)| { *prev = (now, Some(k)); a })
}

#[derive(Debug)]
struct LevelGraph {
    path: std::path::PathBuf,
    sub_levels: HashMap<LevelName, LevelGraph>,
}

fn parse_level_graph<P: AsRef<std::path::Path>>(path: P) -> Result<LevelGraph> {
    fn to_level_name(p: &std::path::Path) -> Result<LevelName> {
        let f = p.file_name().and_then(|x| x.to_str()).ok_or(anyhow!("un-string-able path: {p:?}"))?;
        let nm = f.split("-").next().ok_or(anyhow!("need dashes in level name: {p:?}"))?;
        if p.is_dir() {
            return Ok(SubWorld(nm.parse()?, 0));
        }
        if let Ok(n) = nm.parse::<u8>() {
            Ok(Number(n))
        } else if let Ok(c) = nm.parse::<char>() {
            Ok(Letter(c))
        } else if nm == "extra" {
            let nm = f.split("-").skip(1).next().ok_or(anyhow!("need dashes in level name: {p:?}"))?;
            Ok(Extra(nm.parse::<u8>()?))

        } else {
            anyhow::bail!("level name doesn't fit normal pattern: {p:?}")
        }
    }
    let path = path.as_ref();
    Ok(if path.is_dir() {
        LevelGraph {
            path: path.join("index.txt"),
            sub_levels: {
                let mut m = HashMap::new();
                for e in path.read_dir()? {
                    let path = e?.path();
                    if path.file_name() != Some(std::ffi::OsStr::new("index.txt")) {
                        m.insert(to_level_name(&path)?, parse_level_graph(&path)?);
                    }
                }
                m
            },
        }
    } else {
        LevelGraph{ path: path.into(), sub_levels: HashMap::new() }
    })
}

pub async fn play_overworld(level: &str) {
    let sprites = load_sprite_map();

    fn erase_icon(l: LevelName) -> LevelName {
        match l {
            SubWorld(i, _) => SubWorld(i, 0),
            _ => l,
        }
    }

    use LevelResult::*;
    let level = parse_level_graph(level).unwrap();
    let mut stack = vec![(&level, None)];
    loop {
        let (level, ix) = if let Some(x) = stack.pop() { x } else { return; };
        match play_level(&sprites, &level.path, ix).await {
            Win(_) => (),
            Exit => (),
            Enter(Parent) => (),
            Enter(lvl) => {
                stack.push((&level, Some(lvl)));
                stack.push((&level.sub_levels[&erase_icon(lvl)], None));
            },
        };
    }
}

pub enum LevelResult {
    Win(Replay),
    Exit,
    Enter(LevelName),
}

pub async fn play_level<P>(sprites: &SpriteMap, level: P, cursor: Option<LevelName>) -> LevelResult
where
    P: AsRef<std::path::Path>
{
    let (mut level, palette_name, backgrounds, color_overrides) = parse_level(level);

    fn place_cursor(level: &mut Level, at: LevelName) -> bool {
        for y in 0..level.len() {
            for x in 0..level[0].len() {
                for i in 0..level[y][x].len() {
                    if let Entity::Noun(_, Level(l)) = level[y][x][i] {
                        if l == at {
                            level[y][x].push(Entity::Noun(Right, Cursor));
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    if !place_cursor(&mut level, cursor.unwrap_or(Parent)) {
        place_cursor(&mut level, Number(0));
    }

    let level = level; // drop mutability

    let mut inputs: Vec<Input> = vec![];
    // views is different from history, below, in that the Undo action
    // pops from history and pushes onto views.
    let mut views: Vec<Level> = vec![level.clone()];

    let congrats = sprites.2;

    let mut history = vec![level.clone()];
    let mut current_state = &history[0];

    let palette = load_image(&format!("resources/original/Data/Palettes/{palette_name}.png")).await.unwrap();

    let anim_time = 2.0;
    let border_color = palette.get_pixel(1, 0);
    let pause_color = Color::new(border_color.r, border_color.g, border_color.b, 0.5);
    // let button_color = Color::from_rgba(0x1B, 0x36, 0x44, 0xFF);
    // let button_hilight_color = Color::from_rgba(0x3B, 0x77, 0x97, 0xFF);

    let mut win_time = None;
    let mut paused = false;

    #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
    enum UIInput {
        Control(Input),
        Pause,
        Enter,
    }
    use UIInput::*;

    let mut last_input: (f64, Option<KeyCode>) = (0., None);
    loop {
        if !is_key_down(KeyCode::Escape) && !is_key_down(KeyCode::Enter) {
            break;
        }
        next_frame().await
    }

    loop {
        // update
        let current_input = debounce(
            &mut last_input,
            &[
                (KeyCode::Right, Control(Go(Right))),
                (KeyCode::Left, Control(Go(Left))),
                (KeyCode::Up, Control(Go(Up))),
                (KeyCode::Down, Control(Go(Down))),
                (KeyCode::Space, Control(Wait)),
                (KeyCode::Z, Control(Undo)),
                (KeyCode::Escape, Pause),
                (KeyCode::Enter, Enter),
            ],
            |i, t| match i {
                Control(Undo) => t > 0.075,
                Pause => false,
                _ => t > 0.15,
            },
        );
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
                    let (next, win) = step(&current_state, i, history.len() as u32);
                    if win && win_time.is_none() {
                        win_time = Some(get_time());
                    }
                    history.push(next);
                },
                Some(Pause) => return LevelResult::Exit,
                // Some(Pause) => paused = !paused,
                Some(Enter) => {
                    for y in 0..current_state.len() {
                        for x in 0..current_state[0].len() {
                            if !current_state[y][x].iter().any(|e| match e {
                                Entity::Noun(_, Cursor) => true,
                                _ => false,
                            }) {
                                continue;
                            }
                            for e in current_state[y][x].iter() {
                                if let Entity::Noun(_, Level(l)) = *e {
                                    return LevelResult::Enter(l);
                                }
                            }
                        }
                    }
                },
            };
            current_state = &history[history.len() - 1];
            if let Some(Control(i)) = current_input {
                views.push(current_state.clone());
                inputs.push(i);
            }
        }

        // render
        {
            clear_background(palette.get_pixel(1, 0));
            let bounds = render_level(
                &current_state,
                &palette,
                &sprites,
                &backgrounds,
                &color_overrides,
                Rect::new(0., 0., screen_width(), screen_height()),
                20.,
            );

            // draw pause menu
            if paused {
                draw_rectangle(bounds.x, bounds.y, bounds.w, bounds.h, pause_color);
            }

            // draw congratulations when you win
            match win_time {
                None => (),
                Some(anim_start) => {
                    if (get_time() - anim_start) > anim_time + 0.5 {
                        return LevelResult::Win((views, inputs, palette_name.to_string()));
                    }
                    gl_use_material(*MASK_MATERIAL);
                    MASK_MATERIAL.set_uniform(
                        "radius",
                        (((get_time() - anim_start) / anim_time) as f32).min(0.5_f32.sqrt()),
                    );
                    let scale = (bounds.w * 0.65) / congrats.width();
                    draw_texture_ex(
                        congrats,
                        bounds.x + (bounds.w - congrats.width() * scale) / 2.,
                        bounds.y + (bounds.h - congrats.height() * scale) / 2.,
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

fn render_level(
    level: &Level,
    palette: &Image,
    sprites: &SpriteMap,
    backgrounds: &[String],
    color_overrides: &HashMap<Noun, (u32, u32)>,
    bounds: Rect,
    min_border: f32,
) -> Rect {
    let width = level[0].len();
    let height = level.len();
    let sq_size = ((bounds.w - min_border) / width as f32).min((bounds.h - min_border) / height as f32);
    let game_width = sq_size * width as f32;
    let game_height = sq_size * height as f32;
    let offset_x = bounds.x + (bounds.w - game_width) / 2.;
    let offset_y = bounds.y + (bounds.h - game_height) / 2.;

    let rules = scan_rules(level);

    let active_texts = rules
        .iter()
        .flat_map(|x| x.0.clone())
        .collect::<HashSet<Index>>();

    let overridden_texts = {
        let (_, overridden) = partition_overridden_rules(rules);
        overridden.into_iter()
                  .flat_map(|x| x.0)
                  .collect::<HashSet<Index>>()
    };

    let anim_frame = (get_time() / 0.15).trunc() as usize % 3;

    let draw_sprite = |x, y, sq, sprite, c: Color| {
        SPRITES_MATERIAL.set_uniform("color", [c.r, c.g, c.b]);
        draw_texture_ex(
            sprite, x, y, WHITE, DrawTextureParams {
                dest_size: Some(Vec2{x: sq, y: sq}),
                ..Default::default()
            },
        );
    };

    draw_rectangle(offset_x, offset_y, game_width, game_height, palette.get_pixel(0, 4));

    for b in backgrounds {
        draw_texture_ex(
            sprites.3[b][anim_frame], offset_x, offset_y, WHITE, DrawTextureParams {
                dest_size: Some(Vec2{x: game_width, y: game_height}),
                ..Default::default()
            },
        );
    }

    gl_use_material(*SPRITES_MATERIAL);
    for row in 0..height {
        for col in 0..width {
            let x = offset_x + sq_size * col as f32;
            let y = offset_y + sq_size * row as f32;
            for (i, e) in level[row][col].iter().enumerate() {
                let anim_frame = (row + col + i + anim_frame) % 3;
                let (cx, cy) = match e {
                    Entity::Noun(_, n) => color_overrides.get(n).copied(),
                    _ => None,
                }.unwrap_or_else(|| default_color(
                    *e, active_texts.contains(&(col, row, i)),
                ));
                let c = palette.get_pixel(cx, cy);
                if let Entity::Noun(_, Cursor) = e {
                    let x = x - sq_size * 0.18;
                    let y = y - sq_size * 0.18;
                    let sq_size = sq_size * 1.36;
                    draw_sprite(x, y, sq_size, sprites.0[&canon(&e)][anim_frame], c);
                    continue;
                }
                draw_sprite(x, y, sq_size, sprites.0[&canon(&e)][anim_frame], c);
                if let Entity::Noun(_, Level(l)) = e {
                    let (x, y, sq) = match l {
                        Extra(_) => (x, y, sq_size),
                        Parent => (x, y, sq_size),
                        _ => (
                            x + sq_size * 0.175,
                            y + sq_size * 0.175,
                            sq_size * 0.6,
                        ),
                    };
                    let l = match l {
                        SubWorld(_, i) => SubWorld(0, *i),
                        _ => *l,
                    };
                    draw_sprite(x, y, sq, sprites.1[&l], WHITE);
                }
                if overridden_texts.contains(&(col, row, i)) {
                    let c = palette.get_pixel(2, 1);
                    SPRITES_MATERIAL.set_uniform("color", [c.r, c.g, c.b]);
                    draw_line(
                        x,
                        y,
                        x + sq_size,
                        y + sq_size,
                        sq_size / 10.,
                        c,
                    );
                    draw_line(
                        x + sq_size,
                        y,
                        x,
                        y + sq_size,
                        sq_size / 10.,
                        c,
                    );
                }
            }
        }
    }
    gl_use_default_material();

    Rect::new(offset_x, offset_y, game_width, game_height)
}

type SpriteMapT<T> = (
    HashMap<Entity, [T; 3]>,
    HashMap<LevelName, T>,
    T,
    HashMap<String, [T; 3]>,
);
type SpriteMap = SpriteMapT<Texture2D>;

fn level_icon_names() -> Vec<String> {
    let mut r = vec![];
    for e in std::fs::read_dir("resources/original/Data/Worlds/baba/Sprites").unwrap() {
        let p = e.unwrap().path();
        let mut x = p.file_name().unwrap().to_str().unwrap().split("_");
        if let Some("icon") = x.next() {
            r.push(x.next().unwrap().to_string());
        }
    }
    r
}

pub fn load_sprite_map() -> SpriteMap {
    type CPUTexture = (u16, u16, Vec<u8>);

    fn memo<F: Fn() -> Result<image::DynamicImage>>(key: &str, f: F) -> Result<CPUTexture> {
        let cache = dirs::cache_dir().unwrap().join("baba_is_clone").join(key);

        if let Ok(mut b) = std::fs::read(&cache) {
            let l = b.len() - 1;
            let w = u16::from_be_bytes([b[l-3], b[l-2]]);
            let h = u16::from_be_bytes([b[l-1], b[l-0]]);
            b.truncate(b.len() - 4);
            return Ok((w, h, b));
        }

        let img = f()?;
        let w = img.width() as u16;
        let h = img.height() as u16;
        let mut b = img.to_rgba8().into_raw();
        {
            let w = u16::to_be_bytes(w);
            let h = u16::to_be_bytes(h);
            b.extend_from_slice(&w[..]);
            b.extend_from_slice(&h[..]);
            std::fs::create_dir_all(&cache.parent().unwrap())?;
            std::fs::write(&cache, &b)?;
            b.truncate(b.len() - 4);
        }
        Ok((w, h, b))
    }

    fn load_texture_sync(path: &str) -> Result<CPUTexture> {
        memo(path, || {
            let bytes = std::fs::read(path)?;
            Ok(image::load_from_memory_with_format(&bytes[..], ImageFormat::Png)?)
        })
    }

    fn load(e: Entity) -> (Entity, [CPUTexture; 3]) {
        let original_sprite_path = "resources/original/Data/Sprites";
        let orig = |s| Some(format!("{original_sprite_path}/{s}"));
        let filename = match match e {
            Entity::Noun(_, n) => match n {
                Wall => orig("wall_0".to_string()),
                Line(x) => orig(format!("line_{x}")),
                Level(_) => Some("resources/level_0".to_string()),
                _ => None,
            },
            Entity::Text(_, Text::Object(Line(_))) => orig("text_line_0".to_string()),
            Entity::Text(_, Text::Object(Level(_))) => orig("text_level_0".to_string()),
            _ => None,
        } {
            Some(f) => f,
            None => {
                let name = match e {
                    Entity::Noun(_, Bog) => "water".to_string(),
                    Entity::Noun(_, Lava) => "water".to_string(),
                    Entity::Noun(_, n) => format!("{:?}", n),
                    Entity::Text(_, t) => "text_".to_string() + &(match t {
                        Text::Adjective(a) => format!("{:?}", a),
                        Text::Object(Seastar) => "star".to_string(),
                        Text::Object(o) => format!("{:?}", o),
                        Text::Is => "is".to_string(),
                        Text::And => "and".to_string(),
                        Text::Not => "not".to_string(),
                        Text::Has => "has".to_string(),
                        Text::Text => "text".to_string(),
                    }),
                }.to_lowercase();
                let filename = format!(
                    "{original_sprite_path}/{name}_{}",
                    match match e { Entity::Noun(d, _) => d, Entity::Text(d, _) => d } {
                        Right => "0",
                        Up => "8",
                        Left => "16",
                        Down => "24",
                    }
                );
                if std::path::Path::new(&format!("{filename}_1.png")).exists() {
                    filename
                } else {
                    format!("{original_sprite_path}/{name}_0")
                }
            },
        };
        (
            e,
            [
                load_texture_sync(&format!("{filename}_1.png")).unwrap(),
                load_texture_sync(&format!("{filename}_2.png")).unwrap(),
                load_texture_sync(&format!("{filename}_3.png")).unwrap(),
            ],
        )
    }
    fn load_level_label(icons: &[String], l: LevelName) -> (LevelName, CPUTexture) {
        let original_sprite_path = "resources/original/Data/Sprites";
        let custom_sprite_path = "resources";
        (l, match l {
            Number(n) => memo(&format!("level_number_{n}"), || {
                use image::{ImageBuffer, imageops};
                let load = |n| image::open(
                    &if n == 0 {
                        format!("{original_sprite_path}/text_o_0_1.png")
                    } else {
                        format!("{original_sprite_path}/text_{n}_0_1.png")
                    }
                ).unwrap().into_rgba8();
                fn shrinkwrap(img: image::RgbaImage) -> image::RgbaImage {
                    let b = || img.enumerate_pixels().filter(|x| x.2[3] == 255);
                    let w = || b().map(|x| x.0);
                    let h = || b().map(|x| x.1);
                    let buffer = 1;
                    let left = w().min().unwrap() - buffer;
                    let right = w().max().unwrap() + buffer;
                    let top = h().min().unwrap();
                    let bottom = h().max().unwrap();
                    imageops::crop_imm(&img, left, top, right - left, bottom - top).to_image()
                }
                let a = load(n / 10);
                let b = load(n % 10);
                assert!(a.width() == b.width() && a.height() == b.height());
                let width = a.width();
                let height = a.width();
                let mut result = ImageBuffer::new(width, height);
                let mut overlay = |img: image::RgbaImage, x| imageops::overlay(
                    &mut result,
                    &imageops::resize(
                        &img, width / 2, height, imageops::Lanczos3,
                    ),
                    x, 0,
                );
                overlay(shrinkwrap(a), 0);
                overlay(shrinkwrap(b), (width / 2).into());
                Ok(image::DynamicImage::ImageRgba8(result))
            }).unwrap(),
            Extra(n) => load_texture_sync(
                &format!("{custom_sprite_path}/pip_{n}.png")
            ).unwrap(),
            Letter(l) => load_texture_sync(
                &format!("{original_sprite_path}/text_{}_0_1.png", l)
            ).unwrap(),
            SubWorld(_, icon) => load_texture_sync(
                &format!(
                    "resources/original/Data/Worlds/baba/Sprites/icon_{}_1.png",
                    icons[icon],
                )
            ).unwrap(),
            Parent => load_texture_sync(
                &format!("{custom_sprite_path}/up.png")
            ).unwrap(),
        })
    }

    fn load_background(img: &str) -> (String, [CPUTexture; 3]) {
        fn load(img: &str, n: u8) -> CPUTexture {
            load_texture_sync(
                &format!(
                    "resources/original/Data/Worlds/Baba/Images/{img}_{n}.png"
                )
            ).unwrap()
        }
        (img.to_string(), [load(img, 1), load(img, 2), load(img, 3)])
    }

    let icons = level_icon_names();

    let r: SpriteMapT<CPUTexture> = (
        all_entities().map(load).collect(),
        (0..14).map(move |x| Number(x))
            .chain(('a'..'f').map(move |x| Letter(x)))
            .chain((1..7).map(move |x| Extra(x)))
            .chain((0..icons.len()).map(|x| SubWorld(0, x)))
            .chain(iter::once(Parent))
            .map(|l| load_level_label(&icons, l))
            .collect(),
        load_texture_sync("resources/congratulations.png").unwrap(),
        ["island", "island_decor", "flower"]
            .into_iter().map(load_background).collect(),
    );

    fn l((w, h, b): CPUTexture) -> Texture2D {
        Texture2D::from_rgba8(w, h, &b)
    }

    (
        r.0.into_iter().map(|(k, [v0, v1, v2])| (k, [l(v0), l(v1), l(v2)])).collect(),
        r.1.into_iter().map(|(k, v)| (k, l(v))).collect(),
        l(r.2),
        r.3.into_iter().map(|(k, [v0, v1, v2])| (k, [l(v0), l(v1), l(v2)])).collect(),
    )
}

lazy_static! {

    static ref BLEND_ALPHA: Option<BlendState> = Some(
        BlendState::new(
            Equation::Add,
            BlendFactor::Value(BlendValue::SourceAlpha),
            BlendFactor::OneMinusValue(BlendValue::SourceAlpha),
        )
    );

    static ref SPRITES_MATERIAL: Material = load_material(
        DEFAULT_VERTEX_SHADER,
        SPRITE_FRAGMENT_SHADER,
        MaterialParams {
            uniforms: vec![("color".to_string(), UniformType::Float3)],
            pipeline_params: PipelineParams {
                color_blend: *BLEND_ALPHA,
                ..Default::default()
            },
            ..Default::default()
        },
    ).unwrap();

    static ref MASK_MATERIAL: Material = load_material(
        DEFAULT_VERTEX_SHADER,
        MASK_FRAGMENT_SHADER,
        MaterialParams {
            uniforms: vec![("radius".to_string(), UniformType::Float1)],
            pipeline_params: PipelineParams {
                color_blend: *BLEND_ALPHA,
                ..Default::default()
            },
            ..Default::default()
        },
    ).unwrap();

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
