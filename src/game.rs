#[macro_use]
extern crate lazy_static;

use macros::*;

use anyhow::{anyhow, Result};
use itertools::{iproduct, Itertools};
use macroquad::prelude::*;
use miniquad::graphics::*;
use serde::{Serialize, Deserialize};
use strum::{EnumCount, EnumIter, EnumString, IntoEnumIterator, IntoStaticStr};

use std::{io::{Read, Write}, iter, collections::{HashMap, HashSet, VecDeque}, str::FromStr};

#[derive_the_basics]
enum SpriteVariant {
    Idle, // -1, no variants
    Face, // 0, depends on direction
    Fuse, // 1, depends on neighbors
    Walk, // 2, depends on motion and direction
    Look, // 3, depends on timestep and direction
    Tick, // 4, depends on timestep
    Diag, // n/a, depends on adjacent & diagonal neighbors
}
use SpriteVariant::*;

#[derive_the_basics]
struct Neighborhood {
    right: bool,
    above: bool,
    left: bool,
    below: bool,
}

#[derive_the_basics]
struct NeighborhoodDiag {
    right: bool,
    above: bool,
    left: bool,
    below: bool,

    above_right: bool,
    above_left: bool,
    below_right: bool,
    below_left: bool,
}

#[derive_the_basics]
enum SpriteVariantState {
    IdleState,
    FaceState(Direction),
    FuseState(Neighborhood),
    WalkState(u8, Direction),
    LookState(u8, Direction),
    TickState(u8),
    DiagState(NeighborhoodDiag),
}
use SpriteVariantState::*;

#[derive_the_basics]
#[derive(EnumIter, EnumString, IntoStaticStr, BabaProps)]
#[strum(serialize_all = "snake_case")]
#[props(u32, u32, u32, u32, u32, u32, SpriteVariant)]
pub enum Noun {
    #[props(0, 3, 4, 0, 4, 1, Walk)] Baba,
    #[props(2, 2, 2, 1, 2, 2, Walk)] Keke,
    #[props(1, 1, 1, 1, 0, 1, Diag)] Wall,
    #[props(2, 2, 2, 1, 2, 2, Idle)] Door,
    #[props(2, 4, 6, 1, 2, 4, Idle)] Key,
    #[props(2, 4, 6, 1, 2, 4, Idle)] Flag,
    #[props(6, 2, 6, 0, 6, 1, Idle)] Rock,
    #[props(0, 0, 1, 1, 0, 1, Idle)] Tile,
    #[props(5, 0, 5, 1, 5, 3, Fuse)] Grass,
    #[props(1, 3, 1, 2, 1, 3, Fuse)] Water,
    #[props(2, 1, 2, 0, 2, 1, Face)] Skull,
    #[props(2, 3, 2, 2, 2, 3, Fuse)] Lava,
    #[props(6, 3, 6, 0, 6, 1, Fuse)] Brick,
    #[props(3, 3, 3, 2, 3, 3, Idle)] Flower,
    #[props(1, 2, 1, 2, 1, 3, Fuse)] Ice,
    #[props(1, 4, 1, 3, 1, 4, Idle)] Jelly,
    #[props(2, 2, 2, 1, 2, 2, Face)] Crab,
    #[props(2, 3, 2, 2, 2, 3, Idle)] Seastar,
    #[props(5, 2, 5, 0, 5, 1, Idle)] Algae,
    #[props(4, 2, 4, 1, 4, 2, Idle)] Love,
    #[props(0, 1, 1, 1, 0, 1, Idle)] Pillar,
    #[props(1, 4, 1, 3, 1, 4, Tick)] Bubble,
    #[props(5, 1, 5, 0, 5, 1, Fuse)] Hedge,
    #[props(0, 1, 0, 1, 0, 2, Tick)] Cog,
    #[props(1, 1, 1, 1, 0, 1, Fuse)] Pipe,
    #[props(0, 1, 1, 1, 0, 1, Walk)] Robot,
    #[props(2, 4, 2, 3, 2, 4, Face)] Bolt,
    #[props(6, 2, 6, 1, 6, 2, Idle)] Reed,
    #[props(5, 1, 5, 1, 5, 3, Fuse)] Bog,
    #[props(6, 2, 6, 0, 6, 1, Idle)] Box,
    #[props(6, 1, 6, 0, 6, 2, Idle)] Stump,
    #[props(6, 1, 5, 1, 6, 1, Idle)] Husk,
    #[props(5, 2, 5, 1, 5, 2, Idle)] Tree,
    #[props(4, 2, 4, 1, 4, 2, Face)] Ghost,
    #[props(6, 1, 6, 0, 6, 1, Fuse)] Fence,
    #[props(6, 0, 2, 2, 2, 3, Idle)] Foliage,
    #[props(2, 4, 6, 1, 2, 4, Idle)] Leaf,
    #[props(6, 1, 6, 0, 6, 2, Idle)] Fungi,
    #[props(6, 1, 6, 0, 6, 1, Idle)] Fungus,
    #[props(4, 2, 2, 3, 2, 4, Idle)] Cursor,
    #[props(0, 1, 0, 1, 0, 2, Face)] Statue,
    #[props(2, 2, 2, 1, 2, 2, Idle)] Fruit,
    #[props(0, 1, 1, 1, 0, 1, Face)] Rocket,
    #[props(2, 4, 6, 1, 2, 4, Idle)] Star,
    #[props(5, 2, 5, 1, 5, 2, Idle)] Trees,
    #[props(6, 1, 6, 1, 6, 2, Idle)] Husks,
    #[props(0, 3, 0, 2, 0, 3, Fuse)] Line,
    #[props(0, 0, 4, 0, 4, 1, Idle)] Level(LevelName),
}
use Noun::*;

impl Noun {
    fn color(&self) -> (u32, u32) {
        let p = self.props();
        (p.0, p.1)
    }
    fn text_color(&self, active: bool) -> (u32, u32) {
        let p = self.props();
        if active { (p.4, p.5) } else { (p.2, p.3) }
    }
    fn sprite_variant(&self) -> SpriteVariant {
        self.props().6
    }
}

#[derive_the_basics]
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

#[derive_the_basics]
#[derive(EnumIter, EnumString, EnumCount, IntoStaticStr, BabaProps)]
#[strum(serialize_all = "snake_case")]
#[props(u32, u32, u32, u32)]
pub enum Adjective {
    #[props(4, 0, 4, 1)] You,
    #[props(5, 0, 5, 1)] Stop,
    #[props(6, 0, 6, 1)] Push,
    #[props(6, 1, 2, 4)] Win,
    #[props(1, 2, 1, 3)] Sink,
    #[props(2, 0, 2, 1)] Defeat,
    #[props(2, 2, 2, 3)] Hot,
    #[props(1, 2, 1, 3)] Melt,
    #[props(5, 1, 5, 3)] Move,
    #[props(2, 1, 2, 2)] Shut,
    #[props(6, 1, 2, 4)] Open,
    #[props(1, 2, 1, 4)] Float,
    #[props(1, 1, 1, 2)] Weak,
    #[props(1, 2, 1, 4)] Tele,
    #[props(6, 1, 6, 2)] Pull,
}
use Adjective::*;
impl Adjective {
    fn color(&self, active: bool) -> (u32, u32) {
        let p = self.props();
        if active { (p.2, p.3) } else { (p.0, p.1) }
    }
}

#[derive_the_basics]
#[derive(IntoStaticStr, BabaProps)]
#[props(u32, u32, u32, u32)]
#[strum(serialize_all = "snake_case")]
pub enum Text {
    #[props(0, 1, 0, 3)] Is,
    #[props(0, 1, 0, 3)] And,
    #[props(4, 0, 4, 1)] Text,
    #[props(0, 1, 0, 3)] Has,
    #[props(2, 1, 3, 2)] Not,
    #[props(0, 0, 0, 0)] Object(Noun),
    #[props(0, 0, 0, 0)] Adjective(Adjective),
}
impl Text {
    fn color(&self, active: bool) -> (u32, u32) {
        let p = self.props();
        if active { (p.2, p.3) } else { (p.0, p.1) }
    }
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

#[derive_the_basics]
#[derive(EnumString, EnumIter)]
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

#[derive_the_basics]
pub struct LiveEntity {
    dir: Direction,
    e: Entity,
}

#[derive_the_basics]
pub enum Entity {
    Noun(Noun),
    Text(Text),
}

impl Entity {
    fn to_text_or_noun(&self) -> TextOrNoun {
        match self {
            Entity::Noun(n) => TextOrNoun::Noun(*n),
            Entity::Text(_) => TextOrNoun::Text,
        }
    }
    fn default_color(&self, active: bool) -> (u32, u32) {
        match self {
            Entity::Noun(n) => n.color(),
            Entity::Text(t) => match t {
                Text::Object(n) => n.text_color(active),
                Text::Adjective(a) => a.color(active),
                _ => t.color(active),
            },
        }
    }
}

fn all_entities() -> impl Iterator<Item=Entity> {
    Noun::iter().map(Entity::Noun)
    .chain(iter::once(Entity::Text(Text::Is)))
    .chain(iter::once(Entity::Text(Text::Not)))
    .chain(iter::once(Entity::Text(Text::And)))
    .chain(iter::once(Entity::Text(Text::Has)))
    .chain(iter::once(Entity::Text(Text::Text)))
    .chain(Noun::iter().map(move |n| Entity::Text(Text::Object(n))))
    .chain(Adjective::iter().map(move |a| Entity::Text(Text::Adjective(a))))
}

type Cell = Vec<LiveEntity>;
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
                                     LiveEntity {
                                         dir: Right,
                                         e: Entity::Noun(Noun::Level(SubWorld(n, icon))),
                                    }
                                 } else {
                                     let d = x.next()
                                         .and_then(|s| Direction::from_str(s).ok())
                                         .unwrap_or(Right);
                                     LiveEntity {
                                         dir: d,
                                         e: Entity::Noun(Noun::from_str(n).expect(n)),
                                    }
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
        Line,
        Level(LevelName),
        Blank,
    }
    use Thing::*;
    use self::Text::*;

    fn to_cell(legend: &HashMap<String, LegendValue>, c: char) -> Vec<LiveEntity> {
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

                '.' => Line,

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
                if let Entity::Noun(n) = cell[cell.len() - 1].e {
                    vec![LiveEntity { dir: Right, e: Entity::Text(Object(n)) }]
                } else {
                    vec![]
                }
            } else {
                cell.clone()
            }
            (Some(Abbreviation(noun)), _) => if c.is_uppercase() {
                vec![LiveEntity { dir: Right, e: Entity::Text(Object(*noun)) }]
            } else {
                vec![LiveEntity { dir: Right, e: Entity::Noun(*noun) }]
            }
            (_, Adj(adj)) => vec![LiveEntity { dir: Right, e: Entity::Text(Adjective(adj)) }],
            (_, Txt(txt)) => vec![LiveEntity { dir: Right, e: Entity::Text(txt) }],
            (_, Line) => vec![LiveEntity { dir: Right, e: Entity::Noun(Noun::Line) }],
            (_, Level(x)) => vec![LiveEntity { dir: Right, e: Entity::Noun(Noun::Level(x)) }],
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
                .collect::<Vec<Vec<LiveEntity>>>()).collect::<Vec<Vec<Vec<LiveEntity>>>>())
        .fold(vec![], |level: Vec<Vec<Vec<LiveEntity>>>, layer: Vec<Vec<Vec<LiveEntity>>>|
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
                        .collect::<Vec<Vec<LiveEntity>>>()
                     )
                 .collect::<Vec<Vec<Vec<LiveEntity>>>>());

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
                            if let Entity::Noun(n) = c[0].e {
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

#[derive_the_basics]
enum Predicate {
    HasNoun(TextOrNoun),
    IsNoun(TextOrNoun),
    IsAdjective(Adjective),
}
use Predicate::*;

#[derive_the_basics]
enum TextOrNoun {
    Text,
    Noun(Noun),
}

#[derive_the_basics]
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
                               .map(|t| LiveEntity { dir: Right, e: Entity::Text(*t) })
                               .collect::<Vec<LiveEntity>>())
                     .map(|c| ((0, 0), c))
                     .collect::<Vec<((usize, usize), Vec<LiveEntity>)>>();
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
                     .filter_map(|(i, &e)| match e.e {
                         Entity::Text(t) => Some(((x, y, i), t)),
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

#[derive_the_basics]
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

    fn entities(level: &Level) -> impl Iterator<Item=((usize, usize, usize), LiveEntity)> + '_ {
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
        let t_or_n = level[y][x][i].e.to_text_or_noun();
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

    fn is_or_has(level: &Level, x: usize, y: usize, i: usize, rules: &[Vec<(Subject, TextOrNoun)>; 2]) -> Vec<LiveEntity> {
        let [yes, no] = array_map_ref(&rules,
            |v| v.iter()
                 .filter(|(s, _)| subject_match(level, x, y, i, s))
                 .map(|(_, e)| e)
                 .copied()
                 .collect::<HashSet<TextOrNoun>>());

        let mut result = yes.difference(&no).copied().collect::<Vec<TextOrNoun>>();
        result.sort();
        let e = level[y][x][i];
        result
          .iter()
          .map(|x| LiveEntity {
              dir: e.dir,
              e: match x {
                  TextOrNoun::Noun(n) => Entity::Noun(*n),
                  TextOrNoun::Text => Entity::Text(match e.e {
                      Entity::Noun(n) => Text::Object(n),
                      Entity::Text(_) => Text::Text,
                  }),
              }
          })
          .collect()
    }

    fn has(level: &Level, x: usize, y: usize, i: usize, rules: &RulesCache) -> Vec<LiveEntity> {
        is_or_has(level, x, y, i, &rules.1)
    }

    fn is_noun(level: &Level, x: usize, y: usize, i: usize, rules: &RulesCache) -> Vec<LiveEntity> {
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
                            level[y][x].push(LiveEntity { dir: d, e: e.e, });
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
                    if let Entity::Noun(Noun::Cursor) = level[col][row][i].e {
                        let (x, y) = delta(&level, d, row, col);
                        for j in 0..level[y][x].len() {
                            if let Entity::Noun(n) = level[y][x][j].e {
                                if match n { Line => true, Level(_) => true, _ => false } {
                                    level[col][row].remove(i);
                                    level[y][x].push(LiveEntity {
                                        dir: d,
                                        e: Entity::Noun(Noun::Cursor)
                                    });
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
                true => Some((x, y, i, e.dir, true)),
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
            let mut travelers: HashMap<(usize, usize), Vec<(usize, LiveEntity, (usize, usize))>>
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
    let render = |s, x, y, w, h| render_level(s, 0, &palette, &sprites, &[], &HashMap::new(), true, Rect::new(x, y, w, h), 20.);
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
            i as u32,
            &palette,
            &sprites,
            &[],
            &HashMap::new(),
            true,
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
    let mut last_input = (0., None);
    loop {
        let (level, ix) = if let Some(x) = stack.pop() { x } else { return; };
        let (result, input) = play_level(&sprites, last_input, &level.path, ix).await;
        last_input = input;
        match result {
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

async fn play_level<P>
    (sprites: &SpriteMap, mut last_input: (f64, Option<KeyCode>), level: P, cursor: Option<LevelName>)
    -> (LevelResult, (f64, Option<KeyCode>))
where
    P: AsRef<std::path::Path>
{
    let (mut level, palette_name, backgrounds, color_overrides) = parse_level(level);

    fn place_cursor(level: &mut Level, at: LevelName) -> bool {
        for y in 0..level.len() {
            for x in 0..level[0].len() {
                for i in 0..level[y][x].len() {
                    if let Entity::Noun(Level(l)) = level[y][x][i].e {
                        if l == at {
                            level[y][x].push(LiveEntity {
                                dir: Right,
                                e: Entity::Noun(Cursor)
                            });
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
    let mut seamless = true;

    #[derive_the_basics]
    enum UIInput {
        Control(Input),
        Pause,
        Enter,
        ToggleSeamless,
    }
    use UIInput::*;

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
                (KeyCode::W, ToggleSeamless),
            ],
            |i, t| match i {
                Control(Undo) => t > 0.075,
                Pause => false,
                ToggleSeamless => false,
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
                Some(Pause) => return (LevelResult::Exit, last_input),
                // Some(Pause) => paused = !paused,
                Some(Enter) => {
                    for y in 0..current_state.len() {
                        for x in 0..current_state[0].len() {
                            if !current_state[y][x].iter().any(|e| match e.e {
                                Entity::Noun(Cursor) => true,
                                _ => false,
                            }) {
                                continue;
                            }
                            for e in current_state[y][x].iter() {
                                if let Entity::Noun(Level(l)) = e.e {
                                    return (LevelResult::Enter(l), last_input);
                                }
                            }
                        }
                    }
                },
                Some(ToggleSeamless) => seamless = !seamless,
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
                history.len() as u32,
                &palette,
                &sprites,
                &backgrounds,
                &color_overrides,
                seamless,
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
                        return (LevelResult::Win((views, inputs, palette_name.to_string())), last_input);
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
    physics_step: u32,
    palette: &Image,
    sprites: &SpriteMap,
    backgrounds: &[String],
    color_overrides: &HashMap<Noun, (u32, u32)>,
    seamless: bool,
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
                let (cx, cy) = match e.e {
                    Entity::Noun(n) => color_overrides.get(&n).copied(),
                    _ => None,
                }.unwrap_or_else(|| e.e.default_color(
                    active_texts.contains(&(col, row, i)),
                ));
                let c = palette.get_pixel(cx, cy);
                if let Entity::Noun(Cursor) = e.e {
                    // draw it at the end so it's on top of everything
                    continue;
                }
                {
                    let s = match e.e {
                        Entity::Noun(Level(_)) => Entity::Noun(Level(LevelName::default())),
                        _ => e.e,
                    };
                    let fuse = |dy, dx| {
                        let x = col as i16 + dx;
                        let y = row as i16 + dy;
                        if x < 0 || x >= level[0].len() as i16 || y < 0 || y >= level.len() as i16 {
                            return true;
                        }
                        for n in &level[y as usize][x as usize] {
                            if n.e == s {
                                return true;
                            }
                            if let Entity::Noun(Level(_)) = n.e {
                                if s == Entity::Noun(Line) {
                                    return true;
                                }
                            }
                        }
                        false
                    };
                    let neighborhood = || (fuse(0, 1), fuse(-1, 0), fuse(0, -1), fuse(1, 0));
                    let sprite_state = match s {
                        Entity::Text(_) => IdleState,
                        Entity::Noun(n) => match n.sprite_variant() {
                            Idle => IdleState,
                            Face => FaceState(e.dir),
                            Fuse => {
                                let (right, above, left, below) = neighborhood();
                                FuseState(Neighborhood { right, above, left, below })
                            },
                            Walk => WalkState(0, e.dir),
                            Look => LookState(physics_step as u8 % 4, e.dir),
                            Tick => TickState(physics_step as u8 % 4),
                            Diag => {
                                let (right, above, left, below) = neighborhood();
                                DiagState(NeighborhoodDiag {
                                    right,
                                    above,
                                    left,
                                    below,

                                    above_right: seamless && above && right && fuse(-1, 1),
                                    above_left: seamless && above && left && fuse(-1, -1),
                                    below_right: seamless && below && right && fuse(1, 1),
                                    below_left: seamless && below && left && fuse(1, -1),
                                })
                            },
                        },
                    };
                    draw_sprite(
                        x, y, sq_size,
                        sprites.0[&(s, sprite_state)][anim_frame],
                        c,
                    );
                }
                if let Entity::Noun(Level(l)) = e.e {
                    let (x, y, sq) = match l {
                        Extra(_) => (x, y, sq_size),
                        Parent => (x, y, sq_size),
                        SubWorld(_, _) => (x, y, sq_size),
                        _ => (
                            x + sq_size * 0.175,
                            y + sq_size * 0.175,
                            sq_size * 0.6,
                        ),
                    };
                    let l = match l {
                        SubWorld(_, i) => SubWorld(0, i),
                        _ => l,
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
    for row in 0..height {
        for col in 0..width {
            for e in &level[row][col] {
                if let Entity::Noun(Cursor) = e.e {
                    let (cx, cy) = Cursor.color();
                    let c = palette.get_pixel(cx, cy);
                    let x = offset_x + sq_size * (col as f32 - 0.18);
                    let y = offset_y + sq_size * (row as f32 - 0.18);
                    let sq_size = sq_size * 1.36;
                    draw_sprite(x, y, sq_size, sprites.0[&(e.e, IdleState)][anim_frame], c);
                }
            }
        }
    }
    gl_use_default_material();

    Rect::new(offset_x, offset_y, game_width, game_height)
}

type SpriteMapT<T> = (
    HashMap<(Entity, SpriteVariantState), [T; 3]>,
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

fn load_sprite_map() -> SpriteMap {
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

    fn load(e: Entity, d: SpriteVariantState) -> ((Entity, SpriteVariantState), [CPUTexture; 3]) {
        let asset_dir = match (e, d) {
            (Entity::Noun(Level(_)), _) => "resources",
            (_, DiagState(n)) =>
                if n.above_right || n.above_left || n.below_right || n.below_left {
                    "resources"
                } else {
                    "resources/original/Data/Sprites"
                }
            _ => "resources/original/Data/Sprites",
        };
        let base_name = match e {
            Entity::Text(t) => "text_".to_string() + match t {
                Text::Adjective(a) => a.into(),
                Text::Object(o) => match o {
                    Level(_) => "level",
                    Seastar => "star",
                    _ => o.into(),
                },
                _ => t.into(),
            },
            Entity::Noun(n) => match n {
                Bog => "water",
                Lava => "water",
                _ => n.into(),
            }.to_string(),
        };
        fn face_ix(d: Direction) -> u8 {
            match d {
                Right => 0,
                Up => 8,
                Left => 16,
                Down => 24,
            }
        }
        let variant = match d {
            IdleState => 0,
            FaceState(d) => face_ix(d),
            FuseState(Neighborhood { left, above, right, below }) =>
                (below as u8) << 3 |
                (left  as u8) << 2 |
                (above as u8) << 1 |
                (right as u8),
            WalkState(n, d) => face_ix(d) + (n % 4),
            LookState(n, d) => face_ix(d) + (n as u8 % 4),
            TickState(n) => n as u8 % 4,
            DiagState(NeighborhoodDiag {
                left, above, right, below,
                below_left, below_right, above_left, above_right,
            }) =>
                (below_left  as u8) << 7 |
                (below_right as u8) << 6 |
                (above_left  as u8) << 5 |
                (above_right as u8) << 4 |

                (below as u8) << 3 |
                (left  as u8) << 2 |
                (above as u8) << 1 |
                (right as u8),
        };
        ((e, d), core::array::from_fn(|i| {
            load_texture_sync(
                &format!("{asset_dir}/{base_name}_{variant}_{}.png", i + 1)).unwrap()}))
    }

    fn load_level_label(icons: &[String], l: LevelName) -> (LevelName, CPUTexture) {
        use image::{ImageBuffer, imageops};
        let original_sprite_path = "resources/original/Data/Sprites";
        let custom_sprite_path = "resources";
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
        (l, match l {
            Number(n) => memo(&format!("level_number_{n}"), || {
                let load = |n| image::open(
                    &if n == 0 {
                        format!("{original_sprite_path}/text_o_0_1.png")
                    } else {
                        format!("{original_sprite_path}/text_{n}_0_1.png")
                    }
                ).unwrap().into_rgba8();
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

            Letter(l) => memo(&format!("level_letter_{l}"), || {
                let bytes = std::fs::read(format!(
                    "{original_sprite_path}/text_{l}_0_1.png",
                ))?;
                let img = image::load_from_memory_with_format(&bytes[..], ImageFormat::Png)?;
                Ok(image::DynamicImage::ImageRgba8(shrinkwrap(img.into_rgba8())))
            }).unwrap(),
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
        all_entities()
            .flat_map(|e| match match e { Entity::Text(_) => Idle, Entity::Noun(n) => n.sprite_variant() } {
                Idle => vec![IdleState],
                Face => Direction::iter().map(FaceState).collect::<Vec<_>>(),
                Fuse => [[true, false]; 4].iter()
                    .multi_cartesian_product()
                    .map(|n| {
                        if let &[r, a, l, b] = &n[..] {
                            FuseState(Neighborhood {
                                right: *r,
                                above: *a,
                                left: *l,
                                below: *b,
                            })
                        } else { panic!("logic error") }
                    })
                    .collect::<Vec<_>>(),
                Walk => iproduct!((0..4), Direction::iter())
                    .map(|(n, d)| WalkState(n, d))
                    .collect::<Vec<_>>(),
                Look => iproduct!((0..4), Direction::iter())
                    .map(|(n, d)| LookState(n, d))
                    .collect::<Vec<_>>(),
                Tick => (0..4).map(|t| TickState(t)).collect::<Vec<_>>(),
                Diag => [[true, false]; 8].iter()
                    .multi_cartesian_product()
                    .filter(|n| {
                        if let &[r, a, l, b, ar, al, br, bl] = &n[..] {
                            (!*ar || *a && *r) &&
                                (!*al || *a && *l) &&
                                (!*br || *b && *r) &&
                                (!*bl || *b && *l)
                        } else { panic!("logic error") }
                    })
                    .map(|n| {
                        if let &[r, a, l, b, ar, al, br, bl] = &n[..] {
                            DiagState(NeighborhoodDiag {
                                right: *r,
                                above: *a,
                                left: *l,
                                below: *b,
                                above_right: *ar,
                                above_left: *al,
                                below_right: *br,
                                below_left: *bl,
                            })
                        } else { panic!("logic error") }
                    })
                    .collect::<Vec<_>>(),
            }.into_iter().map(move |s| (e, s)))
            .map(|(e, s)| load(e, s))
            .collect(),
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
        let t = Texture2D::from_rgba8(w, h, &b);
        t.set_filter(FilterMode::Nearest);
        t
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
    vec4 color_ = vec4(color.x, color.y, color.z, 1.0) / vec4(1.0);
    gl_FragColor = color_ * texture2D(Texture, uv);
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

pub async fn record_golden(level: &str, output: &str) {
    let sprites = load_sprite_map();
    let (result, _) = play_level(&sprites, (0., None), level, None).await;
    if let LevelResult::Win(history) = result {
        save(&history, &format!("goldens/{output}.ron.br")).unwrap();
    }
}
