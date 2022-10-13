use anyhow::Result;
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
}
use Noun::*;

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

impl Entity {
    fn direction(&self) -> Direction {
        match self {
            Entity::Noun(d, _) => *d,
            Entity::Text(d, _) => *d,
        }
    }
}

fn default_color(e: Entity, palette: &Image, active: bool) -> Color {
    let text_prop = if active { "text_color_active" } else { "text_color" };
    let ixs = match e {
        Entity::Noun(_, n) => n.get_str("color"),
        Entity::Text(_, t) => match t {
            Text::Object(n) => n.get_str(text_prop),
            Text::Adjective(a) => a.get_str(text_prop),
            Text::Is => t.get_str(text_prop),
            Text::And => t.get_str(text_prop),
            Text::Has => t.get_str(text_prop),
            Text::Text => t.get_str(text_prop),
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
            .chain(iter::once(Entity::Text(d, Text::Has)))
            .chain(iter::once(Entity::Text(d, Text::Text)))
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
                                 let d = x.next().and_then(|s| Direction::from_str(s).ok()).unwrap_or(Right);
                                 Entity::Noun(d, Noun::from_str(n).expect(n))
                             })
                             .collect()
                        )
                    }
                }
             ))
             .collect();
    let map: Vec<&str> = s.lines().skip_while(|s| *s != "---").skip(1).collect();

    let max = map.iter().map(|l| l.chars().count()).max().unwrap_or_default() + right_pad;
    let level = map.split(|s| *s == "---")
        .map(|layer| layer.iter().map(
            |line| line.chars()
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
                    '⨶' => Some(Shut),
                    '⧜' => Some(Open),
                    '⚲' => Some(Float),
                    '_' => Some(Weak),
                    '*' => Some(Tele),
                    '↣' => Some(Pull),
                    _ => None
                }) {
                    (Some(FullCell(cell)), _) => if c.is_uppercase() {
                        if let Entity::Noun(_, n) = cell[cell.len() - 1] {
                            vec![Entity::Text(Right, Text::Object(n))]
                        } else {
                            vec![]
                        }
                    } else {
                        cell.clone()
                    }
                    (Some(Abbreviation(noun)), _) => if c.is_uppercase() {
                        vec![Entity::Text(Right, Text::Object(*noun))]
                    } else {
                        vec![Entity::Noun(Right, *noun)]
                    }
                    (_, Some(adj)) => vec![Entity::Text(Right, Text::Adjective(adj))],
                    _ => match c {
                        '=' => vec![Entity::Text(Right, Text::Is)],
                        '&' => vec![Entity::Text(Right, Text::And)],
                        '~' => vec![Entity::Text(Right, Text::Has)],
                        '@' => vec![Entity::Text(Right, Text::Text)],
                        _ => vec![],
                    }
                })
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

    (level, metas.get("palette").unwrap_or(&"default").to_string())
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

type Subject = TextOrNoun;
type Rule = (Subject, Predicate);

#[cfg(test)]
mod tests {
    use crate::game::*;
    #[test]
    fn scan_rules() {
        let is_a = IsAdjective;
        let is = |n| IsNoun(TextOrNoun::Noun(n));
        let n = |n| TextOrNoun::Noun(n);
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

                let mut n = 0;
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
        Subjects(ListState, Vec<(Vec<Index>, Subject)>),
        Predicates(IsOrHas, ListState, Vec<(Vec<Index>, Subject)>, Option<(Vec<Index>, Predicate)>),
    }
    use ScanState::*;
    use Text::{Is, And, Has, Object, Adjective};
    let zero = |mut v: Vec<(Vec<Index>, Subject)>| { v.clear(); Subjects(Incomplete(None), v) };
    let mut state = zero(vec![]);
    let mut i = 0;
    fn cat_ixs(head: Option<Index>, tail: Index) -> Vec<Index> {
        if let Some(ix) = head { vec![ix, tail] } else { vec![tail] }
    }
    while i < text.len() {
        let (ix, t) = text[i];
        i += 1;
        state = match state {
            Subjects(s, mut subjs) => match s {
                Incomplete(opt_ix) => match t {
                    Object(noun) => {
                        subjs.push((cat_ixs(opt_ix, ix), Subject::Noun(noun)));
                        Subjects(Complete, subjs)
                    },
                    Text::Text => {
                        subjs.push((cat_ixs(opt_ix, ix), Subject::Text));
                        Subjects(Complete, subjs)
                    },
                    _ => zero(subjs),
                },
                Complete => match t {
                    Is => Predicates(IsOrHas::Is, Incomplete(Some(ix)), subjs, None),
                    Has => Predicates(IsOrHas::Has, Incomplete(Some(ix)), subjs, None),
                    And => Subjects(Incomplete(Some(ix)), subjs),
                    _ => { i -= 1; zero(subjs) },
                },
            },
            Predicates(is_or_has, s, subjs, pred) => match s {
                Incomplete(opt_ix) => match (pred, t) {
                    (Some(_), Text::Is) =>
                        Predicates(IsOrHas::Is, Incomplete(Some(ix)), subjs, None),
                    (Some(_), Text::Has) =>
                        Predicates(IsOrHas::Has, Incomplete(Some(ix)), subjs, None),
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
                                rules.push((ixs, (*s, pred)));
                            }
                            Predicates(is_or_has, Complete, subjs, Some((cat_ixs(opt_ix, ix), pred)))
                        },
                        None => { i -= 1; zero(subjs) },
                    },
                },
                Complete => match t {
                    And => Predicates(is_or_has, Incomplete(Some(ix)), subjs, pred),
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

fn partition_overridden_rules<A>(rules: Vec<(A, Rule)>) -> (Vec<(A, Rule)>, Vec<(A, Rule)>) {
    // x is x
    let (mut high_priority, the_rest): (Vec<(A, Rule)>, _) =
        rules.into_iter().partition(|r| match r.1 {
            (x, IsNoun(y)) => x == y,
            _ => false,
        });
    let x_is_x = high_priority.iter().map(|(_, (x, _))| *x).collect::<HashSet<TextOrNoun>>();
    let (overridden, the_rest) = the_rest.into_iter().partition(|r| match r.1 {
        (x, IsNoun(_)) => x_is_x.contains(&x),
        _ => false,
    });

    high_priority.extend(the_rest);

    (high_priority, overridden)
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
    let rules = scan_rules_no_index(&level); // TODO: sus that it's mut

    let width = level[0].len();
    let height = level.len();

    fn clip(level: &Level, x: i16, y: i16) -> (usize, usize) {
        let width = level[0].len() as i16;
        let height = level.len() as i16;

        (0.max(x).min(width as i16 - 1) as usize,
         0.max(y).min(height as i16 - 1) as usize)
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
        [HashSet<Subject>; Adjective::COUNT],
        HashMap<Subject, Vec<TextOrNoun>>
    );

    fn cache_rules(rules: &Vec<Rule>) -> RulesCache {
        ({
            let mut is_adj = core::array::from_fn(|_| HashSet::new());
            is_adj[Push as usize].insert(Subject::Text);
            for (s, p) in rules {
                if let IsAdjective(adj) = p {
                    is_adj[*adj as usize].insert(*s);
                }
            }
            is_adj
        }, {
            let mut has = HashMap::new();
            for (s, p) in rules {
                if let HasNoun(t) = p {
                    has.entry(*s).or_insert(HashSet::new()).insert(*t);
                }
            }
            has.into_iter()
               .map(|(k, v)| {
                   let mut v = v.into_iter().collect::<Vec<_>>();
                   v.sort();
                   (k, v)
                })
               .collect()
        })
    }

    fn is(level: &Level, x: usize, y: usize, i: usize, rules: &RulesCache, quality: Adjective) -> bool {
        rules.0[quality as usize].contains(&match level[y][x][i] {
            Entity::Text(_, _) => Subject::Text,
            Entity::Noun(_, n) => Subject::Noun(n),
        })
    }

    fn has(level: &Level, x: usize, y: usize, i: usize, rules: &RulesCache) -> Vec<Entity> {
        let e = level[y][x][i];
        let d = e.direction();
        rules.1.get(&match level[y][x][i] {
            Entity::Text(_, _) => Subject::Text,
            Entity::Noun(_, n) => Subject::Noun(n),
        }).unwrap_or(&vec![]).iter()
          .map(|h| match h {
              TextOrNoun::Noun(n) => Entity::Noun(d, *n),
              TextOrNoun::Text => Entity::Text(d, match e {
                  Entity::Noun(_, n) => Text::Object(n),
                  Entity::Text(_, _) => Text::Text,
              }),
          })
          .collect()
    }

    // move things
    fn move_things(level: &mut Level, rules: &Vec<Rule>, movers: Vec<(usize, usize, usize, Direction, bool)>) {
        let rules_cache = cache_rules(&rules);
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
        let delta = |d| match d {
            Left  => (-1, 0),
            Up    => (0, -1),
            Right => (1, 0),
            Down  => (0, 1),
        };
        while let Some((x, y, i)) = queue.pop_front() {
            if tombstones.contains(&(x, y, i)) {
                continue;
            }

            // check for push
            {
                let a = &movements[&(x, y, i)];
                let (x_, y_) = {
                    let (dx, dy) = delta(a.dir);
                    clip(&level, x as i16 + dx, y as i16 + dy)
                };
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

            let (x_, y_) = {
                let (dx, dy) = delta(a.dir);
                clip(&level, x as i16 + dx, y as i16 + dy)
            };

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
                let (mut x, mut y) = (x, y);
                loop {
                    let (x_, y_) = {
                        let (dx, dy) = delta(dir);
                        clip(&level, x as i16 - dx, y as i16 - dy)
                    };
                    if x == x_ && y == y_ {
                        break;
                    }
                    let mut keep_pulling = false;
                    for i in 0..level[y_][x_].len() {
                        if is(x_, y_, i, Pull) {
                            if !movements.contains_key(&(x_, y_, i)) {
                                keep_pulling = true;
                                movements.insert((x_, y_, i), Arrow {
                                    dir: dir,
                                    status: Status::Resolved { moving: true },
                                    is_move: false,
                                });
                            }
                        }
                    }
                    if !keep_pulling {
                        break;
                    }
                    x = x_;
                    y = y_;
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
                        let inserts = if tombstones.contains(&(x, y, i)) {
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
                            let (dx, dy) = delta(d);
                            let (x, y) = clip(&level, x as i16 + dx, y as i16 + dy);
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
    {
        let changers =
            rules.iter()
                 .filter_map(|r| match r {
                     (from, IsNoun(to)) => Some((*from, *to)),
                     _ => None,
                 })
                 .collect::<HashMap<Subject, TextOrNoun>>();
        for x in 0..width {
            for y in 0..height {
                for i in 0..level[y][x].len() {
                    // TODO: handle eg Baba Is Wall And Door
                    let old = level[y][x][i];
                    let (d, subject) = match old {
                        Entity::Text(d, _) => (d, Subject::Text),
                        Entity::Noun(d, n) => (d, Subject::Noun(n)),
                    };
                    if let Some(new) = changers.get(&subject) {
                        match new {
                            TextOrNoun::Text => match old {
                                Entity::Text(_, _) => (), // Text Is Text, no-op
                                Entity::Noun(_, n) =>
                                    level[y][x][i] = Entity::Text(d, Text::Object(n)),
                            },
                            TextOrNoun::Noun(n) =>
                                level[y][x][i] = Entity::Noun(d, *n),
                        };
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
    let palette = load_image(&format!("resources/Data/Palettes/{palette_name}.png")).await.unwrap();
    let sprites = load_sprite_map().await;
    let render = |s, x, y, w, h| render_level(s, &palette, &sprites, Rect::new(x, y, w, h), 20.);
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
    let palette = load_image(&format!("resources/Data/Palettes/{palette_name}.png")).await.unwrap();
    let sprites = load_sprite_map().await;

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

pub async fn main(level: Option<&str>) -> Replay {
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

    let width = level[0].len();
    let height = level.len();

    let mut inputs: Vec<Input> = vec![];
    // views is different from history, below, in that the Undo action
    // pops from history and pushes onto views.
    let mut views: Vec<Level> = vec![level.clone()];

    let sprites = load_sprite_map().await;

    let congrats: Texture2D = load_texture("congratulations.png").await.unwrap();

    let mut history = vec![level.clone()];
    let mut current_state = &history[0];

    let palette = load_image(&format!("resources/Data/Palettes/{palette_name}.png")).await.unwrap();

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
    }
    use UIInput::*;

    let mut last_input: (f64, Option<KeyCode>) = (0., None);

    let sq_size = ((screen_width() - 20.) / width as f32).min((screen_height() - 20.) / height as f32);
    let game_width = sq_size * width as f32;
    let game_height = sq_size * height as f32;
    let offset_x = (screen_width() - game_width) / 2.;
    let offset_y = (screen_height() - game_height) / 2.;

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
                Some(Pause) => paused = !paused,
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
            render_level(
                &current_state,
                &palette,
                &sprites,
                Rect::new(0., 0., screen_width(), screen_height()),
                20.,
            );

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
                        return (views, inputs, palette_name.to_string());
                    }
                    gl_use_material(*MASK_MATERIAL);
                    MASK_MATERIAL.set_uniform(
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

fn render_level(level: &Level, palette: &Image, sprites: &SpriteMap, bounds: Rect, min_border: f32) -> Rect {
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

    let draw_sprite = |x, y, w, h, entity, active| {
        let sprite = sprites[&entity];
        let c = default_color(entity, &palette, active);
        SPRITES_MATERIAL.set_uniform("color", [c.r, c.g, c.b]);
        draw_texture_ex(
            sprite, x, y, WHITE, DrawTextureParams {
                dest_size: Some(Vec2{x: w, y: h}),
                ..Default::default()
            },
        );
    };

    draw_rectangle(offset_x, offset_y, game_width, game_height, palette.get_pixel(0, 4));

    gl_use_material(*SPRITES_MATERIAL);
    for row in 0..height {
        for col in 0..width {
            let x = offset_x + sq_size * col as f32;
            let y = offset_y + sq_size * row as f32;
            for (i, e) in level[row][col].iter().enumerate() {
                draw_sprite(
                    x,
                    y,
                    sq_size,
                    sq_size,
                    *e,
                    active_texts.contains(&(col, row, i)),
                );
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

    // (offset_x - bounds.x, offset_y - bounds.y)
    Rect::new(offset_x, offset_y, game_width, game_height)
}

type SpriteMap = HashMap<Entity, Texture2D>;

async fn load_sprite_map() -> SpriteMap {
    async fn load(e: Entity) -> (Entity, Texture2D) {
        let filename = &match match e {
            Entity::Noun(_, n) => match n {
                Wall => Some(n),
                _ => None,
            },
            _ => None,
        } {
            Some(n) => format!("resources/Data/Sprites/{:?}_0_1.png", n).to_lowercase(),
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
                        Text::Has => "has".to_string(),
                        Text::Text => "text".to_string(),
                    }),
                }.to_lowercase();
                let filename = format!(
                    "resources/Data/Sprites/{name}_{}_1.png",
                    match match e { Entity::Noun(d, _) => d, Entity::Text(d, _) => d } {
                        Right => "0",
                        Up => "8",
                        Left => "16",
                        Down => "24",
                    }
                );
                if std::path::Path::new(&filename).exists() {
                    filename
                } else {
                    format!("resources/Data/Sprites/{name}_0_1.png")
                }
            },
        };
        (e, load_texture(filename).await.unwrap())
    }
    futures::future::join_all(all_entities().map(load)).await.into_iter().collect()
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
