use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    sync::Arc,
};

use rayon::prelude::{
    IndexedParallelIterator, IntoParallelIterator, IntoParallelRefIterator, ParallelIterator,
};

pub fn tasks() {
    let input = super::load_input(19);
    let part1 = solve(&input, parse, |blue| {
        par_optimize(blue, 24, |res| {
            res.iter()
                .map(|(i, c)| (*i as u32 + 1) * *c as u32)
                .sum::<u32>()
        })
    });
    println!("Part1: {part1}");

    let part2: u64 = solve(&input, parse, |blue| {
        par_optimize(&blue[..3], 32, |res| {
            res.iter().map(|(_, c)| *c as u64).product::<u64>()
        })
    });
    println!("Part2: {part2}");
}

fn solve<P, S, R>(input: &str, parser: P, solver: S) -> R
where
    P: Fn(&str) -> Vec<BluePrint>,
    S: Fn(&[BluePrint]) -> R,
{
    solver(&parser(input))
}

fn par_optimize<S, R>(blueprints: &[BluePrint], time: u8, strategy: S) -> R
where
    S: Fn(&[(usize, u16)]) -> R,
{
    strategy(
        &blueprints
            .par_iter()
            .enumerate()
            .map(|(i, b)| (i, optimize(b, time)))
            .collect::<Vec<_>>(),
    )
}

fn optimize(b: &BluePrint, time: u8) -> u16 {
    let max_need = {
        let mut need = b.max_need_of_each_robot();
        need.insert(Material::Geode, u16::MAX);
        Arc::new(need)
    };

    let mut states = HashSet::new();
    states.insert(State {
        rob: {
            let mut res = Resource::default();
            *res.get_mut(&Material::Ore) += 1;
            res
        },
        inv: Resource::default(),
    });

    for _minute in 0..time {
        states = states
            .into_par_iter()
            .flat_map_iter(|state| {
                let mut buy_options = b.buy_options(&state.inv);
                let state = state.tick();
                if let Some(_geode) = buy_options.iter().find(|m| *m == &Some(Material::Geode)) {
                    buy_options = vec![Some(Material::Geode)]; //Only consider Geode
                }
                buy_options
                    .into_iter()
                    .map(move |buy| (state.clone(), buy))
                    .filter(|(state, buy)| {
                        buy.map(|buy| state.rob.get(&buy) < *max_need.get(&buy).unwrap())
                            .unwrap_or(true)
                    })
                    .filter_map(|(state, buy)| {
                        if let Some(buy) = buy {
                            // Buy specified Robot
                            state.buy(b, &buy)
                        } else {
                            Some(state)
                        }
                    })
            })
            .collect();
    }

    states
        .iter()
        .map(|state| state.inv.get(&Material::Geode))
        .max()
        .unwrap_or(0)
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
struct State {
    inv: Resource,
    rob: Resource,
}

impl Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = String::with_capacity(10 * 4);
        for (material, _) in self.inv.iter() {
            str.push_str(&format!(
                "{material:?}: {} ({}) ",
                self.inv.get(&material),
                self.rob.get(&material),
            ));
        }
        write!(f, "{str}")
    }
}

impl State {
    /// Next state ith robot products
    fn tick(&self) -> State {
        State {
            inv: Resource {
                ore: self.inv.get(&Material::Ore) + self.rob.get(&Material::Ore),
                clay: self.inv.get(&Material::Clay) + self.rob.get(&Material::Clay),
                obsidian: self.inv.get(&Material::Obsidian) + self.rob.get(&Material::Obsidian),
                geode: self.inv.get(&Material::Geode) + self.rob.get(&Material::Geode),
            },
            rob: self.rob.clone(),
        }
    }

    fn buy(&self, blueprint: &BluePrint, material: &Material) -> Option<Self> {
        if !blueprint.can_buy(material, &self.inv) {
            None
        } else {
            let mut next = self.clone();
            blueprint
                .costs
                .get(material)
                .iter()
                .flat_map(|need| need.iter())
                .filter(|(_, c)| c > &0)
                .for_each(|(m, c)| {
                    *next.inv.get_mut(&m) -= c;
                });
            *next.rob.get_mut(material) += 1;
            Some(next)
        }
    }
}

#[derive(Debug)]
struct BluePrint {
    costs: HashMap<Material, Resource>,
}

impl BluePrint {
    fn can_buy(&self, mat: &Material, resources: &Resource) -> bool {
        self.costs.get(mat).iter().all(|needed| {
            needed
                .iter()
                .all(|(mat_need, mat_amount)| resources.get(&mat_need) >= mat_amount)
        })
    }
    fn buy_options(&self, resources: &Resource) -> Vec<Option<Material>> {
        self.costs
            .iter()
            .filter(|(_, costs)| {
                costs
                    .iter()
                    .zip(resources.iter())
                    .all(|(cost, have)| cost.1 <= have.1)
            })
            .map(|(m, _)| Some(*m))
            .chain([None])
            .collect()
    }

    fn max_need_of_each_robot(&self) -> HashMap<Material, u16> {
        self.costs.iter().fold(
            HashMap::with_capacity(4),
            |mut acc: HashMap<Material, u16>, next| {
                next.1.iter().for_each(|(m, c)| {
                    acc.entry(m).and_modify(|e| *e = (*e).max(c)).or_insert(c);
                });
                acc
            },
        )
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
struct Resource {
    ore: u16,
    clay: u16,
    obsidian: u16,
    geode: u16,
}

impl Resource {
    fn iter(&self) -> std::array::IntoIter<(Material, u16), 4> {
        [
            (Material::Ore, self.ore),
            (Material::Clay, self.clay),
            (Material::Obsidian, self.obsidian),
            (Material::Geode, self.geode),
        ]
        .into_iter()
    }

    fn get(&self, mat: &Material) -> u16 {
        match mat {
            Material::Ore => self.ore,
            Material::Clay => self.clay,
            Material::Obsidian => self.obsidian,
            Material::Geode => self.geode,
        }
    }
    fn get_mut(&mut self, mat: &Material) -> &mut u16 {
        match mat {
            Material::Ore => &mut self.ore,
            Material::Clay => &mut self.clay,
            Material::Obsidian => &mut self.obsidian,
            Material::Geode => &mut self.geode,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum Material {
    Ore,
    Clay,
    Obsidian,
    Geode,
}

impl From<&str> for Material {
    fn from(value: &str) -> Self {
        match value.to_lowercase().as_str() {
            "ore" => Self::Ore,
            "clay" => Self::Clay,
            "obsidian" => Self::Obsidian,
            "geode" => Self::Geode,
            _ => panic!("Unknown material {value}"),
        }
    }
}

fn parse(input: &str) -> Vec<BluePrint> {
    use nom::{
        bytes::complete::tag,
        character::{
            complete::alpha1,
            complete::{digit1, space1},
        },
        combinator::map,
        multi::separated_list1,
        sequence::{delimited, preceded, tuple},
        IResult,
    };
    fn parse_material(str: &str) -> IResult<&str, Material> {
        map(alpha1, Material::from)(str)
    }

    fn parse_cost_line(str: &str) -> IResult<&str, (Material, Resource)> {
        map(
            tuple((
                delimited(tag(" Each "), parse_material, tag(" robot costs ")),
                separated_list1(
                    tag(" and "),
                    tuple((digit1, preceded(space1, parse_material))),
                ),
            )),
            |(mat, costs)| {
                (mat, {
                    let mut res = Resource::default();
                    costs
                        .into_iter()
                        .for_each(|(c, m)| *res.get_mut(&m) = c.parse::<u16>().unwrap());
                    res
                })
            },
        )(str)
    }

    let mut blueprints = Vec::new();
    for line in input.lines() {
        let mut map = HashMap::new();
        for robot_cost in line
            .split(&['.', ':'][..])
            .skip(1)
            .take_while(|str| !str.is_empty())
        {
            if let Ok((_, (mat, costs))) = parse_cost_line(robot_cost) {
                map.insert(mat, costs);
            } else {
                panic!("Cannot parse {line}");
            }
        }
        let blue = BluePrint { costs: map };
        blueprints.push(blue);
    }
    blueprints
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_day19_part1() {
        let result = solve(TEST, parse, |blue| {
            par_optimize(blue, 24, |res| {
                res.iter()
                    .map(|(i, c)| (*i as u32 + 1) * *c as u32)
                    .sum::<u32>()
            })
        });
        assert_eq!(33, result);
    }
    #[test]
    #[ignore]
    fn test_day19_part2() {
        let result = solve(TEST, parse, |blue| {
            par_optimize(&blue[..3], 32, |res| {
                res.iter().map(|(_, c)| *c as u64).product::<u64>()
            })
        });
        assert_eq!(62, result);
    }

    const TEST: &str = "\
Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.";
}
