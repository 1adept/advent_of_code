use std::collections::HashSet;

pub fn tasks() {
    use std::time::Instant;
    let input = super::load_input(24);

    let time = Instant::now();
    let part1 = solve(&input, |field, bounds| {
        find_path_breath(
            0,
            (
                &Pos::new(bounds.min.row - 1, bounds.min.col),
                &Pos::new(bounds.max.row + 1, bounds.max.col),
            ),
            &precalculated_storm(field, &bounds),
            bounds,
        )
    });
    println!(
        "Part1: {part1} in {:?}",
        Instant::now().duration_since(time)
    );

    let time = Instant::now();
    let part2 = solve(&input, |field, bounds| {
        let storms = precalculated_storm(field, &bounds);
        let start = {
            let mut start: Pos = bounds.min;
            start.row -= 1;
            start
        };
        let end = {
            let mut end: Pos = bounds.max;
            end.row += 1;
            end
        };

        // We already arrived at the and and only then we were told to go back
        // so it makes sense we can skip this step, right ;)
        // // let part1 = find_path_breath(0, (&start, &end), &storms, bounds);
        let forgot_snacks = find_path_breath(part1, (&end, &start), &storms, bounds);
        find_path_breath(forgot_snacks, (&start, &end), &storms, bounds)
    });
    println!(
        "Part2: {part2} in {:?}",
        Instant::now().duration_since(time)
    );
}

fn solve<S, R>(input: &str, solver: S) -> R
where
    S: Fn(Vec<Flake>, Bounds) -> R,
{
    let (field, bounds) = parse(input);
    solver(field, bounds)
}

fn find_path_breath(
    minute: u64,
    (start, end): (&Pos, &Pos),
    storms: &[Vec<Flake>],
    bounds: Bounds,
) -> u64 {
    use rayon::prelude::*;

    let mut states: HashSet<Pos> = HashSet::new();
    states.insert(*start);

    let mut minute = minute;
    loop {
        if states.iter().any(|pos| *pos == *end) {
            return 1 + minute;
        }

        let storm = &storms[(minute as usize + 1) % storms.len()];

        states = states
            .into_par_iter()
            .flat_map_iter(|pos| {
                pos.around()
                    .into_iter()
                    .filter(|p| !bounds.oob(p))
                    .filter(|p| !storm.iter().any(|flk| flk.pos == *p))
            })
            .collect();
        minute += 1;
    }
}

fn precalculated_storm(storm: Vec<Flake>, bounds: &Bounds) -> Vec<Vec<Flake>> {
    fn storm_move(flakes: &[Flake], bounds: &Bounds) -> Vec<Flake> {
        flakes
            .iter()
            .cloned()
            .map(|mut flk| {
                flk.mv(bounds);
                flk
            })
            .collect::<Vec<_>>()
    }

    let mut storms = {
        let first = storm_move(&storm, bounds);
        let second = storm_move(&first, bounds);
        vec![first, second]
    };
    let mut last = storms.last().unwrap();
    while *last != storm {
        storms.push(storm_move(last, bounds));
        last = storms.last().unwrap();
    }
    storms
}

fn parse(input: &str) -> (Vec<Flake>, Bounds) {
    let mut flakes = Vec::new();
    let mut min = (usize::MAX, usize::MAX);
    let mut max = (usize::MIN, usize::MIN);
    for (row, line) in input.lines().enumerate() {
        for (col, ch) in line.chars().enumerate() {
            let flake = match ch {
                '^' => Some(Dir::Up),
                'v' => Some(Dir::Down),
                '<' => Some(Dir::Left),
                '>' => Some(Dir::Right),
                '#' => {
                    min.0 = min.0.min(row);
                    min.1 = min.1.min(col);
                    max.0 = max.0.max(row);
                    max.1 = max.1.max(col);
                    None
                }
                _ => None,
            };
            if let Some(flake) = flake {
                flakes.push(Flake {
                    dir: flake,
                    pos: Pos { row, col },
                });
            }
        }
    }
    min = (min.0 + 1, min.1 + 1);
    max = (max.0 - 1, max.1 - 1);
    let bounds = Bounds {
        min: min.into(),
        max: max.into(),
    };
    let entry = Pos::new(min.0 - 1, min.1);
    let exit = Pos::new(max.0 + 1, max.1);
    assert!(!bounds.oob(&entry) && !bounds.oob(&exit));
    // panic!("");
    (flakes, bounds)
}

/// Bounds of the walkable area, everything not contained in `Bounds` is inaccessible
#[derive(Debug, Clone, Copy)]
pub struct Bounds {
    min: Pos,
    max: Pos,
}

impl Bounds {
    fn oob(&self, pos: &Pos) -> bool {
        if pos.row == self.min.row - 1 && pos.col == self.min.col
            || pos.row == self.max.row + 1 && pos.col == self.max.col
        {
            // START
            false
        } else {
            pos.row < self.min.row
                || pos.row > self.max.row
                || pos.col < self.min.col
                || pos.col > self.max.col
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Flake {
    pos: Pos,
    dir: Dir,
}

impl From<(usize, usize)> for Pos {
    fn from(value: (usize, usize)) -> Self {
        Pos {
            row: value.0,
            col: value.1,
        }
    }
}

impl Flake {
    fn mv(&mut self, bounds: &Bounds) {
        self.pos = match self.dir {
            Dir::Up => Pos::new(self.pos.row - 1, self.pos.col),
            Dir::Down => Pos::new(self.pos.row + 1, self.pos.col),
            Dir::Left => Pos::new(self.pos.row, self.pos.col - 1),
            Dir::Right => Pos::new(self.pos.row, self.pos.col + 1),
        };

        if bounds.oob(&self.pos) {
            match self.dir {
                Dir::Up => self.pos.row = bounds.max.row,
                Dir::Down => self.pos.row = bounds.min.row,
                Dir::Left => self.pos.col = bounds.max.col,
                Dir::Right => self.pos.col = bounds.min.col,
            }
        }
    }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct Pos {
    row: usize,
    col: usize,
}

impl Pos {
    fn new(row: usize, col: usize) -> Pos {
        Pos { row, col }
    }
    fn around(&self) -> Vec<Pos> {
        let mut around = vec![
            Pos::new(self.row + 1, self.col),
            *self,
            Pos::new(self.row, self.col + 1),
        ];
        if self.row > 0 {
            around.push(Pos::new(self.row.saturating_sub(1), self.col));
        }
        if self.col > 0 {
            around.push(Pos::new(self.row, self.col.saturating_sub(1)));
        }
        around
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Dir {
    Up,
    Down,
    Left,
    Right,
}

mod visuals {
    #![allow(unused)]
    use super::*;
    use crossterm::{
        cursor::{self, MoveRight, MoveTo, MoveUp},
        execute, queue,
        style::{Color, Print, SetForegroundColor},
        terminal,
    };

    pub fn draw(field: &[Flake], bounds: &Bounds, player: &Pos) {
        // queue!(
        //     std::io::stdout(),
        //     cursor::MoveUp(3 + bounds.max.row as u16),
        //     terminal::Clear(terminal::ClearType::FromCursorDown)
        // )
        // .unwrap();
        let mut str = String::with_capacity(
            (bounds.max.row + 1 - bounds.min.row - 1) * (bounds.max.col + 1 - bounds.min.col - 1),
        );
        for i in (bounds.min.row - 1)..=(bounds.max.row + 1) {
            for j in (bounds.min.col - 1)..=(bounds.max.col + 1) {
                let f = field.iter().find(|flk| flk.pos == Pos::new(i, j));
                if let Some(flk) = f {
                    let ch = match flk.dir {
                        Dir::Up => '^',
                        Dir::Down => 'v',
                        Dir::Left => '<',
                        Dir::Right => '>',
                    };
                    str.push(ch);
                } else if player.row == i && player.col == j {
                    str.push('@');
                } else if i == 0 || j == 0 || i == bounds.max.row + 1 || j == bounds.max.col + 1 {
                    str.push('#');
                } else {
                    str.push('.');
                }
            }
            str.push('\n');
        }
        println!("{str}");
        let pos = cursor::position().unwrap();
        // execute!(
        //     std::io::stdout(),
        //     MoveRight(player.col as u16),
        //     MoveUp((3 + bounds.max.row - player.row) as u16),
        //     SetForegroundColor(Color::Red),
        //     Print('@'),
        //     SetForegroundColor(Color::Reset),
        //     MoveTo(pos.0, pos.1),
        // )
        // .unwrap();
        std::thread::sleep(std::time::Duration::from_millis(500));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore = "Not actually a test :)"]
    fn test_day24_simple() {
        solve(EXAMPLE_SIMPLE, |field, bounds| {
            find_path_breath(
                0,
                (
                    &Pos::new(bounds.min.row - 1, bounds.min.col),
                    &Pos::new(bounds.max.row + 1, bounds.max.col),
                ),
                &precalculated_storm(field, &bounds),
                bounds,
            )
        });
    }

    #[test]
    fn test_day24_part1() {
        assert_eq!(
            18,
            solve(EXAMPLE_COMPLEX, |field, bounds| {
                find_path_breath(
                    0,
                    (
                        &Pos::new(bounds.min.row - 1, bounds.min.col),
                        &Pos::new(bounds.max.row + 1, bounds.max.col),
                    ),
                    &precalculated_storm(field, &bounds),
                    bounds,
                )
            })
        );
    }

    #[test]
    fn test_day24_part2() {
        assert_eq!(
            54,
            solve(EXAMPLE_COMPLEX, |field, bounds| {
                let storms = precalculated_storm(field, &bounds);
                let start = {
                    let mut start: Pos = bounds.min;
                    start.row -= 1;
                    start
                };
                let end = {
                    let mut end: Pos = bounds.max;
                    end.row += 1;
                    end
                };

                let first_pass = find_path_breath(0, (&start, &end), &storms, bounds);
                let forgot_snacks = find_path_breath(first_pass, (&end, &start), &storms, bounds);
                find_path_breath(forgot_snacks, (&start, &end), &storms, bounds)
            })
        );
    }

    const EXAMPLE_SIMPLE: &str = "\
#.#####
#.....#
#>....#
#.....#
#...v.#
#.....#
#####.#";

    const EXAMPLE_COMPLEX: &str = "\
#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#";
}
