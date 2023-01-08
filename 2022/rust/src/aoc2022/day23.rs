use std::collections::BTreeMap;

pub fn tasks() {
    let input = super::load_input(23);
    let mut grid = parse(&input);

    let (part1, part2) = solve(
        &mut grid,
        |round, grid| {
            if round == &10 {
                Some(empty_spaces_in_min_rectangle(grid))
            } else {
                None
            }
        },
        |round, moves| if moves.is_empty() { Some(*round) } else { None },
        false,
    );
    println!("Part1: {part1}");
    println!("Part1: {part2}");
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos {
    row: isize,
    col: isize,
}

impl Pos {
    fn uniform(value: isize) -> Self {
        Self {
            row: value,
            col: value,
        }
    }
    fn go(&self, dir: &Dir) -> Self {
        match dir {
            Dir::N => Pos {
                row: self.row - 1,
                ..*self
            },
            Dir::S => Pos {
                row: self.row + 1,
                ..*self
            },
            Dir::W => Pos {
                col: self.col - 1,
                ..*self
            },
            Dir::E => Pos {
                col: self.col + 1,
                ..*self
            },
        }
    }
    fn around(&self) -> [Pos; 8] {
        [
            Pos {
                row: self.row - 1,
                col: self.col - 1,
            },
            Pos {
                row: self.row - 1,
                col: self.col,
            },
            Pos {
                row: self.row - 1,
                col: self.col + 1,
            },
            Pos {
                col: self.col - 1,
                ..*self
            },
            Pos {
                col: self.col + 1,
                ..*self
            },
            Pos {
                row: self.row + 1,
                col: self.col - 1,
            },
            Pos {
                row: self.row + 1,
                col: self.col,
            },
            Pos {
                row: self.row + 1,
                col: self.col + 1,
            },
        ]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Dir {
    N,
    S,
    W,
    E,
}

impl Dir {
    fn orthogonal(&self) -> [Dir; 2] {
        match self {
            Dir::N | Dir::S => [Dir::E, Dir::W],
            Dir::W | Dir::E => [Dir::N, Dir::S],
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Elf {
    consideration: [Dir; 4],
}
impl Elf {
    fn new() -> Self {
        Self {
            consideration: [Dir::N, Dir::S, Dir::W, Dir::E],
        }
    }

    fn move_first_consideration_back(&mut self) {
        let first = self.consideration[0];
        for i in 0..3 {
            self.consideration[i] = self.consideration[i + 1];
        }
        self.consideration[3] = first;
    }
}

fn solve<PeekGrid, PeekMoves>(
    grid: &mut Grid,
    peek_grid: PeekGrid,
    peek_moves: PeekMoves,
    draw: bool,
) -> (u32, u32)
where
    PeekGrid: Fn(&u32, &Grid) -> Option<u32>,
    PeekMoves: Fn(&u32, &Moves) -> Option<u32>,
{
    if draw {
        visualisation::draw(grid);
    }

    let mut result1 = None;
    let mut result2 = None;

    let mut round = 0;
    loop {
        round += 1;
        let elf_propositions: Vec<_> = grid
            .iter()
            .map(|e| {
                if e.0.around().iter().all(|p| grid.get(p).is_none()) {
                    (*e.0, None)
                } else {
                    let consider = e.1.consideration.iter().copied().find(|consider| {
                        let ortho = consider.orthogonal();
                        let none_or_empty = |p: Pos| -> bool { grid.get(&p).is_none() };
                        let go_consider = e.0.go(consider);
                        [go_consider]
                            .into_iter()
                            .chain(ortho.map(|c| go_consider.go(&c)))
                            .all(none_or_empty) // if any of the 3 cardinal dirs is empty accept that proposal
                    });
                    (*e.0, consider)
                }
            })
            .collect();

        let elf_moves = {
            let elfs_with_proposal: Vec<_> = elf_propositions
                .iter()
                .filter(|(_, dir)| dir.is_some())
                .map(|(e, d)| (e, e.go(&d.unwrap())))
                .collect();

            let mut unique_moves = Moves::new();
            for (pos, goto) in &elfs_with_proposal {
                if elfs_with_proposal
                    .iter()
                    .filter(|(pos2, _)| pos2 != pos)
                    .all(|(_, goto2)| goto != goto2)
                {
                    unique_moves.insert(**pos, *goto);
                }
            }
            unique_moves
        };

        if result2.is_none() {
            if let Some(result) = peek_moves(&round, &elf_moves) {
                result2 = Some(result);
            }
        }

        // Move elfes to new position
        for (from, to) in &elf_moves {
            let elf = grid.remove(from).unwrap();
            grid.insert(*to, elf);
        }

        grid.values_mut()
            .for_each(|elf| elf.move_first_consideration_back());

        if result1.is_none() {
            if let Some(result) = peek_grid(&round, grid) {
                result1 = Some(result);
            }
        }

        if result1.is_some() && result2.is_some() {
            break;
        }

        if draw {
            visualisation::draw(grid);
        }
    }
    (result1.unwrap(), result2.unwrap())
}

fn empty_spaces_in_min_rectangle(grid: &Grid) -> u32 {
    let (min, max) = smallest_rectangle(grid);

    let mut empty_count = 0;
    for row in min.row..=max.row {
        for col in min.col..=max.col {
            if grid.get(&Pos { row, col }).is_none() {
                empty_count += 1;
            }
        }
    }
    empty_count
}

fn smallest_rectangle(grid: &BTreeMap<Pos, Elf>) -> (Pos, Pos) {
    fn re_eval<F>(pos: &mut Pos, other: &Pos, comp: F)
    where
        F: Fn(isize, isize) -> isize,
    {
        pos.row = comp(pos.row, other.row);
        pos.col = comp(pos.col, other.col);
    }
    let mut min = Pos::uniform(isize::MAX);
    let mut max = Pos::uniform(isize::MIN);
    for pos in grid.keys() {
        re_eval(&mut min, pos, isize::min);
        re_eval(&mut max, pos, isize::max);
    }
    (min, max)
}

type Grid = BTreeMap<Pos, Elf>;
type Moves = BTreeMap<Pos, Pos>;

fn parse(input: &str) -> Grid {
    input
        .lines()
        .enumerate()
        .flat_map(|(row, line)| {
            line.chars()
                .enumerate()
                .filter(|(_, c)| *c == '#')
                .map(|(col, _)| {
                    (
                        Pos {
                            row: row as isize,
                            col: col as isize,
                        },
                        Elf::new(),
                    )
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

mod visualisation {
    use std::fmt::Display;

    use super::smallest_rectangle;
    use super::Grid;
    use super::Pos;

    pub fn draw(grid: &Grid) {
        let (min, max) = smallest_rectangle(grid);

        let col_len = 1 + (max.col - min.col) as usize;
        let mut out = String::with_capacity(1 + (max.row - min.row) as usize * col_len);
        for row in min.row..=max.row {
            let mut row_str = String::with_capacity(col_len);
            for col in min.col..=max.col {
                if grid.get(&Pos { row, col }).is_some() {
                    row_str.push('#');
                } else {
                    row_str.push('.');
                }
            }
            out.push_str(&format!("\n{}", row_str));
        }

        println!("{out}\n{}", "-".repeat(col_len));
    }

    impl Display for Pos {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let Pos { row, col } = self;
            write!(f, "Pos({row}, {col})")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_day23_small() {
        let mut grid = parse(SMALL_EXAMPLE);

        let _solution = solve(
            &mut grid,
            |round, grid| {
                if round == &5 {
                    Some(empty_spaces_in_min_rectangle(grid))
                } else {
                    None
                }
            },
            |_, _| Some(1),
            false,
        );
    }

    #[test]
    fn test_day23_part1() {
        let mut grid = parse(TEST);

        let (solution, _) = solve(
            &mut grid,
            |round, grid| {
                if round == &10 {
                    Some(empty_spaces_in_min_rectangle(grid))
                } else {
                    None
                }
            },
            |_, _| Some(0),
            false,
        );
        assert_eq!(110, solution);
    }

    #[test]
    fn test_day23_part2() {
        let mut grid = parse(TEST);

        let (_, solution) = solve(
            &mut grid,
            |_, _| Some(0),
            |round, moves| if moves.is_empty() { Some(*round) } else { None },
            false,
        );
        assert_eq!(20, solution);
    }

    const SMALL_EXAMPLE: &str = ".....
..##.
..#..
.....
..##.
.....";

    const TEST: &str = "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..";
}
