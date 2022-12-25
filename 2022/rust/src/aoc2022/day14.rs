#![allow(unused)]

use std::{
    fmt::Display,
    io::{stdout, Write},
    ops::{Add, Sub},
    thread::panicking,
    time::Duration,
};

use crossterm::{
    cursor, execute, queue,
    style::{Color, Print, ResetColor, SetForegroundColor},
    terminal::Clear,
};

use super::load_input;

pub fn tasks() {
    let input = &load_input(14);
    // println!("{input}");
    // let grid = &mut create_rock_grid(&input);
    // println!("{grid}");

    // let sand = count_when_sand_overflows(grid);
    // println!("Sand until overflow = {sand}");

    let grid_floor = &mut create_rock_grid(input);
    let sand = count_when_sand_plugged(grid_floor);
    println!("Sand until overflow with floor = {sand}");
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Coord {
    row: i32,
    col: i32,
}

impl Coord {
    fn parse(input: (&str, &str)) -> Self {
        Self {
            row: input.1.parse::<i32>().unwrap(),
            col: input.0.parse::<i32>().unwrap(),
        }
    }

    fn normalized(self) -> Self {
        Coord {
            row: self.row.clamp(-1, 1),
            col: self.col.clamp(-1, 1),
        }
    }
}

impl Add for Coord {
    type Output = Coord;

    fn add(self, rhs: Self) -> Self::Output {
        Coord {
            row: self.row + rhs.row,
            col: self.col + rhs.col,
        }
    }
}

impl Sub for Coord {
    type Output = Coord;

    fn sub(self, rhs: Self) -> Self::Output {
        Coord {
            row: self.row - rhs.row,
            col: self.col - rhs.col,
        }
    }
}

impl Display for Coord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.row, self.col)
    }
}

#[derive(Debug)]
struct Rock {
    points: Vec<Coord>,
}

impl Rock {
    fn parse(input: &str) -> Self {
        Rock {
            points: input
                .split("->")
                .map(|raw_coord| raw_coord.trim().split_once(',').map(Coord::parse).unwrap())
                .collect(),
        }
    }
}

impl Display for Rock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.points.iter().map(Coord::to_string).collect::<String>()
        )
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq)]
enum Cell {
    Empty,
    Rock,
    Sand,
}

struct Grid {
    min_x: usize,
    floor: usize,
    width: usize,
    height: usize,
    data: Vec<Cell>,
}

impl Grid {
    fn new(width: usize, height: usize, min_x: usize, floor: usize) -> Self {
        let data = (0..width * height)
            .into_iter()
            .map(|_| Cell::Empty)
            .collect::<Vec<_>>();
        Self {
            min_x,
            floor,
            width,
            height,
            data,
        }
    }

    fn with_rocks(&mut self, rocks: &[Rock]) {
        for rock in rocks {
            for x in 0..=rock.points.len() - 2 {
                let [a, b] = &rock.points[x..=(x + 1)] else { unreachable!() };
                let dir = (*b - *a).normalized();
                let mut current = *a;
                while (current != *b) {
                    self.set(current.row, current.col, Cell::Rock);
                    current = (current + dir);
                }
                self.set(current.row, current.col, Cell::Rock);
            }
        }
    }

    fn get(&self, row: i32, col: i32) -> Option<&Cell> {
        if let Some(index) = self.data_index(row, col) {
            self.data.get(index)
        } else {
            None
        }
    }

    fn set(&mut self, row: i32, col: i32, cell: Cell) -> bool {
        if let Some(index) = self.data_index(row, col) {
            let c = self.data.get_mut(index);
            if let Some(c) = c {
                *c = cell;
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn data_index(&self, row: i32, col: i32) -> Option<usize> {
        let col = (col as isize - self.min_x as isize);
        let i = self.width as isize * row as isize + col;
        if i >= 0 {
            Some(i as usize)
        } else {
            None
        }
    }

    fn get_down(&self, pos: Coord) -> [Coord; 3] {
        [
            pos + Coord { row: 1, col: 0 },
            pos + Coord { row: 1, col: -1 },
            pos + Coord { row: 1, col: 1 },
        ]
    }

    fn is_oob(&self, pos: &Coord) -> bool {
        pos.col < 0
            || pos.col >= (self.min_x + self.width) as i32
            || pos.row < 0
            || pos.row >= self.height as i32
    }

    fn down(&self, pos: &Coord) -> Option<Coord> {
        let down = self.get_down(*pos);

        for d in down {
            return match (self.is_oob(&d), self.get(d.row, d.col)) {
                (true, _) => Some(d),
                (false, Some(c)) => match c {
                    Cell::Empty => Some(d),
                    _ => continue,
                },
                _ => None,
            };
        }

        None
    }
}

impl Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = String::new();
        for row in 0..self.height {
            for col in self.min_x..(self.min_x + self.width) {
                let c = match self.get(row as i32, col as i32) {
                    Some(Cell::Empty) => ' ',
                    Some(Cell::Rock) => '#',
                    Some(Cell::Sand) => 'O',
                    _ => ' ',
                };
                str.push_str(&format!("{c}"));
            }
            str.push('\n');
        }
        write!(f, "{str}")
    }
}

fn parse_input(input: &str) -> Vec<Rock> {
    input.lines().map(Rock::parse).collect()
}

fn get_bounds(rocks: &[Rock]) -> ((i32, i32), (i32, i32)) {
    let min_col = rocks
        .iter()
        .flat_map(|rock| &rock.points)
        .min_by_key(|coord| coord.col)
        .unwrap()
        .col;
    let min_row = rocks
        .iter()
        .flat_map(|rock| &rock.points)
        .min_by_key(|coord| coord.row)
        .unwrap()
        .row;
    let max_col = rocks
        .iter()
        .flat_map(|rock| &rock.points)
        .max_by_key(|coord| coord.col)
        .unwrap()
        .col;
    let max_row = rocks
        .iter()
        .flat_map(|rock| &rock.points)
        .max_by_key(|coord| coord.row)
        .unwrap()
        .row;
    ((min_col, max_col), (min_row, max_row))
}

fn create_rock_grid(input: &str) -> Grid {
    let mut rocks = parse_input(input);

    let ((min_col, max_col), (min_row, max_row)) = get_bounds(&rocks);

    let h = max_row + 2;
    let w = h + 10 * (max_col - min_col);

    let mut grid = Grid::new(w as usize, h as usize, min_col as usize, (h - 1) as usize);
    grid.with_rocks(&rocks);

    grid
}

fn count_when_sand_overflows(grid: &mut Grid) -> usize {
    let mut count = 0;
    loop {
        let mut sand = Coord {
            row: 0,
            col: 500i32,
        };
        while let Some(next) = grid.down(&sand) {
            grid.set(sand.row, sand.col, Cell::Empty);
            sand = next;
            grid.set(sand.row, sand.col, Cell::Sand);

            if grid.is_oob(&sand) {
                return count;
            }
        }
        count += 1;
        if count >= 10_000 {
            panic!("I HATE SAND")
        }
    }

    count
}

fn count_when_sand_plugged(grid: &mut Grid) -> usize {
    let mut count = 0;
    let hole = Coord {
        row: 0,
        col: 500i32,
    };

    fn down_valid(grid: &Grid, down: &Coord, floor: usize) -> Option<Coord> {
        if down.row == floor as i32 {
            return None;
        }

        let downs = grid.get_down(*down);

        for d in downs {
            if let Some(Cell::Empty) = grid.get(d.row, d.col) {
                return Some(d);
            }
        }
        None
    }

    loop {
        let mut sand = hole;
        let mut touched_ground = false;
        while let Some(next) = down_valid(grid, &sand, grid.floor) {
            grid.set(sand.row, sand.col, Cell::Empty);
            sand = next;
            grid.set(sand.row, sand.col, Cell::Sand);

            if sand.row as usize == grid.floor {
                touched_ground = true;
                break;
            }
            if sand == hole {
                dbg!("HOLE!", sand);
                return count;
            }
        }
        count += 1;
        if sand == hole {
            dbg!("HOLE2!", sand);
            return count;
        }
        if touched_ground {
            touched_ground = false;
            continue;
        }
        if count >= 100_000 {
            panic!("I HATE SAND")
        }
    }

    count
}

fn crossterm_print(grid: &Grid) {
    let mut stdout = stdout();
    execute!(stdout, Clear(crossterm::terminal::ClearType::All));
    for x in 0..grid.width {
        for y in 0..grid.height {
            let c = match grid.get(x as i32, y as i32) {
                Some(Cell::Empty) => '.',
                Some(Cell::Rock) => '#',
                Some(Cell::Sand) => 'O',
                _ => '!',
            };
            execute!(
                stdout,
                cursor::MoveTo(y as u16, x as u16),
                SetForegroundColor(match c {
                    '#' => Color::Black,
                    'O' => Color::Yellow,
                    '.' => Color::White,
                    _ => unreachable!(),
                }),
                // SetBackgroundColor(Color::Black),
                Print(c),
                ResetColor,
            );
        }
    }
    stdout.flush();
}

#[cfg(test)]
mod tests {
    use std::{
        collections::HashMap,
        io::{stdin, stdout, Write},
    };

    use crossterm::{
        cursor, execute, queue,
        style::{
            Color::{self, *},
            Print, ResetColor, SetAttribute, SetBackgroundColor, SetForegroundColor, Stylize,
        },
        terminal::Clear,
    };

    use super::*;

    const TEST: &str = "\
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9";

    #[test]
    fn test_rocks_spawn() {
        let grid = create_rock_grid(TEST);

        let rock_positions = [
            Coord {
                row: 4,
                col: 494 + 4,
            },
            Coord {
                row: 4,
                col: 494 + 8,
            },
            Coord {
                row: 4,
                col: 494 + 9,
            },
            Coord {
                row: 5,
                col: 494 + 4,
            },
            Coord {
                row: 5,
                col: 494 + 8,
            },
            Coord {
                row: 6,
                col: 494 + 2,
            },
            Coord {
                row: 6,
                col: 494 + 3,
            },
            Coord {
                row: 6,
                col: 494 + 4,
            },
            Coord {
                row: 6,
                col: 494 + 8,
            },
            Coord {
                row: 7,
                col: 494 + 8,
            },
            Coord {
                row: 8,
                col: 494 + 8,
            },
            Coord { row: 9, col: 494 },
            Coord {
                row: 9,
                col: 494 + 1,
            },
            Coord {
                row: 9,
                col: 494 + 2,
            },
            Coord {
                row: 9,
                col: 494 + 3,
            },
            Coord {
                row: 9,
                col: 494 + 4,
            },
            Coord {
                row: 9,
                col: 494 + 5,
            },
            Coord {
                row: 9,
                col: 494 + 6,
            },
            Coord {
                row: 9,
                col: 494 + 7,
            },
            Coord {
                row: 9,
                col: 494 + 8,
            },
        ];

        let rocks_20 = rock_positions
            .into_iter()
            .map(|c| grid.get(c.row, c.col))
            .collect::<Vec<_>>();
        assert!(rocks_20.iter().all(|&r| r == Some(&Cell::Rock)));
    }

    #[test]
    fn test_day14_part1() {
        let grid = &mut create_rock_grid(TEST);

        let sand = count_when_sand_overflows(grid);
        assert_eq!(24, sand);
    }

    #[test]
    fn test_day14_part2() {
        let grid = &mut create_rock_grid(TEST);

        let sand = count_when_sand_plugged(grid);
        assert_eq!(93, sand);
    }
}
