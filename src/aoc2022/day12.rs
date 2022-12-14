use pathfinding::prelude::astar;

use super::load_input;

pub fn tasks() {
    let input = load_input(12);
    let grid = read_grid::<155, 41>(&input);
    let gear = ClimbingGear { ascend: 1 };

    let (start, end) = grid.start_end();

    let path = search(&grid, &start, &end, &gear).unwrap();

    println!("You need {} steps from start", path.1);

    let mut paths_from_a = Vec::new();
    for (r, row) in grid.grid.iter().enumerate() {
        for (c, col) in row.iter().enumerate() {
            match col {
                'S' | 'a' => {
                    let pos = GridPos::new(r, c);
                    let path = search(&grid, &pos, &end, &gear);
                    if let Some(path) = path {
                        paths_from_a.push(path.1);
                    } else {
                        // No path
                    }
                }
                _ => (),
            }
        }
    }

    let min = paths_from_a.iter().min().unwrap();
    println!("You need {min} steps for the shortest path");
}

fn read_grid<const W: usize, const H: usize>(input: &str) -> Grid<W, H> {
    let mut grid = [['_'; W]; H];
    for (r, row) in input.lines().enumerate() {
        for (c, col) in row.chars().enumerate() {
            grid[r][c] = col;
        }
    }
    Grid { grid }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct GridPos {
    row: usize,
    col: usize,
}

impl GridPos {
    fn new(row: usize, col: usize) -> Self {
        Self { row, col }
    }

    fn distance(&self, other: &GridPos) -> usize {
        // (self.row - other.row) + (self.col - other.col)
        self.row.abs_diff(other.row) + self.col.abs_diff(other.col)
    }
}

#[derive(Debug)]
struct Grid<const W: usize, const H: usize> {
    grid: [[char; W]; H],
}

impl<const W: usize, const H: usize> Grid<W, H> {
    fn get(&self, row: usize, col: usize) -> &char {
        &self.grid[row][col]
    }

    fn get_neighbors_of(&self, pos: &GridPos) -> [Option<GridPos>; 4] {
        let h = self.grid.len();
        let w = self.grid[0].len();
        let GridPos { row, col } = pos;

        // println!("Neighbors of {pos:?} are {neighbors:?}");
        [
            if pos.row < h - 1 {
                Some(GridPos::new(row + 1, *col))
            } else {
                None
            },
            if pos.row != 0 {
                Some(GridPos::new(row - 1, *col))
            } else {
                None
            },
            if pos.col != 0 {
                Some(GridPos::new(*row, col - 1))
            } else {
                None
            },
            if pos.col < w - 1 {
                Some(GridPos::new(*row, col + 1))
            } else {
                None
            },
        ]
    }

    fn start_end(&self) -> (GridPos, GridPos) {
        let mut start = None;
        let mut end = None;
        for (r, row) in self.grid.iter().enumerate() {
            for (c, col) in row.iter().enumerate() {
                match col {
                    'S' => start = Some(GridPos { row: r, col: c }),
                    'E' => end = Some(GridPos { row: r, col: c }),
                    _ => (),
                }
                if let (Some(s), Some(e)) = (&start, &end) {
                    return (s.to_owned(), e.to_owned());
                }
            }
        }
        panic!("Couldnt find start and end")
    }
}

struct ClimbingGear {
    ascend: u8,
}

fn can_climb<const W: usize, const H: usize>(
    grid: &Grid<W, H>,
    from: &GridPos,
    to: &GridPos,
    gear: &ClimbingGear,
) -> bool {
    let from = match *grid.get(from.row, from.col) {
        'S' => 'a',
        c => c,
    };
    let to = match *grid.get(to.row, to.col) {
        'E' => 'z',
        c => c,
    };

    let from = from as u8;
    let to = to as u8;

    if from > to {
        true
    } else {
        from + gear.ascend >= to
    }
}

fn search<const W: usize, const H: usize>(
    grid: &Grid<W, H>,
    start: &GridPos,
    end: &GridPos,
    gear: &ClimbingGear,
    // filter: Option<F>,
) -> Option<(Vec<GridPos>, usize)>
// where
    // F: Fn(&GridPos) -> bool,
{
    astar(
        start,
        |n| {
            grid.get_neighbors_of(n)
                .iter()
                .flatten()
                .filter(|pos| can_climb(grid, n, pos, gear))
                // .filter(|pos| filter.as_ref().map(|f| f(pos)).or(Some(false)).unwrap())
                .cloned()
                .map(|p| (p, 1))
                .collect::<Vec<_>>()
        },
        |n| n.distance(end),
        |n| n == end,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = "\
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi";

    #[test]
    fn test_part1() {
        let gear = ClimbingGear { ascend: 1 };
        let grid = read_grid::<8, 5>(TEST);
        let (start, end) = grid.start_end();

        let path = search::<8, 5>(&grid, &start, &end, &gear);

        assert!(path.is_some());
        let path = path.unwrap();

        assert_eq!(31, path.1);
    }

    #[test]
    fn test_part2() {
        let gear = ClimbingGear { ascend: 1 };
        let grid = read_grid::<8, 5>(TEST);

        let (_, end) = grid.start_end();

        let mut paths_from_a = Vec::new();
        for (r, row) in grid.grid.iter().enumerate() {
            for (c, col) in row.iter().enumerate() {
                match col {
                    'S' | 'a' => {
                        let pos = GridPos::new(r, c);
                        let path = search(&grid, &pos, &end, &gear);
                        if let Some(path) = path {
                            paths_from_a.push(path.1);
                        } else {
                            println!("Cant find path from {pos:?} to goal");
                        }
                    }
                    _ => (),
                }
            }
        }

        let min = paths_from_a.iter().min().unwrap();
        assert_eq!(29, *min);
    }
}
