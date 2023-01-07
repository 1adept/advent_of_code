use std::collections::BTreeMap;

static VISUALIZE: bool = false;
pub fn tasks() {
    let input = super::load_input(22);
    println!("Part1: {}", part1(&input));
    println!("Part2: {}", part2(&input));
    // 162155
}

fn part1(input: &str) -> usize {
    let (map, path) = parse(input);

    let walker = {
        let mut w = Walker::default();
        w.pos = map.find_wrapped_pos(&w.pos, &w.facing);
        w
    };

    let res = walk_map(walker, &map, &path);
    res.score()
}

fn part2(input: &str) -> usize {
    let (cube, mut state, path) = parse_cube(input, 50);

    let mut walker = Walker::default();
    while cube.faces[0].map.get(&walker.pos).is_none() {
        walker.pos.walk(&walker.facing);
    }
    debug_assert_eq!(Point::new(1, 1), walker.pos);

    walk_cube((&mut walker, &mut state), &cube, &path);
    walker.score()
}

fn walk_map(mut walker: Walker, map: &Map, path: &Path) -> Walker {
    if VISUALIZE {
        println!("{map}");
    }
    for step in path.path.iter() {
        for _i in 0..step.1 {
            assert!(map.map.get(&walker.pos).unwrap() == &Tile::Tile);

            if VISUALIZE {
                visualisation::draw_step(map, &walker.pos, &walker.facing, 10);
            }

            let new_point = walker.pos.walk(&walker.facing);
            let (new_pos, success) = match map.map.get(&new_point) {
                Some(tile) => match tile {
                    Tile::Tile => (new_point, true),
                    Tile::Wall => (walker.pos.clone(), false),
                },
                None => {
                    let wrapped = map.find_wrapped_pos(&new_point, &walker.facing);
                    if map.map.get(&wrapped).unwrap() == &Tile::Tile {
                        (wrapped, true)
                    } else {
                        (walker.pos.clone(), false)
                    }
                }
            };
            if success {
                walker.pos = new_pos;
            } else {
                break;
            }
        }
        if let Some(turn) = &step.0 {
            walker.facing = walker.facing.turn(turn);
        }
    }
    println!();

    walker
}

fn walk_cube((walker, state): (&mut Walker, &mut FaceState<6>), map: &MapCube, path: &Path) {
    if VISUALIZE {
        println!("{}", map.current_face(state));
    }
    for step in &path.path {
        for _ in 0..step.1 {
            assert_eq!(
                &Tile::Tile,
                map.current_face(state).map.get(&walker.pos).unwrap()
            );
            let (next_state, next_pos, next_facing) =
                map.walk_face(state, &walker.pos, &walker.facing);
            if Tile::Tile
                == *map
                    .get_face(next_state.sides[0].0)
                    .map
                    .get(&next_pos)
                    .unwrap()
            {
                if VISUALIZE && *state != next_state {
                    println!("{}", map.get_face(state.sides[0].0));
                }
                *state = next_state;
                walker.pos = next_pos;
                walker.facing = next_facing;
            } else {
                break;
            }
            if VISUALIZE {
                visualisation::draw_step(
                    map.get_face(state.sides[0].0),
                    &walker.pos,
                    &walker.facing,
                    50,
                );
            }
        }
        if let Some(turn) = &step.0 {
            walker.facing.turn_mut(turn);
        }
    }

    let sector = map.face_sectors.get(&state.sides[0].0).unwrap();
    let current_face = &map.current_face(state);
    walker.pos.row += current_face.max.row * sector.0;
    walker.pos.col += current_face.max.col * sector.1;
}

fn parse_cube(input: &str, size: usize) -> (MapCube, FaceState<6>, Path) {
    let mut faces = BTreeMap::new();

    let determine_sector = |row, col| (row / size, col / size);
    let get_offset = |(row, col)| Point::new(row * size, col * size);

    let (map_str, path_str) = map_path_strings(input);

    for (row, line) in map_str.lines().enumerate() {
        for (col, c) in line.chars().enumerate() {
            if c.is_whitespace() {
                continue;
            }

            let sector = determine_sector(row, col);
            let offset = get_offset(sector);

            let (point, tile) = match c {
                '.' => (
                    Point::new(1 + row - offset.row, 1 + col - offset.col),
                    Tile::Tile,
                ),
                '#' => (
                    Point::new(1 + row - offset.row, 1 + col - offset.col),
                    Tile::Wall,
                ),
                _ => continue,
            };

            let m: &mut Map = faces.entry(sector).or_default();
            m.max.row = m.max.row.max(point.row);
            m.max.col = m.max.col.max(point.col);
            m.map.insert(point, tile);
        }
    }

    debug_assert_eq!(6, faces.len());
    let (map, face_state) = parse_faces(faces);
    let path = parse_path(path_str);

    (map, face_state, path)
}

fn parse_faces(faces: BTreeMap<(usize, usize), Map>) -> (MapCube, FaceState<6>) {
    let sectors = &faces.keys().cloned().collect::<Vec<_>>();
    let faces_map: [Map; 6] = (faces.into_values())
        .into_iter()
        .take(6)
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();
    let top_sector = *sectors.first().unwrap();
    let calc_init_rot = |row_diff, col| {
        if col == top_sector.1 {
            return Direction::Up;
        }
        let mut dir = Direction::Up;
        let rotate = if col < top_sector.1 {
            Direction::Right
        } else {
            Direction::Left
        };

        for _ in 0..row_diff {
            for _ in 0..top_sector.1.abs_diff(col) {
                dir.turn_mut(&rotate);
            }
        }
        dir
    };
    let mut fs = [Some((0, Direction::Up)), None, None, None, None, None];
    let mut sector_faces = BTreeMap::new();
    sector_faces.insert(0, top_sector);
    for abs_col in 0..=top_sector.1 {
        let same_col: Vec<_> = sectors
            .iter()
            .enumerate()
            .filter(|s| s.1 != &top_sector)
            .filter(|s| s.1 .1.abs_diff(top_sector.1) == abs_col)
            .collect();
        for (i, c) in same_col {
            let slot = if abs_col == 0 {
                if fs[2].is_none() {
                    2
                } else {
                    5
                }
            } else if c.1 > top_sector.1 && fs[4].is_none() {
                4
            } else if fs[3].is_none() {
                3
            } else {
                1
            };

            debug_assert!(fs[slot].is_none());
            fs[slot] = Some((i, calc_init_rot(c.0, c.1)));
            sector_faces.insert(i, *c);
        }
    }

    let map = MapCube {
        faces: faces_map,
        face_sectors: sector_faces,
    };
    let faces = FaceState {
        sides: fs.map(|s| s.unwrap()),
    };
    (map, faces)
}

fn parse(input: &str) -> (Map, Path) {
    let mut map = BTreeMap::new();
    let mut max = Point::new(0, 0);

    let (map_str, path_str) = map_path_strings(input);

    for (row, line) in map_str.lines().enumerate() {
        for (col, c) in line.chars().enumerate() {
            match c {
                '.' => {
                    map.insert(Point::new(1 + row, 1 + col), Tile::Tile);
                    max.row = max.row.max(1 + row);
                    max.col = max.col.max(1 + col);
                }
                '#' => {
                    map.insert(Point::new(1 + row, 1 + col), Tile::Wall);
                    max.row = max.row.max(1 + row);
                    max.col = max.col.max(1 + col);
                }
                _ => continue,
            }
        }
    }

    let path = parse_path(path_str);

    let map = Map { map, max };
    (map, path)
}

fn parse_path(path_str: &str) -> Path {
    let mut path = Vec::new();
    let mut value = 0;
    for char in path_str.chars().chain(" ".chars()) {
        match char {
            'R' => {
                path.push((Some(Direction::Right), value));
                value = 0;
            }
            'L' => {
                path.push((Some(Direction::Left), value));
                value = 0;
            }
            _ => {
                if char.is_whitespace() {
                    path.push((None, value));
                    continue;
                }
                value *= 10;
                value += char
                    .to_digit(10)
                    .unwrap_or_else(|| panic!("No toDigit for <{char}>"))
                    as u8;
            }
        }
    }
    Path { path }
}

fn map_path_strings(input: &str) -> (&str, &str) {
    let (map_str, path_str) = match input.split_once("\r\n\r\n") {
        Some(parts) => parts,
        None => match input.split_once("\n\n") {
            Some(parts) => parts,
            None => panic!("Could not extract map and path from input"),
        },
    };
    (map_str, path_str)
}

#[derive(Debug)]
struct Walker {
    pos: Point,
    facing: Direction,
}

impl Walker {
    fn score(&self) -> usize {
        let mut score = 0;
        score += 1000 * self.pos.row;
        score += 4 * self.pos.col;
        score += match self.facing {
            Direction::Up => 3,
            Direction::Down => 1,
            Direction::Left => 2,
            Direction::Right => 0,
        };
        score
    }
}

impl Default for Walker {
    fn default() -> Self {
        Self {
            pos: Point::new(1, 1),
            facing: Direction::Right,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct FaceState<const FACES: usize> {
    /// Top, Up, Down, Left, Right, Bottom
    /// Direction of each face is the facing towards the initial top
    sides: [(usize, Direction); FACES],
}

impl<const FACES: usize> FaceState<FACES> {
    /// Change face and rotations
    /// If left is the initial state, changing state by going `Down` will change it like this
    ///
    /// +---+---+---+    +---+---+---+   
    /// |   | v |   |    |   | ^ |   |
    /// +---+---+---+    +---+---+---+
    /// | > | ^ | v |    | ^ | ^ | < |
    /// +---+---+---+ => +---+---+---+
    /// |   | ^ |   |    |   | ^ |   |
    /// +---+---+---+    +---+---+---+
    /// |   | ^ |   |    |   | v |   |
    /// +---+---+---+    +---+---+---+
    ///
    /// 'Arrows' indicate which side is up, when that face is on-top
    fn change_face(&mut self, dir: &Direction) {
        assert!(FACES > 1);
        match dir {
            Direction::Up => {
                // Go up with left right staying the same
                self.sides[3].1.turn_mut(&Direction::Right);
                self.sides[4].1.turn_mut(&Direction::Left);
                self.rotate(1, 5, 2);
            }
            Direction::Down => {
                // Go down with left + right staying the same
                self.sides[3].1.turn_mut(&Direction::Left);
                self.sides[4].1.turn_mut(&Direction::Right);
                self.rotate(2, 5, 1);
            }
            Direction::Left => {
                //
                // flip_all_but_sides();
                self.sides[1].1.turn_mut(&Direction::Left);
                self.sides[2].1.turn_mut(&Direction::Right);
                self.sides[5].1 = self.sides[5].1.flip();
                self.rotate(3, 5, 4);
                self.sides[5].1 = self.sides[5].1.flip();
            }
            Direction::Right => {
                //
                // flip_all_but_sides();
                self.sides[1].1.turn_mut(&Direction::Right);
                self.sides[2].1.turn_mut(&Direction::Left);
                self.sides[5].1 = self.sides[5].1.flip();
                self.rotate(4, 5, 3);
                self.sides[5].1 = self.sides[5].1.flip();
            }
        };
    }

    /// Rotates faces with face1-face3 being the next 3 faces (from `top`)
    /// so that face1 -> bottom, face2 -> down, face3 -> top, top = face1
    fn rotate(&mut self, face1: usize, face2: usize, face3: usize) {
        assert!(FACES > 1);
        let temp = self.sides[face1].clone();
        self.sides[face1] = self.sides[face2].clone();
        self.sides[face2] = self.sides[face3].clone();
        self.sides[face3] = self.sides[0].clone();
        self.sides[0] = temp;
    }
}

pub struct MapCube {
    /// Top, Up, Down, Left, Right, Bottom
    faces: [Map; 6],
    face_sectors: BTreeMap<usize, (usize, usize)>,
}

impl MapCube {
    fn walk_face(
        &self,
        face_state: &FaceState<6>,
        pos: &Point,
        dir: &Direction,
    ) -> (FaceState<6>, Point, Direction) {
        let next = pos.walk(dir);
        if self.current_face(face_state).map.get(&next).is_some() {
            (face_state.clone(), next, dir.clone())
        } else {
            // When face is 'left' and we exit 'left' we actually go _down_ the cube
            let cube_turn_dir = face_state.sides[0].1.walk_direction_of(dir);
            let new_state = {
                let mut new_state = face_state.clone();
                new_state.change_face(&cube_turn_dir);
                new_state
            };
            let old_face_rotation = &face_state.sides[0].1;
            let new_face_rotation = &new_state.sides[0].1;
            let map = self.current_face(&new_state);

            let enter_with = map.enter_with(pos, dir, old_face_rotation, new_face_rotation);

            (new_state, enter_with.0, enter_with.1)
        }
    }

    fn current_face(&self, face_state: &FaceState<6>) -> &Map {
        self.get_face(face_state.sides[0].0)
    }

    fn get_face(&self, face_index: usize) -> &Map {
        &self.faces[face_index]
    }
}

#[derive(Debug, Default)]
pub struct Map {
    max: Point,
    map: BTreeMap<Point, Tile>,
}

/// Map is top-down-orientet, meaning (1,1) is in the visualized "top-left" and the max is "bottom-right"
impl Map {
    fn enter_with(
        &self,
        pos: &Point,
        dir: &Direction,
        old_rot: &Direction,
        new_rot: &Direction,
    ) -> (Point, Direction) {
        let face_dir = old_rot.walk_direction_of(dir);
        let enter_new_from = new_rot.relative_dir(&face_dir.opposite());

        let rev = |x: usize| (self.max.row + 1) - x;

        let origin = |d: &Direction| match d {
            Direction::Up | Direction::Right => Direction::Left,
            _ => Direction::Right,
        };

        let template = match (old_rot, new_rot) {
            (x, y) if x == y => pos.clone(),
            (x, y) if x == &y.opposite() => {
                Point::new((self.max.row + 1) - pos.row, (self.max.col + 1) - pos.col)
            }
            (x, y) if origin(x) == origin(y) => Point::new(pos.col, pos.row),
            _ => Point::new(rev(pos.col), rev(pos.row)),
        };

        let enter_pos = match enter_new_from {
            Direction::Down => Point::new(self.max.row, template.col),
            Direction::Up => Point::new(1, template.col),
            Direction::Left => Point::new(template.row, 1),
            Direction::Right => Point::new(template.row, self.max.col),
        };
        (enter_pos, enter_new_from.opposite())
    }

    fn is_oob(&self, point: &Point) -> bool {
        point.row == 0 || point.col == 0 || point.row > self.max.row || point.col > self.max.col
    }

    /// Find the first Tile-Point from `from` facing the direction `dir`
    fn find_wrapped_pos(&self, from: &Point, dir: &Direction) -> Point {
        if self.map.get(from).is_some() {
            return from.clone();
        }

        let mut finder = from.clone();
        if !self.is_oob(from) {
            let mut next_finder = finder.walk(dir);
            while !self.is_oob(&next_finder) && self.map.get(&finder).is_none() {
                finder = next_finder;
                next_finder = finder.walk(dir);
            }
        }

        if self.map.get(&finder).is_none() {
            finder = match dir {
                Direction::Up => Point::new(self.max.row, finder.col),
                Direction::Down => Point::new(1, finder.col),
                Direction::Left => Point::new(finder.row, self.max.col),
                Direction::Right => Point::new(finder.row, 1),
            };
            self.find_wrapped_pos(&finder, dir)
        } else {
            finder
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Point {
    row: usize,
    col: usize,
}

impl Point {
    fn new(x: usize, y: usize) -> Point {
        Point { row: x, col: y }
    }

    fn walk(&self, dir: &Direction) -> Point {
        let Point { row, col } = *self;
        match dir {
            Direction::Up => Point::new(row - 1, col),
            Direction::Down => Point::new(row + 1, col),
            Direction::Left => Point::new(row, col - 1),
            Direction::Right => Point::new(row, col + 1),
        }
    }
}

#[derive(Debug)]
struct Path {
    path: Vec<(Option<Direction>, u8)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Tile {
    Tile,
    Wall,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn turn(&self, other: &Self) -> Self {
        match (self, other) {
            (Direction::Down, Direction::Left) => Direction::Right,
            (Direction::Down, Direction::Right) => Direction::Left,
            (Direction::Left, Direction::Left) => Direction::Down,
            (Direction::Left, Direction::Right) => Direction::Up,
            (Direction::Right, Direction::Left) => Direction::Up,
            (Direction::Right, Direction::Right) => Direction::Down,
            _ => other.clone(),
        }
    }

    fn turn_mut(&mut self, direction: &Self) {
        *self = self.turn(direction);
    }

    fn flip(&self) -> Self {
        match self {
            Direction::Up => Direction::Down,
            Direction::Down => Direction::Up,
            Direction::Left => Direction::Right,
            Direction::Right => Direction::Left,
        }
    }

    fn opposite(&self) -> Self {
        match self {
            Direction::Up => Direction::Down,
            Direction::Down => Direction::Up,
            Direction::Left => Direction::Right,
            Direction::Right => Direction::Left,
        }
    }

    /// Walking `Up` in a left-facing `Direction` will result in a `Left` Direction
    fn walk_direction_of(&self, walk_dir: &Direction) -> Self {
        match walk_dir {
            Direction::Up => self.clone(),
            Direction::Down => self.opposite(),
            d => self.turn(d),
        }
    }

    /// This basically answers the question
    /// 'What is my relative `Direction` when going in absolute `Direction`'
    /// ```
    /// let left = Direction::Left;
    /// assert_eq!(Direction::Down, left.relative_dir(&Direction::Right));
    /// assert_eq!(Direction::Right, left.relative_dir(&Direction::Up));
    /// ```
    fn relative_dir(&self, abs_move_dir: &Self) -> Self {
        match (self, abs_move_dir) {
            (Direction::Up, _) => abs_move_dir.clone(),
            (Direction::Down, _) => abs_move_dir.opposite(),
            (_, Direction::Up) => Direction::Up.turn(&self.opposite()),
            (_, Direction::Down) => Direction::Down.turn(self),
            (x, y) if x == y => Direction::Up,
            _ => Direction::Down,
        }
    }
}

mod visualisation {
    use crossterm::{
        cursor, execute,
        style::{Color, Print, SetForegroundColor},
    };
    use std::{fmt::Display, time::Duration};

    use super::{Direction, FaceState, Map, Point, Tile};

    pub fn draw_step(map: &Map, point: &Point, dir: &Direction, timeout: u64) {
        let mut stdout = std::io::stdout();

        let p = cursor::position().unwrap();
        let pos = Point::new(1 + map.max.row - point.row, point.col);
        let dir_char = match dir {
            Direction::Up => '^',
            Direction::Down => 'v',
            Direction::Left => '<',
            Direction::Right => '>',
        };
        execute!(
            stdout,
            cursor::MoveUp(pos.row as u16),
            cursor::MoveRight(pos.col as u16),
            SetForegroundColor(Color::Red),
            Print(dir_char),
            SetForegroundColor(Color::Reset),
            cursor::MoveTo(p.0, p.1)
        )
        .unwrap();
        std::thread::sleep(Duration::from_millis(timeout));
    }

    impl Display for Map {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let str = (0..=self.max.row)
                .map(|row| {
                    "\n".to_owned()
                        + &(0..=self.max.col)
                            .map(|c| match self.map.get(&Point::new(row, c)) {
                                Some(tile) => tile.to_string(),
                                None => " ".to_owned(),
                            })
                            .collect::<String>()
                })
                .collect::<String>();
            write!(f, "{str}")
        }
    }

    impl Display for Tile {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let char = match self {
                Tile::Tile => '.',
                Tile::Wall => '#',
            };
            write!(f, "{char}")
        }
    }

    impl Display for FaceState<6> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let cap = |dir: &Direction| match dir {
                Direction::Up => "U",
                Direction::Down => "D",
                Direction::Left => "L",
                _ => "R",
            };
            write!(
                f,
                r"On face {}
            +---+---+---+
            |   |{} {}|   |
            +---+---+---+
            |{} {}|{} {}|{} {}|
            +---+---+---+
            |   |{} {}|   |
            +---+---+---+
            |   |{} {}|   |
            +---+---+---+
            ",
                self.sides[0].0,
                self.sides[1].0,
                cap(&self.sides[1].1),
                self.sides[3].0,
                cap(&self.sides[3].1),
                self.sides[0].0,
                cap(&self.sides[0].1),
                self.sides[4].0,
                cap(&self.sides[4].1),
                self.sides[2].0,
                cap(&self.sides[2].1),
                self.sides[5].0,
                cap(&self.sides[5].1)
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_day22_part1() {
        let (map, path) = parse(TEST);

        let mut walker = Walker::default();
        walker.pos = map.find_wrapped_pos(&walker.pos, &walker.facing);
        assert_eq!(Point::new(1, 9), walker.pos);

        let res = walk_map(walker, &map, &path);
        let score = res.score();
        assert_eq!(6032, score);
    }

    #[test]
    fn test_day22_part2() {
        let (map, mut state, path) = parse_cube(TEST, 4);

        let mut walker = Walker::default();
        while map.faces[0].map.get(&walker.pos).is_none() {
            walker.pos.walk(&walker.facing);
        }
        assert_eq!(Point::new(1, 1), walker.pos);

        walk_cube((&mut walker, &mut state), &map, &path);
        let score = walker.score();
        assert_eq!(5031, score);
    }

    const TEST: &str = "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5";
}
