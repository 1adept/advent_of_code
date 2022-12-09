use std::collections::HashSet;

pub fn tasks() {
    let input = super::load_input(9);
    let commands = read_commands(&input);

    let tail_positions_p1 = rope_tail_visits(&commands, 1);
    let tail_positions_p2 = rope_tail_visits(&commands, 9);

    println!("Tail positions Part1 = {}", tail_positions_p1);
    println!("Tail positions Part2 = {}", tail_positions_p2);
}

fn rope_tail_visits(commands: &[Command], tail_size: usize) -> usize {
    let mut head = Rope::head().with_tail(tail_size);

    let mut end = &head;
    while let Some(knot) = &end.knot {
        println!("{:?}", end);
        end = knot;
    }

    let mut visitor = RopeVisitor::new();

    let sep = "#".repeat(10);
    for cmd in commands {
        println!("{} {:?} {}", sep, cmd, sep);
        for _i in 0..cmd.len {
            head.move_dir(&cmd.dir);
            visitor.visit_last(&head);
        }
    }

    visitor.positions.len()
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Coordinate {
    x: isize,
    y: isize,
}

impl Coordinate {
    fn new(x: isize, y: isize) -> Coordinate {
        Coordinate { x, y }
    }
    fn move_dir(&self, dir: &Direction) -> Coordinate {
        match dir {
            Direction::Up => Coordinate::new(self.x, self.y + 1),
            Direction::Down => Coordinate::new(self.x, self.y - 1),
            Direction::Left => Coordinate::new(self.x - 1, self.y),
            Direction::Right => Coordinate::new(self.x + 1, self.y),
            Direction::UpLeft => Coordinate::new(self.x - 1, self.y + 1),
            Direction::UpRight => Coordinate::new(self.x + 1, self.y + 1),
            Direction::DownLeft => Coordinate::new(self.x - 1, self.y - 1),
            Direction::DownRight => Coordinate::new(self.x + 1, self.y - 1),
        }
    }

    fn touching(&self, other: &Coordinate) -> bool {
        (self.x - other.x).abs() <= 1 && (self.y - other.y).abs() <= 1
    }
}

#[derive(Debug)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
    UpLeft,
    UpRight,
    DownLeft,
    DownRight,
}

#[derive(Debug)]
struct Rope {
    name: String,
    coordinate: Coordinate,
    knot: Option<Box<Rope>>,
}

impl Rope {
    fn head() -> Self {
        Rope {
            name: "HEAD".to_owned(),
            coordinate: Coordinate::new(0, 0),
            knot: None,
        }
    }
    fn with_tail(mut self, tail_size: usize) -> Self {
        let mut tail = &mut self;
        for i in 0..tail_size {
            let mut new_tail = Rope::tail();
            new_tail.name = format!("Tail {}", 1 + i);
            tail.knot = Some(Box::new(new_tail));
            tail = tail.knot.as_mut().unwrap();
        }
        self
    }
    fn tail() -> Self {
        Self {
            name: "Tail".to_owned(),
            coordinate: Coordinate::new(0, 0),
            knot: None,
        }
    }

    fn move_dir(&mut self, dir: &Direction) {
        self.coordinate = self.coordinate.move_dir(dir);
        self.pull(); // Dont need to check because its dont in pull()
    }

    /// Pull child-knot if not in range
    fn pull(&mut self) {
        if let Some(knot) = &mut self.knot {
            let touch = self.coordinate.touching(&knot.coordinate);
            if !touch {
                let x = (self.coordinate.x - knot.coordinate.x).clamp(-1, 1);
                let y = (self.coordinate.y - knot.coordinate.y).clamp(-1, 1);

                // REMEMBER: Not in range
                let dir = match (x, y) {
                    (0, 1) => Direction::Up,
                    (0, -1) => Direction::Down,
                    (-1, 0) => Direction::Left,
                    (1, 0) => Direction::Right,
                    (1, 1) => Direction::UpRight,
                    (-1, 1) => Direction::UpLeft,
                    (1, -1) => Direction::DownRight,
                    (-1, -1) => Direction::DownLeft,
                    _ => unreachable!(),
                };
                knot.move_dir(&dir); // Will pull next 'automatically'
            }
        }
    }

    fn head_pull(&mut self) {
        let head = &self.coordinate;
    }
}

fn head_pull(head: &mut Rope) {
    let head_pos = head.coordinate.clone();

    let mut current = head;
    while let Some(knot) = &mut current.knot {
        let touch = current.coordinate.touching(&knot.coordinate);
        if !touch {
            let x = (head_pos.x - knot.coordinate.x).abs().clamp(-1, 1);
            let y = (head_pos.y - knot.coordinate.y).abs().clamp(-1, 1);

            // REMEMBER: Not in range
            let dir = match (x, y) {
                (0, 1) => Direction::Up,
                (0, -1) => Direction::Down,
                (-1, 0) => Direction::Left,
                (1, 0) => Direction::Right,
                (1, 1) => Direction::UpRight,
                (-1, 1) => Direction::UpLeft,
                (1, -1) => Direction::DownRight,
                (-1, -1) => Direction::DownLeft,
                _ => unreachable!(),
            };
            knot.move_dir(&dir); // Will pull next 'automatically'
        }
        current = knot;
    }
}

#[derive(Debug)]
struct Command {
    dir: Direction,
    len: usize,
}

struct RopeVisitor {
    positions: HashSet<Coordinate>,
}

impl RopeVisitor {
    pub fn new() -> Self {
        RopeVisitor {
            positions: HashSet::new(),
        }
    }

    fn visit(&mut self, rope: &Rope) {
        // let mut end: &Rope = rope.knot.as_ref().unwrap(); //Min 1 knot
        // self.positions.insert(end.coordinate.clone());
        let mut end = rope;
        println!("Head: {:?}", rope.coordinate);
        while let Some(knot) = &end.knot {
            println!("{} => {:?}", knot.name, knot.coordinate);
            end = knot;
            self.positions.insert(end.coordinate.clone());
        }
        println!("---");
    }

    fn visit_last(&mut self, rope: &Rope) {

        let mut end = rope;
        while let Some(knot) = &end.knot {
            end = knot;
        }

        self.positions.insert(end.coordinate.clone());
    }
}

fn read_commands(input: &str) -> Vec<Command> {
    input
        .lines()
        .map(|line| {
            let (dir, num) = line.split_once(' ').unwrap();
            Command {
                dir: match dir {
                    "U" => Direction::Up,
                    "D" => Direction::Down,
                    "L" => Direction::Left,
                    "R" => Direction::Right,
                    _ => unreachable!(),
                },
                len: num.parse::<usize>().unwrap(),
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT1: &str = "\
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2";

    const INPUT2: &str = "\
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20";

    #[test]
    fn test_part1() {
        let commands = read_commands(&INPUT1);

        assert_eq!(13, rope_tail_visits(&commands, 1));
    }

    #[test]
    fn test_part2() {
        let commands = read_commands(&INPUT2);

        assert_eq!(36, rope_tail_visits(&commands, 9));
    }
}
