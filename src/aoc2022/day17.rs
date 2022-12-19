use std::{collections::VecDeque, fmt::Display};

const ROCKS: &str = "\
####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##";

pub fn tasks() {
    let input = super::load_input(17);
    let sequence = parse_input_sequence(&input);

    let rocks = parse_input_rocks();

    let height = drop_rocks(sequence, rocks, (false, false, 100));
    println!("Height after 2022 rocks = {height}");
    // 3168 - too low (answer for someone else ?)
    // 3175 soll
}

fn drop_rocks(
    mut sequence: Sequence,
    mut rocks: VecDeque<Rock>,
    (print, steps, timeout): (bool, bool, u64),
) -> usize {
    let mut chamber = Chamber::default();
    for _i in 0..2022 {
        let r = rocks.pop_front().unwrap();
        let mut rock = r.clone();
        rocks.push_back(r);

        for _h in 0..chamber.start_position() {
            rock = rock.shifted(&Direction::Up);
        }
        if print {
            draw(&chamber, &rock, timeout * 2);
        }

        let mut alternator = true;
        loop {
            if alternator {
                let dir = sequence.advance();
                let shifted = rock.shifted(&dir);
                if rock.can_dir(&dir) && chamber.is_space(&shifted) {
                    rock = shifted;
                }
            } else {
                let down = rock.shifted(&Direction::Down);
                if rock.can_dir(&Direction::Down) && chamber.is_space(&down) {
                    rock = down;
                } else {
                    if print {
                        draw(&chamber, &rock, timeout);
                    }
                    break;
                }
            };
            alternator = !alternator;

            if print {
                draw(&chamber, &rock, timeout);
            }
        }
        if !print && steps {
            println!("{}", chamber.height());
            draw(&chamber, &rock, timeout);
        }
        chamber.settle_rock(rock);
    }
    chamber.height()
}

#[derive(Debug, Clone)]
struct Rock {
    height: u8,
    bits: [u64; 7],
}

impl Rock {
    const ROCK: char = '#';
    const EMPTY: char = '.';
    const FALLING: char = '@';

    fn shifted(&self, dir: &Direction) -> Self {
        let mut b = [0u64; Chamber::W];
        match dir {
            Direction::Up => {
                for c in 0..Chamber::W {
                    b[c] = self.bits[c] << 1;
                }
            }
            Direction::Down => {
                for c in 0..Chamber::W {
                    b[c] = self.bits[c] >> 1;
                }
            }
            Direction::Left => {
                for c in 0..Chamber::W - 1 {
                    b[c] = self.bits[c + 1];
                }
            }
            Direction::Right => {
                for c in 1..Chamber::W {
                    b[c] = self.bits[c - 1];
                }
            }
        }
        Self {
            height: self.height,
            bits: b,
        }
    }

    fn can_dir(&self, dir: &Direction) -> bool {
        match dir {
            Direction::Up => Chamber::cols().all(|c| self.bits[c] & (2u64.pow(64)) != 1),
            Direction::Down => Chamber::cols().all(|c| self.bits[c] & 1 != 1),
            Direction::Left => self.bits[0] == 0,
            Direction::Right => self.bits[Chamber::W - 1] == 0,
        }
    }

    /// Unchecked shift in direction
    fn shift(&mut self, dir: &Direction) {
        *self = self.shifted(dir);
    }
}

impl Display for Rock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = String::with_capacity(self.height as usize * self.bits.len());
        for row in 0..self.height {
            for col in 0..self.bits.len() {
                let is_rock = (1 << row) == (1 << row) & self.bits[col as usize];
                let c = if is_rock { Rock::ROCK } else { Rock::EMPTY };
                str.push(c);
            }
            str.push('\n');
        }
        write!(f, "{}", str)
    }
}

impl Display for Chamber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = String::with_capacity(64 * Self::W);
        for row in 0..64 {
            for col in 0..Self::W {
                let is_rock = (1 << row) == (1 << row) & self.bits[col as usize];
                let c = if is_rock { Rock::ROCK } else { Rock::EMPTY };
                str.push(c);
            }
            str.push('\n');
        }
        write!(f, "{}", str)
    }
}

#[derive(Debug, Default)]
struct Chamber {
    bits: [u64; Self::W],
    resting_rocks: Vec<Rock>,
    height: usize,
    stash: Vec<[Stash; Self::W]>,
}

type Stash = u32;
impl Chamber {
    const STASH: Stash = 32;
    const W: usize = 7;

    fn cols() -> std::ops::Range<usize> {
        0..Self::W
    }

    /// Starting height for current "window"
    fn start_position(&self) -> usize {
        self.height + 3
    }

    fn is_space(&self, rock: &Rock) -> bool {
        (0..Self::W).all(|c| !self.bits[c] | !rock.bits[c] == u64::MAX)
    }

    // fn remove_rock(&mut self, rock: &Rock) {
    //     for c in 0..Self::W {
    //         self.bits[c] = self.bits[c] & !rock.bits[c];
    //     }
    // }

    fn settle_rock(&mut self, rock: Rock) {
        for c in 0..Self::W {
            self.bits[c] |= rock.bits[c];
        }

        self.resting_rocks.push(rock);
        self.calculate_height();
        self.stash();
    }

    fn stash(&mut self) {
        // If there an opporunity to trim the bottom take it, else take the 'emergency' || path
        if (self.height > Self::STASH as usize
            && Self::cols().all(|c| self.bits[c] >> Self::STASH != 0))
            || self.height > 64 - 8
        // Min size needed 64->Chamber, 8->(falling) Rock
        {
            let mut lowest = [0 as Stash; Self::W];
            for c in 0..Self::W {
                lowest[c] = self.bits[c] as Stash;
                self.bits[c] >>= Self::STASH;
            }
            self.stash.push(lowest);
            self.height -= Self::STASH as usize;
        }
    }

    fn calculate_height(&mut self) -> usize {
        let mut h = u32::MAX;
        for c in Self::cols() {
            if self.bits[c] == 0 {
                continue;
            }
            h = h.min(self.bits[c].leading_zeros());
        }
        self.height = (64 - h) as usize;
        self.height
    }

    fn height(&self) -> usize {
        let stashed_height = Self::STASH as usize * self.stash.len();
        stashed_height + self.height
    }
}

struct Sequence {
    sequence: Vec<u8>,
    current: usize,
}

impl Sequence {
    fn new(input: &str) -> Self {
        Self {
            current: 0,
            sequence: input.as_bytes().to_owned(),
        }
    }
    fn advance(&mut self) -> Direction {
        let c = self.sequence[self.current];
        let dir = match c {
            b'<' => Direction::Left,
            b'>' => Direction::Right,
            _ => unreachable!("No diretion {c} for now"),
        };
        self.current = (1 + self.current) % self.sequence.len();
        dir
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

fn parse_input_sequence(input: &str) -> Sequence {
    Sequence::new(input)
}

fn parse_input_rocks() -> VecDeque<Rock> {
    let input = ROCKS;

    let mut rocks = VecDeque::new();
    for block in input.split("\n\n") {
        let mut rock_cols = [0; 7];
        let mut height = 0;
        let line_count = block.lines().count();
        for (row, line) in block.lines().enumerate() {
            for (char_index, c) in line.chars().enumerate() {
                if c == Rock::ROCK {
                    rock_cols[2 + char_index] |= 1 << line_count - 1 - row;
                }
            }
            height += 1;
        }
        let r = Rock {
            height,
            bits: rock_cols.clone(),
        };

        rocks.push_back(r);
    }
    rocks
}

fn draw(chamber: &Chamber, rock: &Rock, timeout_ms: u64) {
    use std::thread::sleep;
    use std::time::Duration;

    let mut str = String::with_capacity(64 * Chamber::W);
    for row in (0..64).rev() {
        for c in Chamber::cols() {
            let matcher = 1 << row;
            let c = match (
                matcher == matcher & rock.bits[c],
                matcher == matcher & chamber.bits[c],
            ) {
                (true, _) => Rock::FALLING,
                (_, true) => Rock::ROCK,
                _ => Rock::EMPTY,
            };
            str.push(c);
        }
        str.push('\n');
    }
    println!("{str}");
    sleep(Duration::from_millis(timeout_ms));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_day17_part1() {
        let rocks = parse_input_rocks();
        let seq = parse_input_sequence(TEST);

        let height = drop_rocks(seq, rocks, (false, false, 100));

        assert_eq!(3068, height);
    }

    #[test]
    #[ignore = "TODO"]
    fn test_day17_part2() {}

    #[test]
    fn test_day17_sequence() {
        let mut seq = parse_input_sequence(TEST);

        let sequence = vec![
            Direction::Right,
            Direction::Right,
            Direction::Right,
            Direction::Left,
            Direction::Left,
            Direction::Right,
            Direction::Left,
            Direction::Right,
            Direction::Right,
            Direction::Left,
            Direction::Left,
            Direction::Left,
            Direction::Right,
            Direction::Right,
            Direction::Left,
            Direction::Right,
            Direction::Right,
            Direction::Right,
            Direction::Left,
            Direction::Left,
            Direction::Left,
            Direction::Right,
            Direction::Right,
            Direction::Right,
            Direction::Left,
            Direction::Left,
            Direction::Left,
            Direction::Right,
            Direction::Left,
            Direction::Left,
            Direction::Left,
            Direction::Right,
            Direction::Right,
            Direction::Left,
            Direction::Right,
            Direction::Right,
            Direction::Left,
            Direction::Left,
            Direction::Right,
            Direction::Right,
            Direction::Right,
            Direction::Right,
            Direction::Right,
            Direction::Left,
        ];

        for i in 0..sequence.len() {
            assert_eq!(sequence[i], seq.advance());
        }
    }
    const TEST: &str = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";
}
