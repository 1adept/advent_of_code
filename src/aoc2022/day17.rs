#![allow(unused)]
use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
    iter::Cycle,
    slice::Iter,
};

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
    const ONE_BIO: usize = 1_000_000_000_000;
    let input = super::load_input(17);
    let sequence = parse_input_sequence(&input);
    let rocks = parse_input_rocks();

    let h1 = drop(&input, 2022);
    println!("Height after 2022 rocks = {h1}",);

    // let h2 = drop(&input, ONE_BIO);
    // println!("Height after {ONE_BIO} rocks = {h2}");
    // 1555113636385
}

fn drop(input: &str, num_rocks: usize) -> usize {
    let mut sequence = parse_input_sequence(input);
    let mut rocks = parse_input_rocks();

    Chamber::default().drop_rocks(
        &mut 0,
        num_rocks,
        &mut sequence,
        &mut rocks,
        (false, false, 200),
    )
}

fn find_repitition(sequence: &Sequence, rocks: &Rocks, length: u64) -> (u64, u64, u64) {
    let len_seq = sequence.sequence.len() as u64;
    let len_rocks = rocks.len() as u64;
    println!("Sequence: {len_seq}, rocks: {len_rocks}");
    let common = len_seq * len_rocks;
    let div = length / common;
    let rest = length % common;
    println!("Common is {common} so divided by that is {div} with {rest} remaining");
    (common, div, rest)
}

struct Rocks {
    rocks: VecDeque<Rock>,
}

impl Rocks {
    fn next(&self, index: usize) -> Rock {
        self.rocks[index % self.rocks.len()].clone()
    }

    fn len(&self) -> usize {
        self.rocks.len()
    }
}

#[derive(Debug, Clone, Eq)]
struct Rock {
    rock_id: u8,
    height: u8,
    bits: [u64; 7],
}

impl std::hash::Hash for Rock {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u8(self.rock_id);
    }
}

impl PartialEq for Rock {
    fn eq(&self, other: &Self) -> bool {
        self.rock_id == other.rock_id
    }
}

impl Rock {
    const ROCK: char = '#';
    const EMPTY: char = '.';
    const FALLING: char = '@';

    fn shifted(&self, dir: &Direction) -> Self {
        let mut b = [0u64; Chamber::W];
        match dir {
            Direction::Up => {
                b[..].copy_from_slice(&self.bits.map(|b| b << 1));
            }
            Direction::Down => {
                b[..].copy_from_slice(&self.bits.map(|b| b >> 1));
            }
            Direction::Left => {
                b[..(Chamber::W - 1)].copy_from_slice(&self.bits[1..]);
            }
            Direction::Right => {
                b[1..].copy_from_slice(&self.bits[0..(Chamber::W - 1)]);
            }
        }
        Self {
            rock_id: self.rock_id,
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
}

impl Display for Rock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = String::with_capacity(self.height as usize * self.bits.len());
        for row in 0..self.height {
            for col in 0..self.bits.len() {
                let is_rock = (1 << row) == (1 << row) & self.bits[col];
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
                let is_rock = (1 << row) == (1 << row) & self.bits[col];
                let c = if is_rock { Rock::ROCK } else { Rock::EMPTY };
                str.push(c);
            }
            str.push('\n');
        }
        write!(f, "{}", str)
    }
}

type Stash = u64;
#[derive(Debug, Default, PartialEq, Eq)]
struct Chamber {
    bits: [u64; Self::W],
    current_height: u64,
    total_height: u64,
}

impl Chamber {
    const STASH_SIZE: Stash = 32;
    const W: usize = 7;

    fn cols() -> std::ops::Range<usize> {
        0..Self::W
    }

    /// Starting height for current "window"
    fn start_position(&self) -> u64 {
        self.current_height + 3
    }

    fn is_space(&self, rock: &Rock) -> bool {
        (0..Self::W).all(|c| !self.bits[c] | !rock.bits[c] == u64::MAX)
    }

    fn settle_rock(&mut self, rock: Rock) {
        for c in Self::cols() {
            self.bits[c] |= rock.bits[c];
        }

        let new_height = self.calculate_height();

        if new_height == self.current_height {
            return;
        }

        let change = new_height - self.current_height;

        let mut trimmed = 0;
        let any_one = |bits: &[u64; Self::W]| bits.as_ref().iter().any(|c| *c == 1 || *c == 0);
        loop {
            if any_one(&self.bits) {
                break;
            }
            for c in Self::cols() {
                self.bits[c] >>= 1;
            }
            trimmed += 1;
        }

        if trimmed == 0 && self.current_height > 64 - 8 {
            draw(self, &rock, 0);
            let trim_to_8 = change + 16  - (64 - self.current_height);
            self.bits.iter_mut().for_each(|b| *b >>= trim_to_8);
            trimmed = trim_to_8;
        }

        self.total_height += change;
        self.current_height = new_height - trimmed;
    }

    fn calculate_height(&mut self) -> u64 {
        let mut h = u64::MAX;
        for c in Self::cols() {
            if self.bits[c] == 0 {
                continue;
            }
            h = h.min(self.bits[c].leading_zeros() as u64);
        }
        64 - h
    }

    fn drop_rocks(
        &mut self,
        i: &mut usize,
        num_rocks: usize,
        sequence: &mut Sequence,
        rocks: &mut Rocks,
        print: (bool, bool, u64),
    ) -> usize {
        let timeout = print.2;
        let steps = print.1;
        let prints = print.0;

        if num_rocks == 0 || i.abs_diff(num_rocks) == 0 {
            return 0;
        }

        let mut cache = HashMap::new();

        let end = *i + num_rocks;
        while *i < end {
            let key = (*i % rocks.len(), sequence.current, self.bits);

            if let Some((height_before, i_before)) = cache.get(&key) {
                let passed_rocks = *i - i_before;
                let passed_height = self.total_height - height_before;

                let remaining_rocks = end - *i;
                let repeat = remaining_rocks / passed_rocks;
                let skip_rocks_count = repeat * passed_rocks;

                let remaining_num_rocks = remaining_rocks - skip_rocks_count;
                let skipped_height = passed_height as usize * repeat;

                // Cant add to self.total just yet
                let mut total_height = self.total_height + skipped_height as u64;

                let remaining_height =
                    self.drop_rocks(i, remaining_num_rocks, sequence, rocks, print);

                let test1 = skipped_height + remaining_height;

                dbg!(
                    self.total_height,
                    test1,
                    passed_rocks,
                    passed_height,
                    repeat,
                    remaining_rocks,
                    remaining_num_rocks,
                    skip_rocks_count,
                    skipped_height,
                    height_before,
                );

                self.total_height = test1 as u64;
                break;
            } else {
                let height_before = self.total_height;
                let rock = rocks.next(*i);
                self.simulate_rock_fall(sequence, rock, print);
                cache.insert(key, (height_before, *i));
            }
            *i += 1;
        }
        self.total_height as usize
    }

    fn simulate_rock_fall(
        &mut self,
        sequence: &mut Sequence,
        mut rock: Rock,
        print: (bool, bool, u64),
    ) {
        for _h in 0..self.start_position() {
            rock = rock.shifted(&Direction::Up);
        }
        if print.0 {
            draw(self, &rock, print.2 * 2);
        }

        let mut alternator = true;
        loop {
            let dir = if alternator {
                sequence.advance()
            } else {
                Direction::Down
            };
            let shifted = rock.shifted(&dir);
            if rock.can_dir(&dir) && self.is_space(&shifted) {
                rock = shifted;
            } else if dir == Direction::Down {
                break;
            }
            alternator = !alternator;

            if print.0 {
                draw(self, &rock, print.2);
            }
        }
        if print.0 || print.1 {
            println!("{}", self.total_height);
            draw(self, &rock, print.2);
        }
        self.settle_rock(rock);
    }
}

#[derive(Clone)]
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

    fn next(&self, index: usize) -> Direction {
        match self.sequence[index % self.len()] {
            b'<' => Direction::Left,
            b'>' => Direction::Right,
            _ => unreachable!("No direction"),
        }
    }

    fn cycle(&self) -> Cycle<Iter<u8>> {
        self.sequence.iter().cycle()
    }

    fn reset(&mut self) {
        self.current = 0;
    }

    fn len(&self) -> usize {
        self.sequence.len()
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

fn parse_input_sequence(input: &str) -> Sequence {
    Sequence::new(input)
}

fn parse_input_rocks() -> Rocks {
    let input = ROCKS;

    let mut rocks = VecDeque::new();
    for (i, block) in input.split("\n\n").enumerate() {
        let mut rock_cols = [0; 7];
        let mut height = 0;
        let line_count = block.lines().count();
        for (row, line) in block.lines().enumerate() {
            for (char_index, c) in line.chars().enumerate() {
                if c == Rock::ROCK {
                    rock_cols[2 + char_index] |= 1 << (line_count - 1 - row);
                }
            }
            height += 1;
        }
        let r = Rock {
            rock_id: i as u8,
            height,
            bits: rock_cols,
        };

        rocks.push_back(r);
    }
    Rocks { rocks }
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
        let mut rocks = parse_input_rocks();
        let mut seq = parse_input_sequence(TEST);

        let height = drop(TEST, 2022);
        assert_eq!(3068, height);
    }

    #[test]
    fn test_day17_part2() {
        let mut rocks = parse_input_rocks();
        let mut seq = parse_input_sequence(TEST);

        let one_bio: usize = 1_000_000_000_000;

        let total = drop(TEST, one_bio);
        assert_eq!(1_514_285_714_288, total);
    }

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

        for i in sequence {
            assert_eq!(i, seq.advance());
        }
    }
    const TEST: &str = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";
}
