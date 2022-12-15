pub fn tasks() {
    let input = super::load_input(15);
    let signals = parse_positions(&input);

    let row = 2_000_000;
    let occupied = occupied_in_row(row, &signals);
    println!("Occupied in {row} = {}", occupied.count());

    let max: PosType = 4_000_000;
    let empty = find_empty_pos(max, &signals);

    let score = calculate_pos_score(&empty);
    println!("Score at {empty:?} => {score}");
}

type PosType = i128;

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
struct Pos {
    row: i128,
    col: i128,
}

#[derive(Debug)]
struct CheckedPositions {
    row: PosType,
    checked: Vec<(PosType, PosType)>,
}

impl CheckedPositions {
    fn new(row: PosType) -> Self {
        Self {
            row,
            checked: Vec::new(),
        }
    }

    fn count(&self) -> PosType {
        let mut count = 0;
        for range in &self.checked {
            count += range.1 - range.0;
        }
        count
    }

    fn missing(&self) -> PosType {
        self.checked[0].1 + 1
    }

    fn add_checked(&mut self, (from, to): (Pos, Pos)) {
        assert_eq!(self.row, from.row);
        assert_eq!(self.row, to.row);

        let mut from = from.col;
        let mut to = to.col;

        for x in 0..self.checked.len() {
            let range = &mut self.checked[x];

            let (left, maybe_right) = Self::try_merge_range(*range, (from, to));
            *range = left;
            // Already merged
            if let Some((c, d)) = maybe_right {
                from = c;
                to = d;
            } else {
                // Right is None => already merged
                return;
            }
        }
        self.checked.push((from, to));
    }

    fn repeat_merge(&mut self) {
        if self.checked.len() == 1 {
            return;
        }

        for x in 0..self.checked.len() - 1 {
            let [left, right] = &mut self.checked[x..=(x+1)] else { unreachable!() };
            let (merged, rest) = Self::try_merge_range(*left, *right);
            *left = merged;
            if rest.is_none() {
                self.checked.remove(x + 1);
                self.repeat_merge();
                return;
            }
        }
    }

    fn try_merge_range(
        (a, b): (PosType, PosType),
        (c, d): (PosType, PosType),
    ) -> ((PosType, PosType), Option<(PosType, PosType)>) {
        if a > d + 1 {
            // println!("({a}-{b}) right of ({c}-{d})");
            ((c, d), Some((a, b)))
        } else if b < c - 1 {
            // println!("({a}-{b}) left of ({c}-{d})");
            ((a, b), Some((c, d)))
        } else {
            let min = a.min(c);
            let max = b.max(d);
            // println!("Just taking min of {a}/{c} => {min} and max of {b}/{d} => {max}");
            ((min, max), None)
        }
    }
}

impl Pos {
    fn new(row: PosType, col: PosType) -> Self {
        Pos { row, col }
    }

    fn manhatten(&self, other: &Pos) -> u128 {
        self.row.abs_diff(other.row) + self.col.abs_diff(other.col)
    }

    fn man_to_row(&self, beacon: &Pos, row: PosType) -> Option<(Pos, Pos)> {
        let manhatten = self.manhatten(beacon);
        let diference_row = self.row.abs_diff(row);
        let remaining: PosType = manhatten as PosType - diference_row as PosType;

        if remaining < 0 {
            None
        } else {
            Some((
                Pos::new(row, self.col - remaining),
                Pos::new(row, self.col + remaining),
            ))
        }
    }
}

fn find_empty_pos(max: PosType, signals: &[(Pos, Pos)]) -> Pos {
    for row in (0..=max).rev() {
        let checked = occupied_in_row(row as PosType, signals);
        if checked.checked.len() > 1 {
            return Pos::new(row, checked.missing());
        }
    }
    panic!("No missing position found");
}

fn occupied_in_row(row: PosType, signals: &[(Pos, Pos)]) -> CheckedPositions {
    let mut checked = CheckedPositions::new(row);
    for (signal, beacon) in signals {
        if let Some(min_max) = signal.man_to_row(beacon, row) {
            checked.add_checked(min_max);
        }
    }
    checked.repeat_merge();
    checked
}

fn parse_positions(input: &str) -> Vec<(Pos, Pos)> {
    use regex::Regex;
    let pattern =
        Regex::new(r#"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"#)
            .unwrap();

    let mut pairs = Vec::new();
    for line in input.lines() {
        let captures = pattern.captures(line).unwrap();
        let signal = Pos {
            row: captures
                .get(2)
                .unwrap()
                .as_str()
                .parse::<PosType>()
                .unwrap(),
            col: captures
                .get(1)
                .unwrap()
                .as_str()
                .parse::<PosType>()
                .unwrap(),
        };
        let beacon = Pos {
            row: captures
                .get(4)
                .unwrap()
                .as_str()
                .parse::<PosType>()
                .unwrap(),
            col: captures
                .get(3)
                .unwrap()
                .as_str()
                .parse::<PosType>()
                .unwrap(),
        };
        pairs.push((signal, beacon));
    }
    pairs
}

fn calculate_pos_score(pos: &Pos) -> PosType {
    pos.col * 4_000_000 + pos.row
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = "\
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3";

    #[test]
    fn test_beacon_range() {
        let signal = Pos::new(7, 8);
        let beacon = Pos::new(10, 2);

        let (min, max) = signal.man_to_row(&beacon, 3).unwrap();

        assert_eq!(Pos::new(3, 3), min);
        assert_eq!(Pos::new(3, 13), max);

        let (min, _max) = signal.man_to_row(&beacon, 10).unwrap();
        assert_eq!(beacon, min);
    }

    #[test]
    fn test_day15_part1() {
        let pairs = parse_positions(TEST);

        let checked = occupied_in_row(10, &pairs);
        let checked_count = checked.count();
        assert_eq!(26, checked_count);
    }

    #[test]
    fn test_day15_part2() {
        const MAX: PosType = 20;
        let signals = parse_positions(TEST);

        let only = find_empty_pos(MAX, &signals);

        assert_eq!(56000011, calculate_pos_score(&only));
    }
}
