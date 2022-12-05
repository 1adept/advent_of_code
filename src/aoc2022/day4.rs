use crate::util;

pub fn tasks() {
    let assignments = get_elf_assignments();

    println!("There are {} assignments", assignments.len());

    let assignments_overlap = assignments
        .iter()
        .filter(|assignment| assignment.dublicate_assignments())
        .count();
    println!("Overlaps in assignements for every elf: {assignments_overlap}");

    let overlapping_pairs = assignments.iter().filter(|ass| ass.any_overlap()).count();
    println!("Num Overlapping pairs: {overlapping_pairs}");
}

/// Doing the whole assignment in one iter
pub fn tasks_iter() {
    let (p1, p2) = util::read_file("./input/2022/day4.txt")
        .lines()
        .into_iter()
        .map(|line| {
            let mut iter = line
                .splitn(4, |c| c == '-' || c == ',')
                .into_iter()
                .map(|i| i.parse::<usize>().unwrap());
            (
                (iter.next().unwrap(), iter.next().unwrap()),
                (iter.next().unwrap(), iter.next().unwrap()),
            )
        })
        .map(|(a, b)| {
            let fully_contains = |l: (usize, usize), r: (usize, usize)| l.0 >= r.0 && l.1 <= r.1;
            let any_contained = |l: (usize, usize), r: (usize, usize)| {
                l.0 >= r.0 && l.0 <= r.1 || l.1 >= r.0 && l.1 <= r.1
            };
            (
                fully_contains(a, b) || fully_contains(b, a),
                any_contained(a, b) || any_contained(b, a),
            )
        })
        .map(|(b1, b2)| (b1 as usize, b2 as usize))
        .reduce(|(p1, p2), (r1, r2)| (p1 + r1, p2 + r2))
        .unwrap();
    println!("Result for Part1: {p1}");
    println!("Result for Part2: {p2}");
}

fn get_elf_assignments() -> Vec<ElfAssignment> {
    util::read_file("./input/2022/day4.txt")
        .lines()
        .into_iter()
        .map(|line| ElfAssignment::from(line))
        .collect()
}

#[derive(Debug)]
struct ElfAssignment {
    first: Assignment,
    second: Assignment,
}

impl ElfAssignment {
    /// Checks if one assignment completly covers the other one
    fn dublicate_assignments(&self) -> bool {
        self.first.contains(&self.second) || self.second.contains(&self.first)
    }

    fn any_overlap(&self) -> bool {
        self.first.any_dublicate(&self.second) || self.second.any_dublicate(&self.first)
    }
}

impl From<&str> for ElfAssignment {
    fn from(str: &str) -> Self {
        let mut split = str.split(",").into_iter();
        ElfAssignment {
            first: Assignment::from(split.next().unwrap()),
            second: Assignment::from(split.next().unwrap()),
        }
    }
}

#[derive(Debug)]
struct Assignment {
    range: (u8, u8),
}

impl Assignment {
    fn contains(&self, other: &Assignment) -> bool {
        let (min, max) = &self.range;
        let (omin, omax) = &other.range;
        omin >= min && omax <= max && omin <= max && omax >= min
    }

    fn any_dublicate(&self, other: &Assignment) -> bool {
        let (a, b) = self.range;
        let (c, d) = other.range;
        let o = c..=d;
        for section in a..=b {
            if o.contains(&section) {
                return true;
            }
        }
        false
    }
}

impl From<&str> for Assignment {
    fn from(str: &str) -> Self {
        let mut split = str.split("-").into_iter();
        let from = split.next().unwrap();
        let to = split.next().unwrap();
        Assignment {
            range: (from.parse().unwrap(), to.parse().unwrap()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_assignment((a, b): (u8, u8), (c, d): (u8, u8)) -> ElfAssignment {
        ElfAssignment {
            first: Assignment { range: (a, b) },
            second: Assignment { range: (c, d) },
        }
    }
}
