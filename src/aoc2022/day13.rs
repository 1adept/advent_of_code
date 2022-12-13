use std::{cmp::Ordering, fmt::Display};

use nom::{
    branch::alt,
    character::complete::{self},
    combinator::{map},
    multi::{separated_list0},
    sequence::{delimited},
    Finish, IResult,
};

pub fn tasks() {
    let input = &super::load_input(13);
    let packages = &mut read_packets(input);

    let sum = count_ordered_packages(packages);
    println!("Ordered Packages = {sum}");

    let prod = divider_index_prod(packages);
    println!("Divider product: {prod}");
}

fn divider_index_prod(packages: &mut Vec<PackageEntry>) -> usize {
    let [two, six] = &*read_packets("[[2]]\n[[6]]") else { unreachable!() };
    packages.extend([two.clone(), six.clone()]);

    packages.sort_unstable();

    packages
        .iter()
        .enumerate()
        .filter_map(|(i, p)| (p == two || p == six).then_some(i + 1))
        .product()
}

fn count_ordered_packages(packages: &[PackageEntry]) -> usize {
    packages
        .chunks(2)
        .enumerate()
        .filter_map(|(i, p)| (p[0] < p[1]).then_some(i + 1))
        .sum()
}

fn read_packets(input: &str) -> Vec<PackageEntry> {
    fn parse(input: &str) -> IResult<&str, PackageEntry> {
        alt((
            delimited(
                complete::char('['),
                map(
                    separated_list0(complete::char(','), parse),
                    PackageEntry::List,
                ),
                complete::char(']'),
            ),
            map(complete::i8, PackageEntry::Single),
        ))(input)
    }

    input
        .lines()
        .filter_map(|line| parse(line).finish().map(|x| x.1).ok())
        .collect()
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum PackageEntry {
    Single(i8),
    List(Vec<PackageEntry>),
}

impl PackageEntry {}

impl Display for PackageEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PackageEntry::Single(v) => write!(f, "{v}"),
            PackageEntry::List(l) => write!(f, "{l:?}"),
        }
    }
}

impl PartialOrd for PackageEntry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PackageEntry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (PackageEntry::Single(l), PackageEntry::Single(r)) => l.cmp(r),
            (PackageEntry::List(l), PackageEntry::List(r)) => l.cmp(r),
            (l, PackageEntry::List(r)) => match &**r {
                [r, ..] if l != r => l.cmp(r),
                _ => 1.cmp(&r.len()),
            },
            _ => Ordering::reverse(other.cmp(self)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = "\
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]";

    #[test]
    fn test_13part1() {
        let result = &read_packets(TEST);

        assert_eq!(13, count_ordered_packages(result));
    }

    #[test]
    fn test_13part2() {
        let packages = &mut read_packets(TEST);
        let prod = super::divider_index_prod(packages);

        assert_eq!(140, prod);
    }
}
