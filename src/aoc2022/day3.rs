use std::{collections::HashSet, ops::Deref};

pub fn tasks() {
    let rucksacks: Vec<Rucksack> = super::load_input(3)
        .lines()
        .into_iter()
        .map(|line| {
            let compartments = line.split_at(line.len() / 2);
            Rucksack::from(compartments)
        })
        .collect();

    let total_priority = task1(&rucksacks);
    println!("Sum: {total_priority}");

    task2(&rucksacks);
}

fn task1(rucksacks: &Vec<Rucksack>) -> u64 {
    rucksacks
        .iter()
        .map(|r| *r.find_doubled_item() as u64)
        .sum::<u64>()
}

fn task2(rucksacks: &Vec<Rucksack>) {
    let mut badges = rucksacks
        .chunks(3)
        .map(|chunk| {
            let (a, b, c) = (&chunk[0], &chunk[1], &chunk[2]);
            a.common_items(b.common_items(c.all_items()))
        })
        .map(|set| {
            assert!(set.len() == 1);
            set.into_iter().next().unwrap()
        })
        .collect::<Vec<Item>>();

    badges.sort_by_key(|item| item.0);

    let badge_sum = badges.iter().map(|item| **item as u32).sum::<u32>();
    println!("Sum of Badge-Priority: {badge_sum}");
}

struct Rucksack {
    left: Compartment,
    right: Compartment,
}

impl Rucksack {
    fn find_doubled_item(&self) -> Item {
        let left = &self.left.items;
        let right = &self.right.items;

        let intersect = left.intersection(&right).cloned().collect::<Vec<Item>>();
        assert!(
            intersect.len() == 1,
            "Rucksack compartments dont have exactly 1 common item"
        );
        intersect[0].clone()
    }

    fn all_items(&self) -> HashSet<Item> {
        self.left.items.union(&self.right.items).cloned().collect()
    }

    fn common_items(&self, items: HashSet<Item>) -> HashSet<Item> {
        self.all_items().intersection(&items).cloned().collect()
    }
}

impl From<(&str, &str)> for Rucksack {
    fn from(compartments: (&str, &str)) -> Self {
        Rucksack {
            left: Compartment::new(compartments.0.to_string()),
            right: Compartment::new(compartments.1.to_string()),
        }
    }
}

struct Compartment {
    pub items: HashSet<Item>,
}

impl Compartment {
    fn new(items: String) -> Self {
        Self {
            items: items.chars().map(Item::from).collect(),
        }
    }
}

#[derive(Debug, Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Item(pub u8);

impl Deref for Item {
    type Target = u8;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<char> for Item {
    /// Creates item with priority
    fn from(item: char) -> Self {
        let priority = match item {
            'a'..='z' => (item as u8) - 96,
            'A'..='Z' => (item as u8) - 38,
            _ => unreachable!(),
        };
        Item(priority)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::RangeInclusive;

    #[test]
    fn test_item_first_last() {
        assert_eq!(1, Item::from('a').0 as usize);
        assert_eq!(27, Item::from('A').0 as usize);

        assert_eq!(26, Item::from('z').0 as usize);
        assert_eq!(52, Item::from('Z').0 as usize);
    }

    #[test]
    fn test_item_from_char_all() {
        let test_row = |chars: RangeInclusive<char>, start: usize| {
            for (index, c) in chars.enumerate() {
                assert_eq!(start + index, Item::from(c).0 as usize);
            }
        };

        test_row('a'..='z', 1);
        test_row('A'..='Z', 27);
    }
}
