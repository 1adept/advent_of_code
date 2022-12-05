use std::collections::LinkedList;

const NUM_STACKS: usize = 9;
const STACK_HEIGHT: usize = 8;

pub fn tasks() {
    let input = super::load_input(5);
    read_initial_depot(&input);
    let p1 = task1(&input);
    let p2 = task2(&input);

    println!("Part1 => {}\nPart2 => {}", p1, p2);
}

fn task1(input: &str) -> String {
    let mut depot = read_initial_depot(&input);

    instructions(&input)
        .map(read_instruction)
        .for_each(|ins| depot.execute(ins));

    depot.read_tops()
}

fn task2(input: &str) -> String {
    let mut depot = read_initial_depot(&input);

    instructions(&input)
        .map(read_instruction)
        .for_each(|ins| depot.execute_keep_order(ins));

    depot.read_tops()
}

fn read_instruction(line: &str) -> Move {
    lazy_static::lazy_static! {
        static ref MOVE_REGEX: regex::Regex = regex::Regex::new(r"move (\d{1,2}) from (\d) to (\d)").unwrap();
    }

    let captures = MOVE_REGEX.captures(line).unwrap();
    Move {
        amount: captures[1].parse::<u8>().unwrap(),
        from: captures[2].parse::<usize>().unwrap(),
        to: captures[3].parse::<usize>().unwrap(),
    }
}

fn read_initial_depot(input: &str) -> Depot {
    const START: usize = 1;
    const GAP: usize = 4;

    let mut depot = Depot::default();

    input
        .lines()
        .take(STACK_HEIGHT)
        .collect::<Vec<&str>>()
        .into_iter()
        .rev()
        .map(|line| {
            line.as_bytes()
                .into_iter()
                .skip(START)
                .step_by(GAP)
                .map(|c| *c as char)
                .collect::<Vec<char>>()
        })
        .for_each(|chars| {
            chars
                .into_iter()
                .enumerate()
                .filter(|(_i, c)| !char_empty(*c))
                .for_each(|(i, c)| depot.stacks[i].push(c))
        });

    // println!("{depot:#?}");
    depot
}

fn instructions(input: &str) -> std::iter::Skip<std::str::Lines> {
    input.lines().skip(STACK_HEIGHT + 2) //+2 for stack-index line and empty-seperator line
}

#[derive(Debug)]
struct Move {
    amount: u8,
    from: usize,
    to: usize,
}

#[derive(Debug, Default)]
struct Depot {
    stacks: [Stack; NUM_STACKS],
}

impl Depot {
    fn execute(&mut self, instruction: Move) {
        for _i in 0..instruction.amount {
            let c = self.stacks[instruction.from - 1].pop();
            self.stacks[instruction.to - 1].push(c);
        }
    }

    fn execute_keep_order(&mut self, instruction: Move) {
        let mut crane_arm = Vec::with_capacity(instruction.amount as usize);

        let from = &mut self.stacks[instruction.from - 1];
        for _i in 0..instruction.amount {
            crane_arm.push(from.pop());
        }

        let to = &mut self.stacks[instruction.to - 1];
        crane_arm.into_iter().rev().for_each(|c| to.push(c));
    }

    fn read_tops(&mut self) -> String {
        self.stacks
            .iter_mut()
            .map(|stack| stack.peek())
            .collect::<String>()
    }
}

#[derive(Debug, Default)]
struct Stack {
    crates: LinkedList<char>,
}

impl Stack {
    fn push(&mut self, char: char) {
        self.crates.push_back(char);
    }

    fn pop(&mut self) -> char {
        self.crates.pop_back().unwrap()
    }

    fn peek(&self) -> &char {
        self.crates.back().unwrap()
    }
}

fn char_empty(char: char) -> bool {
    char == ' '
}

struct CrateMover {
    holds: Vec<char>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stack() {
        let mut stack = Stack::default();
        stack.push('a');
        stack.push('b');
        stack.push('c');
        stack.push('d');
        stack.push('e');
        stack.push('f');
        stack.push('g');
        stack.push('h');

        assert_eq!(
            LinkedList::from(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',]),
            stack.crates
        );
        assert_eq!('h', stack.pop());
        assert_eq!('g', stack.pop());
        assert_eq!('f', stack.pop());
    }

    #[test]
    fn test_move() {
        let mut depot = Depot::default();
        {
            let mut first = &mut depot.stacks[0];
            first.push('a');
            first.push('b');
            first.push('c');
        }
        {
            let mut second = &mut depot.stacks[1];
            second.push('z');
            second.push('y');
            second.push('x');
        }

        let move_ins = Move {
            amount: 1,
            from: 2,
            to: 1,
        };

        assert_eq!(LinkedList::from(['a', 'b', 'c']), depot.stacks[0].crates);
        assert_eq!(LinkedList::from(['z', 'y', 'x']), depot.stacks[1].crates);

        depot.execute(move_ins);

        assert_eq!(
            LinkedList::from(['a', 'b', 'c', 'x']),
            depot.stacks[0].crates
        );
        assert_eq!(LinkedList::from(['z', 'y']), depot.stacks[1].crates);

        assert_eq!('y', depot.stacks[1].pop());
    }

    #[test]
    fn test_move_keep_order(){
        let mut depot = Depot::default();
        {
            let mut first = &mut depot.stacks[0];
            first.push('a');
            first.push('b');
            first.push('c');
        }
        {
            let mut second = &mut depot.stacks[1];
            second.push('z');
            second.push('y');
            second.push('x');
        }

        let move_ins = Move {
            amount: 2,
            from: 2,
            to: 1,
        };

        assert_eq!(LinkedList::from(['a', 'b', 'c']), depot.stacks[0].crates);
        assert_eq!(LinkedList::from(['z', 'y', 'x']), depot.stacks[1].crates);

        depot.execute_keep_order(move_ins);

        assert_eq!(
            LinkedList::from(['a', 'b', 'c', 'y', 'x']),
            depot.stacks[0].crates
        );
        assert_eq!(LinkedList::from(['z']), depot.stacks[1].crates);

        assert_eq!('z', depot.stacks[1].pop());
    }
}
