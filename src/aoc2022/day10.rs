use std::{str::FromStr, string::ParseError};

pub fn tasks() {
    let input = super::load_input(10);
    let mut instructions = read_instructions(&input);
    let p1 = tick_signal(&mut instructions, 20, 40);

    println!("Signal from {} every {} cycles => {}", 20, 40, p1);
}

/// Part2 gets returned in the terminal
fn tick_signal(instr: &mut [Instruction], start: usize, repeat: usize) -> usize {
    let mut register = Register::new();
    let mut cycle = Cycle::new(repeat);
    let mut cycle_offset = Cycle::new(repeat).with_offset(start);
    let mut cycle_checker = CycleChecker::new();
    let mut crt_row = String::new();

    println!();
    for instr in instr {
        for _tick in 0..instr.cycles() {
            cycle.advance();
            cycle_offset.advance();
            cycle_checker.check(&cycle_offset, &register);

            let pixel = determine_cycle_pixel(&cycle, &register);
            crt_row.push(pixel);
            if cycle.just_completed() {
                println!("{crt_row}");
                crt_row.clear();
            }
        }
        if let Instruction::Add(op) = instr {
            op(&mut register.value);
        }
    }
    println!("{crt_row}");

    cycle_checker
        .signal_str_at
        .iter()
        .enumerate()
        .map(|(_i, signal)| *signal as usize)
        .sum::<usize>()
}

/// Draws a single pixel each cycle
/// If the sprite ('###') is positioned so that its at the same pos as the pixel bein drawn
/// THEN a '#' will be drawn otherwise a '.' will be drawn
fn determine_cycle_pixel(cycle: &Cycle, register: &Register) -> char {
    let cycle = cycle.current_in_cycle() as isize - 1;
    let center = register.value;

    if (cycle - center).abs() <= 1 {
        '#'
    } else {
        '.'
    }
}

struct Register {
    value: isize,
}

impl Register {
    fn new() -> Self {
        Self { value: 1 }
    }
}

#[derive(Debug)]
struct Cycle {
    offset: Option<usize>,
    current: usize,
    cycle: usize,
}

impl Cycle {
    pub fn new(cycle: usize) -> Self {
        Self {
            offset: None,
            current: 0,
            cycle,
        }
    }
    pub fn with_offset(self, offset: usize) -> Self {
        Self {
            offset: Some(offset),
            ..self
        }
    }
    fn just_completed(&self) -> bool {
        if let Some(offset) = self.offset {
            0 == (offset + self.current) % self.cycle
        } else {
            0 == self.current % self.cycle
        }
    }

    pub fn advance(&mut self) {
        self.current += 1;
    }

    fn current_in_cycle(&self) -> usize {
        self.current % self.cycle
    }
}

enum Instruction {
    Noop,
    Add(Box<dyn FnMut(&mut isize)>),
}

impl Instruction {
    fn cycles(&self) -> usize {
        match self {
            Instruction::Noop => 1,
            _ => 2,
        }
    }
}

impl FromStr for Instruction {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let op = match s {
            "noop" => Instruction::Noop,
            s if s.starts_with("add") => {
                // let name = &s[3..4].parse::<char>().expect("Couldnt parse as char");
                let value = s[5..].parse::<isize>().expect("Couldnt parse OPs value");

                let op = Box::new(move |c: &mut isize| *c += value);
                Instruction::Add(op)
            }
            _ => todo!(),
        };
        Ok(op)
    }
}

#[derive(Debug)]
struct CycleChecker {
    signal_str_at: Vec<isize>,
}

impl CycleChecker {
    fn new() -> Self {
        Self {
            signal_str_at: Vec::new(),
        }
    }

    fn check(&mut self, cycle: &Cycle, register: &Register) {
        if cycle.just_completed() {
            let cycle = cycle.current;

            let signal_strength = register.value;
            self.signal_str_at
                .push(cycle as isize * signal_strength as isize);
        }
    }
}

fn read_instructions(input: &str) -> Vec<Instruction> {
    input.lines().flat_map(Instruction::from_str).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let mut instr = read_instructions(&INPUT);

        let sum = tick_signal(&mut instr, 20, 40);
        assert_eq!(13140, sum);
    }

    #[test]
    #[ignore = "Returned to console..."]
    fn test_part2() {}

    const INPUT: &str = "\
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop";
}
