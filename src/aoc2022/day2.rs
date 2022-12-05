use crate::util;

pub fn tasks() {
    // Part 1
    // let max_score = INPUT
    //     .lines()
    //     .into_iter()
    //     .filter(|s| !s.is_empty())
    //     .map(|str| str.split_at(1))
    //     .map(|(l, r)| (RPS::from(l), RPS::from(r.trim_start())))
    //     .map(|(l, r)| r.score_against(l) as u128)
    //     .sum::<u128>();

    let planned_moves = util::read_file("./input/2022/day2.txt")
        .lines()
        .into_iter()
        .filter(|s| !s.is_empty())
        .map(|str| str.split_at(1))
        .map(|(l, r)| (RPS::from(l), Result::from(r.trim_start())))
        .map(|(l, r)| {
            let plan = l.counter_for_result(r);
            (l, plan)
        })
        .map(|(l, r)| r.score_against(l) as u128)
        .sum::<u128>();
    println!("With planned moves: result is: {planned_moves}");
}

#[derive(Debug)]
enum RPS {
    ROCK,
    PAPER,
    SCISSORS,
}

impl From<&str> for RPS {
    fn from(str: &str) -> Self {
        match str {
            "A" => RPS::ROCK,
            "B" => RPS::PAPER,
            "C" => RPS::SCISSORS,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
enum Result {
    WIN,
    LOSS,
    DRAW,
}

impl From<&str> for Result {
    fn from(str: &str) -> Self {
        match str {
            "X" => Result::LOSS,
            "Y" => Result::DRAW,
            "Z" => Result::WIN,
            _ => unreachable!(),
        }
    }
}

impl Result {
    fn score(&self) -> u8 {
        match self {
            Result::WIN => 6,
            Result::LOSS => 0,
            Result::DRAW => 3,
        }
    }
}

impl RPS {
    fn counter_for_result(&self, result: Result) -> RPS {
        let result_slide = match result {
            Result::WIN => 1,
            Result::LOSS => -1,
            Result::DRAW => 0,
        };

        let index = match self {
            RPS::ROCK => 0,
            RPS::PAPER => 1,
            RPS::SCISSORS => 2,
        };
        let new_index = ((3 + index + result_slide) % 3) as usize;
        match new_index {
            0 => RPS::ROCK,
            1 => RPS::PAPER,
            2 => RPS::SCISSORS,
            _ => unreachable!(),
        }
    }

    fn score_against(&self, other: RPS) -> u8 {
        let result = match (self, other) {
            (RPS::ROCK, RPS::ROCK) => Result::DRAW,
            (RPS::ROCK, RPS::PAPER) => Result::LOSS,
            (RPS::ROCK, RPS::SCISSORS) => Result::WIN,
            (RPS::PAPER, RPS::ROCK) => Result::WIN,
            (RPS::PAPER, RPS::PAPER) => Result::DRAW,
            (RPS::PAPER, RPS::SCISSORS) => Result::LOSS,
            (RPS::SCISSORS, RPS::ROCK) => Result::LOSS,
            (RPS::SCISSORS, RPS::PAPER) => Result::WIN,
            (RPS::SCISSORS, RPS::SCISSORS) => Result::DRAW,
        };

        result.score() + self.score()
    }

    fn score(&self) -> u8 {
        match self {
            RPS::ROCK => 1,
            RPS::PAPER => 2,
            RPS::SCISSORS => 3,
        }
    }
}