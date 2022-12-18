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

    let planned_moves = super::load_input(2)
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

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug)]
enum RPS {
    Rock,
    Paper,
    Scissors,
}

impl From<&str> for RPS {
    fn from(str: &str) -> Self {
        match str {
            "A" => RPS::Rock,
            "B" => RPS::Paper,
            "C" => RPS::Scissors,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
enum Result {
    Win,
    Loss,
    Draw,
}

impl From<&str> for Result {
    fn from(str: &str) -> Self {
        match str {
            "X" => Result::Loss,
            "Y" => Result::Draw,
            "Z" => Result::Win,
            _ => unreachable!(),
        }
    }
}

impl Result {
    fn score(&self) -> u8 {
        match self {
            Result::Win => 6,
            Result::Loss => 0,
            Result::Draw => 3,
        }
    }
}

impl RPS {
    fn counter_for_result(&self, result: Result) -> RPS {
        let result_slide = match result {
            Result::Win => 1,
            Result::Loss => -1,
            Result::Draw => 0,
        };

        let index = match self {
            RPS::Rock => 0,
            RPS::Paper => 1,
            RPS::Scissors => 2,
        };
        let new_index = ((3 + index + result_slide) % 3) as usize;
        match new_index {
            0 => RPS::Rock,
            1 => RPS::Paper,
            2 => RPS::Scissors,
            _ => unreachable!(),
        }
    }

    fn score_against(&self, other: RPS) -> u8 {
        let result = match (self, other) {
            (RPS::Rock, RPS::Rock) => Result::Draw,
            (RPS::Rock, RPS::Paper) => Result::Loss,
            (RPS::Rock, RPS::Scissors) => Result::Win,
            (RPS::Paper, RPS::Rock) => Result::Win,
            (RPS::Paper, RPS::Paper) => Result::Draw,
            (RPS::Paper, RPS::Scissors) => Result::Loss,
            (RPS::Scissors, RPS::Rock) => Result::Loss,
            (RPS::Scissors, RPS::Paper) => Result::Win,
            (RPS::Scissors, RPS::Scissors) => Result::Draw,
        };

        result.score() + self.score()
    }

    fn score(&self) -> u8 {
        match self {
            RPS::Rock => 1,
            RPS::Paper => 2,
            RPS::Scissors => 3,
        }
    }
}
