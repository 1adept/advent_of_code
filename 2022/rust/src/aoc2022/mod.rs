use crate::util;

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day20;
mod day21;
mod day22;

#[allow(clippy::zero_prefixed_literal)]
pub fn day(day: u16) {
    match day {
        01 => day01::tasks(),
        02 => day02::tasks(),
        03 => day03::tasks(),
        04 => day04::tasks(),
        05 => day05::tasks(),
        06 => day06::tasks(),
        07 => day07::tasks(),
        08 => day08::tasks(),
        09 => day09::tasks(),
        10 => day10::tasks(),
        11 => day11::tasks(),
        12 => day12::tasks(),
        13 => day13::tasks(),
        14 => day14::tasks(),
        15 => day15::tasks(),
        16 => day16::tasks(),
        17 => day17::tasks(),
        18 => day18::tasks(),
        // 19 => day19::tasks(),
        20 => day20::tasks(),
        21 => day21::tasks(),
        22 => day22::tasks(),
        // 23 => day23::tasks(),
        // 24 => day24::tasks(),
        // 25 => day25::tasks(),
        _ => unreachable!("Advent only has 25 days"),
    }
}

fn load_input(day: u16) -> String {
    util::load_input(2022, day)
}
