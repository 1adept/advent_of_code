use crate::util;

pub mod day1;
pub mod day2;
pub mod day3;
pub mod day4;
pub mod day5;

pub fn day(day: u16) {
    match day {
        1 => day1::tasks(),
        2 => day2::tasks(),
        3 => day3::tasks(),
        4 => day4::tasks(),
        5 => day5::tasks(),
        // 6 => day6::tasks(),
        // 7 => day7::tasks(),
        // 8 => day8::tasks(),
        // 9 => day9::tasks(),
        // 10 => day10::tasks(),
        // 11 => day11::tasks(),
        // 12 => day12::tasks(),
        // 13 => day13::tasks(),
        // 14 => day14::tasks(),
        // 15 => day15::tasks(),
        // 16 => day16::tasks(),
        // 17 => day17::tasks(),
        // 18 => day18::tasks(),
        // 19 => day19::tasks(),
        // 20 => day20::tasks(),
        // 21 => day21::tasks(),
        // 22 => day22::tasks(),
        // 23 => day23::tasks(),
        // 24 => day24::tasks(),
        _ => unreachable!("Advent only has 24 days")
    }
}

fn load_input(day: u16) -> String {
    util::load_input(2022, day)
}