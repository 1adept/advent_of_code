use crate::util;

mod day1;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
// mod day16;
mod day16_2;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;

pub fn day(day: u16) {
    match day {
        1 => day1::tasks(),
        2 => day2::tasks(),
        3 => day3::tasks(),
        4 => day4::tasks(),
        5 => day5::tasks(),
        6 => day6::tasks(),
        7 => day7::tasks(),
        8 => day8::tasks(),
        9 => day9::tasks(),
        10 => day10::tasks(),
        11 => day11::tasks(),
        12 => day12::tasks(),
        13 => day13::tasks(),
        14 => day14::tasks(),
        15 => day15::tasks(),
        16 => day16_2::tasks(),
        // 17 => day17::tasks(),
        // 18 => day18::tasks(),
        // 19 => day19::tasks(),
        // 20 => day20::tasks(),
        // 21 => day21::tasks(),
        // 22 => day22::tasks(),
        // 23 => day23::tasks(),
        // 24 => day24::tasks(),
        // 25 => day25::tasks(),
        _ => unreachable!("Advent only has 25 days"),
    }
}

fn load_input(day: u16) -> String {
    util::load_input(2022, day)
}
