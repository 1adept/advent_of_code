use std::env::{self, Args};

mod aoc2022;
pub mod util;

fn main() {
    let mut args = env::args();
    args.next();

    // args.into_iter()
    //     .enumerate()
    //     .for_each(|(i, d)| println!("{}: {}", i, d));

    // let year = parse_or_request(&mut args, "Please enter a year: ");
    // let day = parse_or_request(&mut args, "Please enter a day: ");
let year = 2022;
let day = 16;
    println!("Executing task for Year: {}; Day: {}", year, day);

    match year {
        2022 => aoc2022::day(day),
        _ => unreachable!("Hasnt happened yet"),
    }
}

fn parse_or_request(args: &mut Args, message: &str) -> u16 {
    if let Some(data) = args.next() {
        data
    } else {
        println!("{}", message);
        util::read_line()
    }
    .parse::<u16>()
    .unwrap()
}
