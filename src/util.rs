use std::{fmt::format, fs, io};

pub fn load_input(year: u16, day: u16) -> String {
    let path = format!("./input/{}/day{}.txt", year, day);
    fs::read_to_string(path).unwrap()
}

pub fn read_line() -> String {
    let mut str = String::with_capacity(4);

    let stdin = io::stdin();
    if let Ok(_n) = stdin.read_line(&mut str) {
        str.trim()
    } else {
        ""
    }
    .to_owned()
}

