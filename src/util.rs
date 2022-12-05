use std::{fs, io};

pub fn read_file(path: &str) -> String {
    fs::read_to_string(path).expect("Cant read file")
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
