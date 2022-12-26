use std::{
    fs::{self, File},
    io::{self, Read},
};

pub fn load_input(_year: u16, day: u16) -> String {
    let path = format!("../input/day{}.txt", day);
    if let Ok(string) = fs::read_to_string(&path) {
        string
    } else {
        let mut file_content = Vec::new();
        let mut file = File::open(&path).expect("Unable to open file");
        file.read_to_end(&mut file_content)
            .expect("Unable to read file");
        file_content
            .into_iter()
            .map(|c| c as char)
            .collect::<String>()
    }
}

pub fn load_input_as_vec(_year: u16, day: u16) -> Vec<u8> {
    let path = format!("../../input/day{}.txt", day);

    let mut file_content = Vec::new();
    let mut file = File::open(path).expect("Unable to open file");
    file.read_to_end(&mut file_content)
        .expect("Unable to read file");
    file_content
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
