pub fn tasks() {
    let input = super::load_input(6);

    let p1 = task1(&input);
    let p2 = task2(&input);
    println!("Part1: {}\nPart2: {}", p1, p2);
}

fn task1(input: &str) -> String {
    find_marker::<4>(&input)
}

fn task2(input: &str) -> String {
    find_marker::<14>(&input)
}

fn find_marker<const L: usize>(input: &str) -> String {
    let mut char_iter = input.chars().enumerate();
    let mut chars: [char; L] = [' '; L];

    for i in 0..L {
        chars[i] = char_iter.next().unwrap().1;
    }

    for (i, c) in char_iter {
        if chars_unique(&chars) {
            return i.to_string();
        } else {
            chars[i % L] = c;
        }
    }
    panic!("No marker found");
}

fn chars_unique(chars: &[char]) -> bool {
    // itertools distinct() would be really easy solution for this

    chars
        .iter()
        .enumerate()
        .all(|(i, c)| chars.iter().skip(1 + i).all(|c2| c != c2))
}

#[cfg(test)]
mod tests {
    use super::*;

    // Example inputs and solutions
    const INPUT1: &str = "mjqjpqmgbljsphdztnvjfqwrcgsmlb";
    const INPUT2: &str = "bvwbjplbgvbhsrlpgdmjqwftvncz";
    const INPUT3: &str = "nppdvjthqldpwncqszvftbrmjlhg";
    const INPUT4: &str = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg";
    const INPUT5: &str = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw";

    #[test]
    fn test_task1() {
        assert_eq!(7.to_string(), task1(INPUT1));
        assert_eq!(5.to_string(), task1(INPUT2));
        assert_eq!(6.to_string(), task1(INPUT3));
        assert_eq!(10.to_string(), task1(INPUT4));
        assert_eq!(11.to_string(), task1(INPUT5));
    }
    
    #[test]
    fn test_task2(){
        assert_eq!(19.to_string(), task2(INPUT1));
        assert_eq!(23.to_string(), task2(INPUT2));
        assert_eq!(23.to_string(), task2(INPUT3));
        assert_eq!(29.to_string(), task2(INPUT4));
        assert_eq!(26.to_string(), task2(INPUT5));
    }
}
