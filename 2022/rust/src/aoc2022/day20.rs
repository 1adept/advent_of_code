pub fn tasks() {
    let input = super::load_input(20);
    let elems = parse(&input);

    let pos1 = decode_coordinate(elems.clone(), 1, 1);
    println!("Grove is at {pos1}");

    let pos2 = decode_coordinate(elems, 811589153, 10);
    println!("Grove is at {pos2}");
}

type Element = (usize, isize);

fn parse(input: &str) -> Vec<Element> {
    input
        .lines()
        .map(|line| {
            line.parse::<isize>()
                .unwrap_or_else(|e| panic!("Cannot parse {e}"))
        })
        .enumerate()
        .collect::<Vec<_>>()
}

fn decode_coordinate(mut coords: Vec<Element>, key: isize, rounds: usize) -> isize {
    coords.iter_mut().for_each(|e| e.1 *= key);

    for _round in 0..rounds {
        for e in 0..coords.len() {
            // The first non-mixed number will always be the next from the original list due to all numbers being moved forward being already mixed
            let index = coords.iter().position(|i| i.0 == e).unwrap();
            let value = coords[index].1;

            let new_index = (index as isize + value).rem_euclid(coords.len() as isize - 1);

            let temp = coords.remove(index);
            coords.insert(new_index as usize, temp);
        }
    }
    let zero = coords.iter().position(|i| i.1 == 0).unwrap();

    let mut decoded = 0;
    for key in [1000, 2000, 3000] {
        decoded += coords[(zero + key) % coords.len()].1;
    }
    decoded
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_day20_part1() {
        let elem = parse(TEST);

        let pos = decode_coordinate(elem, 1, 1);
        assert_eq!(3, pos);
    }

    #[test]
    fn test_day20_part2() {
        let elems = parse(TEST);

        let pos = decode_coordinate(elems, 811589153, 10);
        assert_eq!(1623178306, pos);
    }

    const TEST: &str = "\
1
2
-3
3
-2
0
4";
}
