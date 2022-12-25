use std::collections::BTreeSet;

pub fn tasks() {
    let input = super::load_input(18);
    let vecs = parse_input(&input);

    let obsidian = count_open_faces(&vecs);
    println!("Open faces: {obsidian}");

    println!("{} faces exposed to air", flood(&vecs));
}

fn flood(lava: &[Vector]) -> u32 {
    let lava = BTreeSet::from_iter(lava.iter());
    let mut water = BTreeSet::new();

    fn is_contained(v: &Vector) -> bool {
        v.x >= 0 && v.x <= 26 && v.y >= 0 && v.y <= 26 && v.z >= 0 && v.z <= 26
    }

    let mut points = vec![Vector { x: 0, y: 0, z: 0 }];
    let mut result = 0;
    while let Some(point) = points.pop() {
        for adj in point.get_adjacent().iter().filter(|v| is_contained(v)) {
            if lava.contains(&adj) {
                // Found lava face
                result += 1;
            } else if !water.contains(adj) {
                // Continue flood here
                water.insert(*adj);
                points.push(*adj);
            }
        }
    }
    result
}

fn count_open_faces(vecs: &[Vector]) -> usize {
    let mut assume = vecs.len() * (2 * 3);

    for i in 0..vecs.len() {
        let v1 = vecs[i];
        for v2 in vecs.iter().skip(i + 1) {
            if v1.touches(v2) {
                assume -= 2;
            }
        }
    }
    assume
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Vector {
    x: i8,
    y: i8,
    z: i8,
}

impl Vector {
    fn touches(&self, other: &Vector) -> bool {
        let dx = self.x.abs_diff(other.x);
        let dy = self.y.abs_diff(other.y);
        let dz = self.z.abs_diff(other.z);

        1 >= (dx + dy + dz)
    }

    fn get_adjacent(&self) -> [Vector; 6] {
        let v = |x, y, z| Vector { x, y, z };
        [
            v(self.x, self.y + 1, self.z),
            v(self.x, self.y - 1, self.z),
            v(self.x - 1, self.y, self.z),
            v(self.x + 1, self.y, self.z),
            v(self.x, self.y, self.z + 1),
            v(self.x, self.y, self.z - 1),
        ]
    }
}

fn parse_input(input: &str) -> Vec<Vector> {
    let mut vecs = Vec::new();
    for line in input.lines() {
        let [x, y, z] = line
            .split(',')
            // 1 gap on the edge for part2 bc we need to go 'around' the coordinates and this way we can stay in the positive range
            .map(|num| 1 + num.parse::<i8>().unwrap_or_else(|_| panic!("{num} is not a number")))
            .collect::<Vec<_>>()[..]
        else{ panic!("Error parsing vector in line {line}"); };
        vecs.push(Vector { x, y, z });
    }
    vecs
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore = "No test"]
    fn test_day18_input() {
        let input = super::super::load_input(18);
        let vecs = parse_input(&input);

        let faces = count_open_faces(&vecs);
        println!("Open faces: {faces}");

        let flood = flood(&vecs);
        println!("Flood {flood}");
    }

    #[test]
    fn test_day18_part1() {
        let vecs = parse_input(TEST);
        assert_eq!(64, count_open_faces(&vecs));
    }

    #[test]
    fn test_day18_part2() {
        let vecs = parse_input(TEST);
        assert_eq!(58, flood(&vecs));
    }

    const TEST: &str = "\
2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5";
}
