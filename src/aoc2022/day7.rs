pub fn tasks() {
    let root = read_dirs(&super::load_input(7));
    let p1 = part1(&root);
    let p2 = part2(&root);
    println!("Part1: {}", p1);
    println!("Part2: {}", p2);
}

fn part1(root: &Dir) -> usize {
    size_of_dirs(root)
}

fn part2(root: &Dir) -> usize {
    delete_smalles_to_free(root)
}

const SPACE_AVAILABLE: usize = 70_000_000;
const SPACE_TO_CLEAR: usize = 30_000_000;

fn size_of_dirs(root: &Dir) -> usize {
    root.sizes()
        .iter()
        .filter(|&&size| size <= 100_000)
        .sum::<usize>()
}

fn delete_smalles_to_free(root: &Dir) -> usize {
    let size_to_free = SPACE_TO_CLEAR - (SPACE_AVAILABLE - root.size());
    let mut sizes = root
        .sizes()
        .iter()
        .filter(|&&size| size >= size_to_free)
        .cloned()
        .collect::<Vec<usize>>();
    sizes.sort();
    sizes.into_iter().next().unwrap()
}

#[derive(Debug, Default)]
struct Dir {
    sub: Vec<Dir>,
    files: Vec<usize>,
}

impl Dir {
    fn size(&self) -> usize {
        let file_size = self.files.iter().sum::<usize>();
        let dir_size = self.sub.iter().map(|dir| dir.size()).sum::<usize>();
        file_size + dir_size
    }

    fn sizes(&self) -> Vec<usize> {
        self.sub
            .iter()
            .flat_map(|dir| dir.sizes())
            .chain([self.size()])
            .collect::<Vec<usize>>()
    }
}

fn read_dirs(input: &str) -> Dir {
    let mut dir_stack = vec![Dir::default()];

    for line in input.lines() {
        // println!("Line: '{}'", line);
        match line {
            "$ ls" => (),
            "$ cd /" => (),
            "$ cd .." => {
                let dir = dir_stack.pop().unwrap();
                dir_stack.last_mut().unwrap().sub.push(dir);
            }
            line if line.starts_with("$ cd") => {
                dir_stack.push(Dir::default());
            }
            line if line.starts_with("dir ") => (),
            line => {
                let file = line
                    .split_once(' ')
                    .map(|(size, _name)| size.parse::<usize>().unwrap())
                    .unwrap();
                dir_stack.last_mut().unwrap().files.push(file);
            }
        }
    }
    while dir_stack.len() > 1 {
        let dir = dir_stack.pop().unwrap();
        dir_stack.last_mut().unwrap().sub.push(dir);
    }

    dir_stack.into_iter().next().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k";

    #[test]
    fn test_p1() {
        assert_eq!(95437, size_of_dirs(&read_dirs(&INPUT)));
    }

    #[test]
    fn test_p2() {
        assert_eq!(24933642, delete_smalles_to_free(&read_dirs(&INPUT)));
    }
}
