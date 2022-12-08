pub fn tasks() {
    let input = super::load_input(8);
    let data = read_data(&input);

    let vis = count_visible_trees(&data);
    println!("Visible trees from outside: {vis}");

    let far = far_sight(&data);
    println!("Farest sight: {far}");
}

fn far_sight(grid: &Vec<Vec<u8>>) -> usize {
    grid.iter()
        .enumerate()
        .flat_map(|(x, row)| {
            row.into_iter()
                .enumerate()
                .map(|(y, _)| (x, y))
                .collect::<Vec<_>>()
        })
        .map(|(x, y)| dist(&grid, x, y))
        .max()
        .unwrap()
}

fn count_visible_trees(grid: &Vec<Vec<u8>>) -> usize {
    let mut trees_visible = 0;
    for (y, col) in grid.iter().enumerate() {
        for (x, _char) in col.iter().enumerate() {
            if tree_visible(&grid, x, y) {
                trees_visible += 1;
            }
        }
    }
    trees_visible
}

fn grid_get(grid: &Vec<Vec<u8>>, x: usize, y: usize) -> u8 {
    *grid.get(y).unwrap().get(x).unwrap()
}

fn dist(grid: &Vec<Vec<u8>>, x: usize, y: usize) -> usize {
    let w = grid.len();
    let h = grid[0].len();

    let tree = grid_get(&grid, x, y);
    let mut d_up = 0;
    let mut d_down = 0;
    let mut d_left = 0;
    let mut d_right = 0;

    const EDGE: usize = 0;

    if y != 0 {
        while y >= d_up + 1 {
            d_up += 1;
            let t = grid_get(&grid, x, y - d_up);
            if t >= tree {
                break;
            }
        }
    } else {
        d_up = EDGE;
    }

    if x != 0 {
        while x >= d_left + 1 {
            d_left += 1;
            let t = grid_get(&grid, x - d_left, y);
            if t >= tree {
                break;
            }
        }
    } else {
        d_left = EDGE;
    }

    if x != w - 1 {
        while x + d_right + 1 < w {
            d_right += 1;
            let t = grid_get(&grid, x + d_right, y);
            if t >= tree {
                break;
            }
        }
    } else {
        d_right = EDGE;
    }

    if y != h - 1 {
        while y + d_down + 1 < h {
            d_down += 1;
            let t = grid_get(&grid, x, y + d_down);
            if t >= tree {
                break;
            }
        }
    } else {
        d_down = EDGE;
    }

    d_up * d_down * d_left * d_right
}

fn tree_visible(grid: &Vec<Vec<u8>>, x: usize, y: usize) -> bool {
    let w = grid.len();
    let h = grid[0].len();
    let in_grid = |col, row| col < h && row < w;

    let tree = grid[y][x];

    let mut up = true;
    let mut down = true;
    let mut left = true;
    let mut right = true;

    let mut d = 0;

    while up || down || left || right {
        d += 1;

        if up && in_grid(y + d, x) {
            up = grid[y + d][x] < tree;
        } else if up {
            return true;
        }

        if down && y as isize - d as isize >= 0 && in_grid(y - d, x) {
            down = grid[y - d][x] < tree;
        } else if down {
            return true;
        }

        if left && (x as isize - d as isize) >= 0 && in_grid(y, x - d) {
            left = grid[y][x - d] < tree;
        } else if left {
            return true;
        }

        if right && in_grid(y, x + d) {
            right = grid[y][x + d] < tree;
        } else if right {
            return true;
        }
    }

    false
}

fn read_data(input: &str) -> Vec<Vec<u8>> {
    input
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| String::from(c).parse::<u8>().unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "\
30373
25512
65332
33549
35390";

    #[test]
    fn test_visibility() {
        let grid = read_data(&INPUT);

        let check_vis_at = |x, y| tree_visible(&grid, x, y);
        assert!(check_vis_at(0, 0));
        assert!(check_vis_at(0, 1));
        assert!(check_vis_at(0, 2));
        assert!(check_vis_at(0, 3));
        assert!(check_vis_at(0, 4));
        assert!(check_vis_at(1, 0));
        assert!(check_vis_at(1, 1));
        assert!(check_vis_at(1, 2));
        assert!(check_vis_at(1, 3) == false);
        assert!(check_vis_at(1, 4));
        assert!(check_vis_at(2, 0));
        assert!(check_vis_at(2, 1));
        assert!(check_vis_at(2, 2) == false);
        assert!(check_vis_at(2, 3));
        assert!(check_vis_at(2, 4));
        assert!(check_vis_at(3, 0));
        assert!(check_vis_at(3, 1) == false);
        assert!(check_vis_at(3, 2));
        assert!(check_vis_at(3, 3) == false);
        assert!(check_vis_at(3, 4));
        assert!(check_vis_at(4, 0));
        assert!(check_vis_at(4, 1));
        assert!(check_vis_at(4, 2));
        assert!(check_vis_at(4, 3));
        assert!(check_vis_at(4, 4));
    }

    #[test]
    fn test_part1() {
        let grid = read_data(&INPUT);

        assert_eq!(21, count_visible_trees(&grid));
    }

    #[test]
    fn test_part2() {
        let grid = read_data(&INPUT);

        // assert_eq!(4, dist(&grid, 0, 0));
        assert_eq!(4, dist(&grid, 2, 1));
        assert_eq!(8, dist(&grid, 2, 3));
        assert_eq!(8, far_sight(&grid));
    }
}
