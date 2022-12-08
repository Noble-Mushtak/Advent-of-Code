use std::collections::HashSet;
use std::error::Error;
use std::fs;

peg::parser! {
    grammar parser() for str {
        rule digit() -> usize
          = n:$(['0'..='9']) {
            n.parse().unwrap()
        }

        rule digits() -> Vec<usize>
          = l:(digit()+) { l }

        pub(crate) rule parse() -> Vec<Vec<usize>>
          = l:(digits()**"\n") _:"\n"? { l }
    }
}

fn add_visibles(grid: &[Vec<usize>], visible: &mut HashSet<(usize, usize)>, points: impl Iterator<Item=(usize, usize)>) {
    points.fold(None, |mx_so_far, (x, y)| {
        if Some(grid[x][y]) > mx_so_far {
            visible.insert((x, y));
            Some(grid[x][y])
        } else {
            mx_so_far
        }
    });
}

fn calc_viewing_distance(grid: &[Vec<usize>], height: usize, mut points: impl Iterator<Item=(usize, usize)>) -> usize {
    match points.try_fold(0, |res, (x, y)| {
        if grid[x][y] >= height {
            Err(res+1)
        } else {
            Ok(res+1)
        }
    }) {
        Ok(val) => val,
        Err(val) => val
    }
}

fn calc_scenic_score(grid: &[Vec<usize>], x: usize, y: usize) -> usize {
    let num_rows = grid.len();
    let num_cols = grid[0].len();
    let cur_height = grid[x][y];
    let dst_left = calc_viewing_distance(grid, cur_height, (0..x).rev().map(|i| (i, y)));
    let dst_right = calc_viewing_distance(grid, cur_height, (x+1..num_rows).map(|i| (i, y)));
    let dst_up = calc_viewing_distance(grid, cur_height, (0..y).rev().map(|j| (x, j)));
    let dst_down = calc_viewing_distance(grid, cur_height, (y+1..num_cols).map(|j| (x, j)));
    dst_left*dst_right*dst_up*dst_down
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let grid = parser::parse(&fs::read_to_string("in.txt")?)?;
    let num_rows = grid.len();
    let num_cols = grid[0].len();

    let mut visible: HashSet<(usize, usize)> = HashSet::new();
    for j in 0..num_cols {
        add_visibles(&grid, &mut visible, (0..num_rows).map(|i| (i, j)));
        add_visibles(&grid, &mut visible, (0..num_rows).rev().map(|i| (i, j)));
    }
    for i in 0..num_rows {
        add_visibles(&grid, &mut visible, (0..num_cols).map(|j| (i, j)));
        add_visibles(&grid, &mut visible, (0..num_cols).rev().map(|j| (i, j)));
    }
    println!("Part 1: {}", visible.len());
    println!("Part 2: {}", (0..num_rows).flat_map(|i| (0..num_cols).map(move |j| (i, j))).map(|(i, j)| calc_scenic_score(&grid, i, j)).max().unwrap());
    Ok(())
}