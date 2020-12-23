use snafu::Snafu;
use std::error::Error;
use std::fs;
use std::str::FromStr;

#[derive(PartialEq, Debug)]
enum Cell {
    Empty,
    Tree,
}

#[derive(PartialEq, Debug)]
struct CellGrid(Vec<Vec<Cell>>);

#[derive(PartialEq, Debug, Snafu)]
enum ParseCellError {
    #[snafu(display("\"{}\" does not represent a cell", unknown_char))]
    NotCell { unknown_char: char },
}

impl FromStr for CellGrid {
    type Err = ParseCellError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(CellGrid(
            input
                .trim()
                .split('\n')
                .map(|line| {
                    line.chars()
                        .map(|ch| match ch {
                            '.' => Ok(Cell::Empty),
                            '#' => Ok(Cell::Tree),
                            _ => Err(ParseCellError::NotCell { unknown_char: ch }),
                        })
                        .collect::<Result<_, _>>()
                })
                .collect::<Result<_, _>>()?,
        ))
    }
}

fn count_trees_on_slope(grid: &CellGrid, slope: &(usize, usize)) -> usize {
    grid.0
        .iter()
        .enumerate()
        .filter(|(i, row)| {
            ((i % slope.1) == 0) && row[(slope.0 * (i / slope.1)) % row.len()] == Cell::Tree
        })
        .count()
}

fn calc_answer(grid: &CellGrid) -> usize {
    let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)];
    slopes
        .iter()
        .map(|slope| count_trees_on_slope(&grid, slope))
        .fold(1, std::ops::Mul::mul)
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let grid: CellGrid = fs::read_to_string("in.txt")?.parse()?;

    println!("Part 1: {}", count_trees_on_slope(&grid, &(3, 1)));
    println!("Part 2: {}", calc_answer(&grid));
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn example_grid() -> CellGrid {
        CellGrid(vec![
            vec![Cell::Empty, Cell::Empty],
            vec![Cell::Empty, Cell::Tree],
            vec![Cell::Tree, Cell::Empty],
        ])
    }

    #[test]
    fn test_parse() {
        assert!("dfijg".parse::<CellGrid>().is_err());
        assert!(".....\n...#.\n...s.".parse::<CellGrid>().is_err());
        assert_eq!("..\n.#\n#.".parse::<CellGrid>(), Ok(example_grid()));
    }

    fn example_grid2() -> CellGrid {
        "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"
            .parse()
            .unwrap()
    }

    #[test]
    fn test_count_trees() {
        assert_eq!(count_trees_on_slope(&example_grid(), &(3, 1)), 2);

        assert_eq!(count_trees_on_slope(&example_grid2(), &(1, 1)), 2);
        assert_eq!(count_trees_on_slope(&example_grid2(), &(3, 1)), 7);
        assert_eq!(count_trees_on_slope(&example_grid2(), &(5, 1)), 3);
        assert_eq!(count_trees_on_slope(&example_grid2(), &(7, 1)), 4);
        assert_eq!(count_trees_on_slope(&example_grid2(), &(1, 2)), 2);
    }

    #[test]
    fn test_answer() {
        assert_eq!(calc_answer(&example_grid2()), 336);
    }
}
