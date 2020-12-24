use snafu::Snafu;
use std::convert::{TryFrom, TryInto};
use std::error::Error;
use std::fs;
use std::str::FromStr;

#[derive(PartialEq, Debug, Clone)]
enum Cell {
    Floor,
    Empty,
    Occupied,
}

#[derive(PartialEq, Debug, Snafu)]
enum ParseCellError {
    #[snafu(display("\"{}\" does not represent a valid cell", ch))]
    BadCell { ch: char },
}

impl TryFrom<char> for Cell {
    type Error = ParseCellError;

    fn try_from(ch: char) -> Result<Self, Self::Error> {
        match ch {
            '.' => Ok(Cell::Floor),
            'L' => Ok(Cell::Empty),
            '#' => Ok(Cell::Occupied),
            _ => Err(ParseCellError::BadCell { ch }),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
struct Grid(Vec<Vec<Cell>>);

impl FromStr for Grid {
    type Err = ParseCellError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        input
            .trim()
            .split('\n')
            .map(|line| line.chars().map(char::try_into).collect::<Result<_, _>>())
            .collect::<Result<_, _>>()
            .map(Grid)
    }
}

const DIRECTIONS: [(isize, isize); 8] = [
    (1, 0),
    (1, 1),
    (0, 1),
    (-1, 1),
    (-1, 0),
    (-1, -1),
    (0, -1),
    (1, -1),
];

fn try_add(&i: &usize, &dx: &isize) -> Option<usize> {
    ((i as isize) + dx).try_into().ok()
}

impl Grid {
    fn at(&self, &i: &usize, &j: &usize) -> Option<&Cell> {
        self.0.get(i)?.get(j)
    }

    fn num_occupied(&self) -> usize {
        self.0
            .iter()
            .flatten()
            .filter(|&cell| *cell == Cell::Occupied)
            .count()
    }

    fn find_neighbor_part1(
        &self,
        &i: &usize,
        &j: &usize,
        &dx: &isize,
        &dy: &isize,
    ) -> Option<&Cell> {
        let new_i: usize = try_add(&i, &dx)?;
        let new_j: usize = try_add(&j, &dy)?;

        self.at(&new_i, &new_j)
    }

    fn find_neighbor_part2(
        &self,
        &i: &usize,
        &j: &usize,
        &dx: &isize,
        &dy: &isize,
    ) -> Option<&Cell> {
        let mut cur_i = i;
        let mut cur_j = j;
        std::iter::from_fn(move || {
            cur_i = try_add(&cur_i, &dx)?;
            cur_j = try_add(&cur_j, &dy)?;

            self.at(&cur_i, &cur_j)
        })
        .find(|cell| *cell != &Cell::Floor)
    }

    fn num_occupied_neighbors<F>(&self, find_neighbor: &F, &i: &usize, &j: &usize) -> usize
    where
        for<'a> F: Fn(&'a Grid, &usize, &usize, &isize, &isize) -> Option<&'a Cell>,
    {
        DIRECTIONS
            .iter()
            .map(|&(dx, dy)| find_neighbor(&self, &i, &j, &dx, &dy))
            .filter(|res| *res == Some(&Cell::Occupied))
            .count()
    }

    fn advance_step<F>(&mut self, find_neighbor: &F, threshold: usize) -> bool
    where
        for<'a> F: Fn(&'a Grid, &usize, &usize, &isize, &isize) -> Option<&'a Cell>,
    {
        let old_self = self.clone();
        (0..old_self.0.len())
            .map(|i| (0..old_self.0[i].len()).map(move |j| (i, j)))
            .flatten()
            .map(|(i, j)| match old_self.0[i][j] {
                Cell::Empty => {
                    if old_self.num_occupied_neighbors(find_neighbor, &i, &j) == 0 {
                        self.0[i][j] = Cell::Occupied;
                        true
                    } else {
                        false
                    }
                }
                Cell::Occupied => {
                    if old_self.num_occupied_neighbors(find_neighbor, &i, &j) >= threshold {
                        self.0[i][j] = Cell::Empty;
                        true
                    } else {
                        false
                    }
                }
                Cell::Floor => false,
            })
            .collect::<Vec<_>>()
            .iter()
            .any(|&b| b)
    }

    fn advance_step_part1(&mut self) -> bool {
        self.advance_step(&Grid::find_neighbor_part1, 4)
    }

    fn advance_step_part2(&mut self) -> bool {
        self.advance_step(&Grid::find_neighbor_part2, 5)
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let mut grid: Grid = fs::read_to_string("in.txt")?.parse()?;

    let mut grid_part1 = grid.clone();
    while grid_part1.advance_step_part1() {}
    println!("Part 1: {}", grid_part1.num_occupied());

    while grid.advance_step_part2() {}
    println!("Part 2: {}", grid.num_occupied());

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_grid() {
        use Cell::*;
        assert_eq!(
            "L.#\n#.L\n..#\n".parse(),
            Ok(Grid(vec![
                vec![Empty, Floor, Occupied],
                vec![Occupied, Floor, Empty],
                vec![Floor, Floor, Occupied]
            ]))
        );
    }

    fn example_grid1() -> Grid {
        "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"
            .parse()
            .unwrap()
    }

    fn example_grid2() -> Grid {
        "#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##"
            .parse()
            .unwrap()
    }

    fn example_grid3() -> Grid {
        "#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##"
            .parse()
            .unwrap()
    }

    #[test]
    fn test_advance_grid_p1() {
        let mut grid1 = example_grid1();
        assert!(grid1.advance_step_part1());
        assert_eq!(grid1, example_grid2());
        assert!(grid1.advance_step_part1());
        assert_eq!(grid1, example_grid3());
        assert!(grid1.advance_step_part1());
        assert!(grid1.advance_step_part1());
        assert!(grid1.advance_step_part1());
        assert!(!grid1.advance_step_part1());
    }

    fn example_grid4() -> Grid {
        "#.LL.LL.L#
#LLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLLL.L
#.LLLLL.L#"
            .parse()
            .unwrap()
    }

    fn example_grid5() -> Grid {
        "#.L#.##.L#
#L#####.LL
L.#.#..#..
##L#.##.##
#.##.#L.##
#.#####.#L
..#.#.....
LLL####LL#
#.L#####.L
#.L####.L#"
            .parse()
            .unwrap()
    }

    #[test]
    fn test_advance_grid_p2() {
        let mut grid1 = example_grid1();
        assert!(grid1.advance_step_part2());
        assert_eq!(grid1, example_grid2());
        assert!(grid1.advance_step_part2());
        assert_eq!(grid1, example_grid4());
        assert!(grid1.advance_step_part2());
        assert_eq!(grid1, example_grid5());
        assert!(grid1.advance_step_part2());
        assert!(grid1.advance_step_part2());
        assert!(grid1.advance_step_part2());
        assert!(!grid1.advance_step_part2());
    }
}
