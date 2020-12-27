use itertools::Itertools;
use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::hash::Hash;

#[derive(PartialEq, Debug, Clone)]
enum Cell {
    Inactive,
    Active,
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
struct Point3D {
    x: isize,
    y: isize,
    z: isize,
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
struct Point4D {
    x: isize,
    y: isize,
    z: isize,
    w: isize,
}

#[derive(PartialEq, Debug, Clone)]
struct PocketState3D {
    min_x: isize,
    max_x: isize,
    min_y: isize,
    max_y: isize,
    min_z: isize,
    max_z: isize,
    cells: HashMap<Point3D, Cell>,
}

#[derive(PartialEq, Debug, Clone)]
struct PocketState4D {
    min_x: isize,
    max_x: isize,
    min_y: isize,
    max_y: isize,
    min_z: isize,
    max_z: isize,
    min_w: isize,
    max_w: isize,
    cells: HashMap<Point4D, Cell>,
}

impl PocketState3D {
    fn new(cells: Vec<Vec<Cell>>) -> PocketState3D {
        PocketState3D {
            min_x: 0,
            max_x: (cells[0].len() - 1) as isize,
            min_y: 0,
            max_y: (cells.len() - 1) as isize,
            min_z: 0,
            max_z: 0,
            cells: cells
                .iter()
                .enumerate()
                .map(|(i, vec)| {
                    vec.iter().enumerate().map(move |(j, cell)| {
                        (
                            Point3D {
                                x: j as isize,
                                y: i as isize,
                                z: 0,
                            },
                            cell.clone(),
                        )
                    })
                })
                .flatten()
                .collect(),
        }
    }
}

peg::parser! {
    grammar parser() for str {
        rule cell() -> Cell
          = c:$(['.' | '#']) {
            match c {
                "." => Cell::Inactive,
                "#" => Cell::Active,
                _ => unreachable!(),
            }
        }

        pub(crate) rule parse() -> PocketState3D
          = cells:((cell()+)**"\n") {
            PocketState3D::new(cells)
        }
    }
}

trait AssociatedType {
    type Assoc;
    fn produce(&self) -> Self::Assoc;
}

trait Neighbors: Sized {
    fn neighbors(&self) -> Box<dyn Iterator<Item = Self> + '_>;

    fn new_cell_state(&self, cells: &HashMap<Self, Cell>) -> Cell
    where
        Self: Eq + Hash,
    {
        let num_active_neighbors = self
            .neighbors()
            .filter(|p| {
                if let Some(cell) = cells.get(p) {
                    cell == &Cell::Active
                } else {
                    false
                }
            })
            .count();
        let cur_cell = cells.get(&self).unwrap_or(&Cell::Inactive);
        match (cur_cell, num_active_neighbors) {
            (Cell::Active, 2) => Cell::Active,
            (Cell::Active, 3) => Cell::Active,
            (Cell::Inactive, 3) => Cell::Active,
            _ => Cell::Inactive,
        }
    }
}

impl Neighbors for Point3D {
    fn neighbors(&self) -> Box<dyn Iterator<Item = Point3D> + '_> {
        Box::new(
            (-1..=1)
                .cartesian_product(-1..=1)
                .cartesian_product(-1..=1)
                .filter(|((i, j), k)| (*i != 0) || (*j != 0) || (*k != 0))
                .map(move |((i, j), k)| Point3D {
                    x: self.x + i,
                    y: self.y + j,
                    z: self.z + k,
                }),
        )
    }
}

impl Neighbors for Point4D {
    fn neighbors(&self) -> Box<dyn Iterator<Item = Point4D> + '_> {
        Box::new(
            (-1..=1)
                .cartesian_product(-1..=1)
                .cartesian_product(-1..=1)
                .cartesian_product(-1..=1)
                .filter(|(((i, j), k), l)| (*i != 0) || (*j != 0) || (*k != 0) || (*l != 0))
                .map(move |(((i, j), k), l)| Point4D {
                    x: self.x + i,
                    y: self.y + j,
                    z: self.z + k,
                    w: self.w + l,
                }),
        )
    }
}

impl PocketState3D {
    fn advance_step(&mut self) {
        self.min_x -= 1;
        self.max_x += 1;
        self.min_y -= 1;
        self.max_y += 1;
        self.min_z -= 1;
        self.max_z += 1;
        self.cells = (self.min_x..=self.max_x)
            .cartesian_product(self.min_y..=self.max_y)
            .cartesian_product(self.min_z..=self.max_z)
            .map(|((x, y), z)| {
                let point = Point3D { x, y, z };
                let new_cell = point.new_cell_state(&self.cells);
                (point, new_cell)
            })
            .collect();
    }
}

impl PocketState4D {
    fn from3d(state3d: PocketState3D) -> PocketState4D {
        PocketState4D {
            min_x: state3d.min_x,
            max_x: state3d.max_x,
            min_y: state3d.min_y,
            max_y: state3d.max_y,
            min_z: state3d.min_z,
            max_z: state3d.max_z,
            min_w: 0,
            max_w: 0,
            cells: state3d
                .cells
                .iter()
                .map(|(Point3D { x, y, z }, cell)| {
                    (
                        Point4D {
                            x: *x,
                            y: *y,
                            z: *z,
                            w: 0,
                        },
                        cell.clone(),
                    )
                })
                .collect(),
        }
    }

    fn advance_step(&mut self) {
        self.min_x -= 1;
        self.max_x += 1;
        self.min_y -= 1;
        self.max_y += 1;
        self.min_z -= 1;
        self.max_z += 1;
        self.min_w -= 1;
        self.max_w += 1;
        self.cells = (self.min_x..=self.max_x)
            .cartesian_product(self.min_y..=self.max_y)
            .cartesian_product(self.min_z..=self.max_z)
            .cartesian_product(self.min_w..=self.max_w)
            .map(|(((x, y), z), w)| {
                let point = Point4D { x, y, z, w };
                let new_cell = point.new_cell_state(&self.cells);
                (point, new_cell)
            })
            .collect();
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let mut init_state = parser::parse(&fs::read_to_string("in.txt")?[..])?;
    let mut init_state_4d = PocketState4D::from3d(init_state.clone());

    for _ in 0..6 {
        init_state.advance_step()
    }
    println!(
        "Part 1: {}",
        init_state
            .cells
            .iter()
            .filter(|&(_, cell)| cell == &Cell::Active)
            .count()
    );
    for i in 0..6 {
        init_state_4d.advance_step();
        println!(
            "Part 2: After {} iterations: {}",
            i + 1,
            init_state_4d
                .cells
                .iter()
                .filter(|&(_, cell)| cell == &Cell::Active)
                .count()
        );
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn example_data() -> PocketState3D {
        PocketState3D {
            min_x: 0,
            max_x: 2,
            min_y: 0,
            max_y: 2,
            min_z: 0,
            max_z: 0,
            cells: [
                ((0, 0, 0), Cell::Inactive),
                ((1, 0, 0), Cell::Active),
                ((2, 0, 0), Cell::Inactive),
                ((0, 1, 0), Cell::Inactive),
                ((1, 1, 0), Cell::Inactive),
                ((2, 1, 0), Cell::Active),
                ((0, 2, 0), Cell::Active),
                ((1, 2, 0), Cell::Active),
                ((2, 2, 0), Cell::Active),
            ]
            .iter()
            .map(|((x, y, z), cell)| {
                (
                    Point3D {
                        x: *x,
                        y: *y,
                        z: *z,
                    },
                    cell.clone(),
                )
            })
            .collect(),
        }
    }

    #[test]
    fn test_parse() {
        assert_eq!(parser::parse(".#.\n..#\n###"), Ok(example_data()));
    }

    #[test]
    fn test_neighbors() {
        let origin = Point3D { x: 0, y: 0, z: 0 };
        let neighbors = origin.neighbors().collect::<Vec<_>>();
        assert_eq!(neighbors.len(), 26);
        assert!(neighbors.iter().all(|tpl| *tpl != origin));
    }

    #[test]
    fn test_advance() {
        let mut init_state = example_data();
        for _ in 0..6 {
            init_state.advance_step()
        }
        assert_eq!(
            init_state
                .cells
                .iter()
                .filter(|&(_, cell)| cell == &Cell::Active)
                .count(),
            112
        );
    }

    #[test]
    fn test_advance4d() {
        let mut init_state = PocketState4D::from3d(example_data());
        for _ in 0..6 {
            init_state.advance_step()
        }
        assert_eq!(
            init_state
                .cells
                .iter()
                .filter(|&(_, cell)| cell == &Cell::Active)
                .count(),
            848
        );
    }
}
