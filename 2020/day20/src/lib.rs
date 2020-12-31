use multimap::MultiMap;
use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::hash::Hash;

#[derive(PartialEq, Debug, Clone, Copy)]
enum Cell {
    Dot,
    Hash,
}

#[derive(PartialEq, Debug, Eq, Hash, Clone, Copy)]
struct TileId(usize);

#[derive(PartialEq, Debug, Clone)]
struct Tile(Vec<Vec<Cell>>);

peg::parser! {
    grammar parser() for str {
        rule usize() -> usize
          = n:$(['0'..='9']+) {
            n.parse().unwrap()
        }

        rule cell() -> Cell
          = c:$(['.' | '#']) {
            match c {
                "." => Cell::Dot,
                "#" => Cell::Hash,
                _ => unreachable!(),
            }
        }

        rule row() -> Vec<Cell>
          = cells:(cell()+) { cells }

        rule tile() -> (TileId, Tile)
          = "Tile " id:usize() ":\n" cells:(row()**"\n") {
            (TileId(id), Tile(cells))
        }

        pub(crate) rule parse() -> HashMap<TileId, Tile>
          = tiles:(tile()**"\n\n") { tiles.into_iter().collect() }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum Direction {
    Top,
    Bottom,
    Left,
    Right,
}

#[derive(PartialEq, Debug, Eq, Hash, Clone, Copy)]
struct BoundaryId(usize);

#[derive(PartialEq, Debug)]
struct BoundaryIds {
    top: BoundaryId,
    bottom: BoundaryId,
    left: BoundaryId,
    right: BoundaryId,
}

fn calc_boundary_num<'a>(iter: impl Iterator<Item = &'a Cell>) -> BoundaryId {
    BoundaryId(iter.fold(0, |res, cell| match *cell {
        Cell::Dot => 2 * res,
        Cell::Hash => 2 * res + 1,
    }))
}

fn calc_boundary_id(lst: &[Cell]) -> BoundaryId {
    BoundaryId(std::cmp::min(
        calc_boundary_num(lst.iter()).0,
        calc_boundary_num(lst.iter().rev()).0,
    ))
}

impl Tile {
    fn boundary_id_on_dir(&self, dir: Direction) -> BoundaryId {
        match dir {
            Direction::Top => calc_boundary_id(&self.0.first().unwrap()[..]),
            Direction::Bottom => calc_boundary_id(&self.0.last().unwrap()[..]),
            Direction::Left => calc_boundary_id(
                &self
                    .0
                    .iter()
                    .map(|row| row.first().unwrap())
                    .copied()
                    .collect::<Vec<_>>()[..],
            ),
            Direction::Right => calc_boundary_id(
                &self
                    .0
                    .iter()
                    .map(|row| row.last().unwrap())
                    .copied()
                    .collect::<Vec<_>>()[..],
            ),
        }
    }

    fn boundary_ids(&self) -> BoundaryIds {
        BoundaryIds {
            top: self.boundary_id_on_dir(Direction::Top),
            bottom: self.boundary_id_on_dir(Direction::Bottom),
            left: self.boundary_id_on_dir(Direction::Left),
            right: self.boundary_id_on_dir(Direction::Right),
        }
    }

    fn rotate(&mut self) {
        if self.0.is_empty() {
            return;
        }
        let length = self.0.len();
        let width = self.0[0].len();

        self.0 = (0..width)
            .map(|i| (0..length).map(|j| self.0[length - j - 1][i]).collect())
            .collect()
    }

    fn flip_rows(&mut self) {
        let size = self.0.len();
        for i in 0..size / 2 {
            let (left, right) = self.0.split_at_mut(i + 1);
            std::mem::swap(&mut left[i], &mut right[size - 2 * i - 2]);
        }
    }

    fn flip_columns(&mut self) {
        let size = self.0.len();
        for i in 0..size {
            for j in 0..size / 2 {
                let (left, right) = self.0[i].split_at_mut(j + 1);
                std::mem::swap(&mut left[j], &mut right[size - 2 * j - 2]);
            }
        }
    }

    fn fix<F, G>(&mut self, dir: Direction, mut check_boundary_id: F, mut change_tile: G)
    where
        F: FnMut(BoundaryId) -> bool,
        G: FnMut(&mut Tile),
    {
        let mut counter = 0;
        while !check_boundary_id(self.boundary_id_on_dir(dir)) {
            change_tile(self);
            counter += 1;
            assert!(counter < 4);
        }
    }
}

#[derive(PartialEq, Debug)]
struct Arrangement(Vec<Vec<(TileId, Tile)>>);

fn find_arrangement(mut tiles: HashMap<TileId, Tile>) -> Arrangement {
    let mut mapping: MultiMap<BoundaryId, TileId> = MultiMap::new();
    let mut boundary_ids_mapping = HashMap::new();
    for (tile_id, tile) in tiles.iter() {
        let boundary_ids = tile.boundary_ids();
        mapping.insert(boundary_ids.top, *tile_id);
        mapping.insert(boundary_ids.bottom, *tile_id);
        mapping.insert(boundary_ids.left, *tile_id);
        mapping.insert(boundary_ids.right, *tile_id);
        boundary_ids_mapping.insert(*tile_id, boundary_ids);
    }

    let is_border_id = |bound_id| mapping.get_vec(&bound_id).unwrap().len() == 1;
    let get_other_tile_with_boundary_id = |given_tile_id, needed_boundary_id| {
        mapping
            .get_vec(&needed_boundary_id)
            .unwrap()
            .iter()
            .find(|&tile_id| *tile_id != given_tile_id)
            .copied()
    };

    let corner_tile_id = tiles
        .iter()
        .find(|(tile_id, _)| {
            let boundary_ids = boundary_ids_mapping.get(&tile_id).unwrap();
            [
                boundary_ids.top,
                boundary_ids.bottom,
                boundary_ids.left,
                boundary_ids.right,
            ]
            .iter()
            .filter(|bound_id| is_border_id(**bound_id))
            .count()
                == 2
        })
        .map(|(tile_id, _)| *tile_id)
        .expect("Failed to find corner tile");
    let mut corner_tile = tiles.remove(&corner_tile_id).unwrap();
    corner_tile.fix(Direction::Left, is_border_id, Tile::rotate);
    corner_tile.fix(Direction::Top, is_border_id, Tile::flip_rows);

    let mut arrangement = vec![vec![(corner_tile_id, corner_tile)]];
    let mut cur_x = 1;
    let mut cur_y = 0;
    while !tiles.is_empty() {
        if cur_x > 0 {
            let tile_to_left = &arrangement[cur_y][cur_x - 1];
            let needed_left_id = tile_to_left.1.boundary_id_on_dir(Direction::Right);
            let cur_tile_id = get_other_tile_with_boundary_id(tile_to_left.0, needed_left_id);
            let cur_tile_id = match cur_tile_id {
                Some(tile_id) => tile_id,
                None => {
                    cur_y += 1;
                    cur_x = 0;
                    arrangement.push(vec![]);
                    continue;
                }
            };
            let mut cur_tile = tiles.remove(&cur_tile_id).unwrap();

            cur_tile.fix(
                Direction::Left,
                |bound_id| bound_id == needed_left_id,
                Tile::rotate,
            );
            if cur_y > 0 {
                let tile_on_top = &arrangement[cur_y - 1][cur_x];
                let needed_top_id = tile_on_top.1.boundary_id_on_dir(Direction::Bottom);
                cur_tile.fix(
                    Direction::Top,
                    |bound_id| bound_id == needed_top_id,
                    Tile::flip_rows,
                );
            } else {
                cur_tile.fix(Direction::Top, is_border_id, Tile::flip_rows);
            }

            arrangement[cur_y].push((cur_tile_id, cur_tile));
        } else {
            assert!(cur_y > 0);

            let tile_on_top = &arrangement[cur_y - 1][cur_x];
            let needed_top_id = tile_on_top.1.boundary_id_on_dir(Direction::Bottom);
            let cur_tile_id = get_other_tile_with_boundary_id(tile_on_top.0, needed_top_id)
                .expect(&format!("Could not find tile with boundary id {}", needed_top_id.0)[..]);
            let mut cur_tile = tiles.remove(&cur_tile_id).unwrap();

            cur_tile.fix(
                Direction::Top,
                |bound_id| bound_id == needed_top_id,
                Tile::rotate,
            );
            cur_tile.fix(Direction::Left, is_border_id, Tile::flip_columns);

            arrangement[cur_y].push((cur_tile_id, cur_tile));
        }
        cur_x += 1;
    }
    Arrangement(arrangement)
}

#[derive(PartialEq, Debug)]
enum MarkedCell {
    Dot,
    UnmarkedHash,
    MarkedHash,
}

#[derive(PartialEq, Debug)]
struct Image(Vec<Vec<MarkedCell>>);

impl Tile {
    fn strip_boundaries(self) -> Self {
        let mut cells = self.0;
        cells.remove(cells.len() - 1);
        cells.remove(0);
        for row in cells.iter_mut() {
            row.remove(row.len() - 1);
            row.remove(0);
        }
        Tile(cells)
    }
}

impl Image {
    fn from_tile(tile: Tile) -> Self {
        Image(
            tile.0
                .into_iter()
                .map(|row| {
                    row.into_iter()
                        .map(|cell| match cell {
                            Cell::Dot => MarkedCell::Dot,
                            Cell::Hash => MarkedCell::UnmarkedHash,
                        })
                        .collect()
                })
                .collect(),
        )
    }

    fn concatenate_horizontal(mut img1: Self, mut img2: Self) -> Self {
        if img1.0.is_empty() {
            img2
        } else {
            for (i, row) in img1.0.iter_mut().enumerate() {
                row.append(&mut img2.0[i]);
            }
            img1
        }
    }

    fn concatenate_vertical(mut img1: Self, mut img2: Self) -> Self {
        img1.0.append(&mut img2.0);
        img1
    }

    fn from_arrangement(arrangement: Arrangement) -> Self {
        arrangement
            .0
            .into_iter()
            .map(|row| {
                row.into_iter()
                    .map(|(_, tile)| Image::from_tile(tile.strip_boundaries()))
                    .fold(Image(vec![]), Image::concatenate_horizontal)
            })
            .fold(Image(vec![]), Image::concatenate_vertical)
    }

    fn tile_pattern_is_at_loc(&self, tile: &Tile, x: usize, y: usize) -> bool {
        tile.0.iter().enumerate().all(|(i, row)| {
            row.iter().enumerate().all(move |(j, cell)| {
                !((cell == &Cell::Hash) && (self.0[x + i][y + j] == MarkedCell::Dot))
            })
        })
    }

    fn mark_hashes_with_tile_at_loc(&mut self, tile: &Tile, x: usize, y: usize) -> bool {
        if self.tile_pattern_is_at_loc(tile, x, y) {
            for (i, row) in tile.0.iter().enumerate() {
                for (j, cell) in row.iter().enumerate() {
                    if cell == &Cell::Hash {
                        self.0[x + i][y + j] = MarkedCell::MarkedHash;
                    }
                }
            }
            true
        } else {
            false
        }
    }

    fn mark_hashes_with_tile_one_pass(&mut self, tile: &Tile) -> bool {
        if self.0.is_empty() {
            return false;
        }
        let self_length = self.0.len();
        let self_width = self.0[0].len();

        if tile.0.is_empty() {
            return false;
        }
        let tile_length = tile.0.len();
        let tile_width = tile.0[0].len();

        let mut at_least_one_loc_worked = false;
        for i in 0..self_length - tile_length + 1 {
            for j in 0..self_width - tile_width + 1 {
                at_least_one_loc_worked |= self.mark_hashes_with_tile_at_loc(tile, i, j);
            }
        }
        at_least_one_loc_worked
    }

    fn mark_hashes_with_tile(&mut self, tile: &mut Tile) {
        if self.mark_hashes_with_tile_one_pass(&tile) {
            return;
        }
        tile.rotate();
        if self.mark_hashes_with_tile_one_pass(&tile) {
            return;
        }
        tile.rotate();
        if self.mark_hashes_with_tile_one_pass(&tile) {
            return;
        }
        tile.rotate();
        if self.mark_hashes_with_tile_one_pass(&tile) {
            return;
        }
        tile.flip_rows();
        if self.mark_hashes_with_tile_one_pass(&tile) {
            return;
        }
        tile.rotate();
        if self.mark_hashes_with_tile_one_pass(&tile) {
            return;
        }
        tile.rotate();
        if self.mark_hashes_with_tile_one_pass(&tile) {
            return;
        }
        tile.rotate();
        self.mark_hashes_with_tile_one_pass(&tile);
    }
}

fn sea_dragon() -> Tile {
    use Cell::*;
    Tile(vec![
        vec![
            Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot, Dot,
            Dot, Hash, Dot,
        ],
        vec![
            Hash, Dot, Dot, Dot, Dot, Hash, Hash, Dot, Dot, Dot, Dot, Hash, Hash, Dot, Dot, Dot,
            Dot, Hash, Hash, Hash,
        ],
        vec![
            Dot, Hash, Dot, Dot, Hash, Dot, Dot, Hash, Dot, Dot, Hash, Dot, Dot, Hash, Dot, Dot,
            Hash, Dot, Dot, Dot,
        ],
    ])
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let tiles = parser::parse(&fs::read_to_string("in.txt")?[..])?;

    let arrangement = find_arrangement(tiles);
    println!(
        "Part 1: {}",
        arrangement.0.first().unwrap().first().unwrap().0 .0
            * arrangement.0.last().unwrap().first().unwrap().0 .0
            * arrangement.0.first().unwrap().last().unwrap().0 .0
            * arrangement.0.last().unwrap().last().unwrap().0 .0
    );

    let mut img = Image::from_arrangement(arrangement);
    img.mark_hashes_with_tile(&mut sea_dragon());
    println!(
        "Part 2: {}",
        img.0
            .into_iter()
            .flatten()
            .filter(|cell| cell == &MarkedCell::UnmarkedHash)
            .count()
    );

    Ok(())
}

impl Image {
    fn draw(&self) -> String {
        self.0
            .iter()
            .map(|row| {
                row.iter()
                    .map(|cell| match cell {
                        MarkedCell::Dot => '.',
                        MarkedCell::UnmarkedHash => '#',
                        MarkedCell::MarkedHash => 'O',
                    })
                    .collect::<String>()
            })
            .fold(String::new(), |acc, row_str| acc + &row_str[..] + "\n")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    fn example_data() -> HashMap<TileId, Tile> {
        parser::parse(
            "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...",
        )
        .unwrap()
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            example_data()
                .into_iter()
                .map(|(tile_id, _)| tile_id)
                .collect::<HashSet<_>>(),
            vec![
                TileId(2311),
                TileId(1951),
                TileId(1171),
                TileId(1427),
                TileId(1489),
                TileId(2473),
                TileId(2971),
                TileId(2729),
                TileId(3079)
            ]
            .into_iter()
            .collect::<HashSet<_>>()
        );
    }

    #[test]
    fn test_ids() {
        assert_eq!(
            example_data()[&TileId(2311)].boundary_ids(),
            BoundaryIds {
                top: BoundaryId(210),
                bottom: BoundaryId(231),
                left: BoundaryId(318),
                right: BoundaryId(89)
            }
        );
        assert_eq!(
            example_data()[&TileId(1427)].boundary_ids().bottom,
            example_data()[&TileId(2311)].boundary_ids().top
        );
    }

    #[test]
    fn test_rotate_and_flip() {
        let mut tile = example_data()[&TileId(2311)].clone();
        tile.rotate();
        assert_eq!(
            tile.boundary_ids(),
            BoundaryIds {
                right: BoundaryId(210),
                left: BoundaryId(231),
                top: BoundaryId(318),
                bottom: BoundaryId(89)
            }
        );
        tile.rotate();
        assert_eq!(
            tile.boundary_ids(),
            BoundaryIds {
                bottom: BoundaryId(210),
                top: BoundaryId(231),
                right: BoundaryId(318),
                left: BoundaryId(89)
            }
        );
        tile.flip_rows();
        assert_eq!(
            tile.boundary_ids(),
            BoundaryIds {
                top: BoundaryId(210),
                bottom: BoundaryId(231),
                right: BoundaryId(318),
                left: BoundaryId(89)
            }
        );
        tile.flip_columns();
        assert_eq!(
            tile.boundary_ids(),
            BoundaryIds {
                top: BoundaryId(210),
                bottom: BoundaryId(231),
                left: BoundaryId(318),
                right: BoundaryId(89)
            }
        );

        let mut tile2 = example_data()[&TileId(1171)].clone();
        assert_eq!(
            tile2.boundary_ids(),
            BoundaryIds {
                top: BoundaryId(399),
                bottom: BoundaryId(24),
                left: BoundaryId(391),
                right: BoundaryId(18)
            }
        );
        tile2.rotate();
        assert_eq!(
            tile2.boundary_ids(),
            BoundaryIds {
                right: BoundaryId(399),
                left: BoundaryId(24),
                top: BoundaryId(391),
                bottom: BoundaryId(18)
            }
        );
        tile2.flip_columns();
        assert_eq!(
            tile2.boundary_ids(),
            BoundaryIds {
                left: BoundaryId(399),
                right: BoundaryId(24),
                top: BoundaryId(391),
                bottom: BoundaryId(18)
            }
        );
    }

    fn example_image() -> Image {
        use MarkedCell::*;
        Image(vec![
            vec![
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
            ],
            vec![
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                Dot,
                Dot,
            ],
            vec![
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
            ],
            vec![
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
            ],
            vec![
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
            ],
            vec![
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
            ],
            vec![
                Dot,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
            ],
            vec![
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                Dot,
                Dot,
            ],
            vec![
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
            ],
            vec![
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
            ],
            vec![
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
            ],
            vec![
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
            ],
            vec![
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
            ],
            vec![
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
            ],
            vec![
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
            ],
            vec![
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
            ],
            vec![
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
            ],
            vec![
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
            ],
            vec![
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
            ],
            vec![
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
            ],
            vec![
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
            ],
            vec![
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
            ],
            vec![
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
            ],
            vec![
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
            ],
        ])
    }

    fn example_image2() -> Image {
        use MarkedCell::*;
        Image(vec![
            vec![
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
            ],
            vec![
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                MarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                Dot,
                Dot,
            ],
            vec![
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                MarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                MarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
            ],
            vec![
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                MarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
            ],
            vec![
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
            ],
            vec![
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                MarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
            ],
            vec![
                Dot,
                Dot,
                Dot,
                Dot,
                MarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                MarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
            ],
            vec![
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                MarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                Dot,
                MarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                Dot,
                Dot,
            ],
            vec![
                UnmarkedHash,
                Dot,
                Dot,
                MarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                MarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
            ],
            vec![
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                MarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
            ],
            vec![
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
            ],
            vec![
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                MarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
            ],
            vec![
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                MarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                MarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
            ],
            vec![
                Dot,
                Dot,
                Dot,
                MarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                MarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
            ],
            vec![
                Dot,
                UnmarkedHash,
                Dot,
                MarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                MarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
            ],
            vec![
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                MarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
            ],
            vec![
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
            ],
            vec![
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                MarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
            ],
            vec![
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                MarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                MarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
            ],
            vec![
                UnmarkedHash,
                Dot,
                Dot,
                MarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                Dot,
                MarkedHash,
                MarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
            ],
            vec![
                UnmarkedHash,
                UnmarkedHash,
                MarkedHash,
                MarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                MarkedHash,
                Dot,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
            ],
            vec![
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                MarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
            ],
            vec![
                Dot,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
            ],
            vec![
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                Dot,
                UnmarkedHash,
                Dot,
                Dot,
                UnmarkedHash,
                UnmarkedHash,
                UnmarkedHash,
            ],
        ])
    }

    #[test]
    fn test_arrangement_and_to_image() {
        let poss_arrangements = vec![
            vec![
                vec![TileId(1951), TileId(2311), TileId(3079)],
                vec![TileId(2729), TileId(1427), TileId(2473)],
                vec![TileId(2971), TileId(1489), TileId(1171)],
            ],
            vec![
                vec![TileId(2971), TileId(1489), TileId(1171)],
                vec![TileId(2729), TileId(1427), TileId(2473)],
                vec![TileId(1951), TileId(2311), TileId(3079)],
            ],
            vec![
                vec![TileId(3079), TileId(2311), TileId(1951)],
                vec![TileId(2473), TileId(1427), TileId(2729)],
                vec![TileId(1171), TileId(1489), TileId(2971)],
            ],
            vec![
                vec![TileId(1171), TileId(1489), TileId(2971)],
                vec![TileId(2473), TileId(1427), TileId(2729)],
                vec![TileId(3079), TileId(2311), TileId(1951)],
            ],
        ];
        let gen_tile_ids = |arrangement: &Arrangement| {
            arrangement
                .0
                .iter()
                .map(|row| row.iter().map(|(tile_id, _)| *tile_id).collect::<Vec<_>>())
                .collect::<Vec<_>>()
        };
        let mut actual_arrangement = find_arrangement(example_data());
        while gen_tile_ids(&actual_arrangement) != poss_arrangements[0] {
            assert!(poss_arrangements.contains(&gen_tile_ids(&actual_arrangement)));
            actual_arrangement = find_arrangement(example_data());
        }
        let mut img = Image::from_arrangement(actual_arrangement);
        assert_eq!(img, example_image());
        eprintln!("{}", img.draw());
        img.mark_hashes_with_tile(&mut sea_dragon());
        eprintln!("{}", img.draw());
        assert_eq!(img, example_image2());
        assert_eq!(
            img.0
                .into_iter()
                .flatten()
                .filter(|cell| cell == &MarkedCell::UnmarkedHash)
                .count(),
            273
        );
    }
}
