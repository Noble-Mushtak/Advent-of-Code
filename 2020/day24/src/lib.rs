use std::collections::HashSet;
use std::error::Error;
use std::fs;

#[derive(PartialEq, Debug, Clone, Copy, Eq, Hash)]
struct Point {
    x: isize,
    y: isize,
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum Direction {
    East,
    Southeast,
    Southwest,
    West,
    Northwest,
    Northeast,
}

impl Direction {
    fn adjust_point(&self, p: Point) -> Point {
        use Direction::*;
        match self {
            East => Point { x: p.x + 1, y: p.y },
            Southeast => Point {
                x: p.x + 1,
                y: p.y - 1,
            },
            Southwest => Point { x: p.x, y: p.y - 1 },
            West => Point { x: p.x - 1, y: p.y },
            Northwest => Point {
                x: p.x - 1,
                y: p.y + 1,
            },
            Northeast => Point { x: p.x, y: p.y + 1 },
        }
    }
}

impl Point {
    fn neighbors(&self) -> [Point; 6] {
        [
            Direction::East.adjust_point(*self),
            Direction::Southeast.adjust_point(*self),
            Direction::Southwest.adjust_point(*self),
            Direction::West.adjust_point(*self),
            Direction::Northwest.adjust_point(*self),
            Direction::Northeast.adjust_point(*self),
        ]
    }
}

#[derive(PartialEq, Debug, Clone)]
struct Directions(Vec<Direction>);

impl Directions {
    fn final_loc(&self) -> Point {
        self.0
            .iter()
            .fold(Point { x: 0, y: 0 }, |point, dir| dir.adjust_point(point))
    }
}

peg::parser! {
    grammar parser() for str {
        rule dir() -> Direction
          = "e" { Direction::East } /
            "se" { Direction::Southeast } /
            "sw" { Direction::Southwest } /
            "w" { Direction::West } /
            "nw" { Direction::Northwest } /
            "ne" { Direction::Northeast }

        rule dirs() -> Directions
          = dirs:(dir()*) { Directions(dirs) }

        pub(crate) rule parse() -> Vec<Directions>
          = dirss:(dirs()**"\n") { dirss }
    }
}

fn black_tiles(dirss: &[Directions]) -> HashSet<Point> {
    let mut black_tiles = HashSet::new();
    for loc in dirss.iter().map(Directions::final_loc) {
        if black_tiles.contains(&loc) {
            black_tiles.remove(&loc);
        } else {
            black_tiles.insert(loc);
        }
    }
    black_tiles
}

fn next_day(cur_black_tiles: HashSet<Point>) -> HashSet<Point> {
    let mut all_tiles = HashSet::new();
    for tile in cur_black_tiles.iter() {
        all_tiles.insert(*tile);
        for neighbor in tile.neighbors().iter().cloned() {
            all_tiles.insert(neighbor);
        }
    }

    all_tiles
        .into_iter()
        .filter(|tile| {
            let num_black_neighbors = tile
                .neighbors()
                .iter()
                .filter(|&tile2| cur_black_tiles.contains(tile2))
                .count();
            if cur_black_tiles.contains(tile) {
                num_black_neighbors == 1 || num_black_neighbors == 2
            } else {
                num_black_neighbors == 2
            }
        })
        .collect()
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let dirss = parser::parse(&fs::read_to_string("in.txt")?[..])?;

    let black_tiles = black_tiles(&dirss);
    println!("Part 1: {}", black_tiles.len());
    println!(
        "Part 2: {}",
        (0..100).fold(black_tiles, |tiles, _| next_day(tiles)).len()
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_final_loc() {
        assert_eq!(
            parser::parse("esenee").unwrap()[0].final_loc(),
            Point { x: 3, y: 0 }
        );
        assert_eq!(
            parser::parse("esew").unwrap()[0].final_loc(),
            Point { x: 1, y: -1 }
        );
        assert_eq!(
            parser::parse("nwwswee").unwrap()[0].final_loc(),
            Point { x: 0, y: 0 }
        );
    }

    fn example_data() -> Vec<Directions> {
        parser::parse(
            "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew",
        )
        .unwrap()
    }

    #[test]
    fn test_black_tiles() {
        assert_eq!(black_tiles(&example_data()).len(), 10);
    }

    #[test]
    fn test_next_day() {
        let mut tiles = black_tiles(&example_data());
        tiles = next_day(tiles);
        assert_eq!(tiles.len(), 15);
        tiles = next_day(tiles);
        assert_eq!(tiles.len(), 12);
        tiles = next_day(tiles);
        assert_eq!(tiles.len(), 25);
        tiles = next_day(tiles);
        assert_eq!(tiles.len(), 14);
        tiles = next_day(tiles);
        assert_eq!(tiles.len(), 23);
        tiles = next_day(tiles);
        assert_eq!(tiles.len(), 28);
        tiles = next_day(tiles);
        assert_eq!(tiles.len(), 41);
        tiles = next_day(tiles);
        assert_eq!(tiles.len(), 37);
        tiles = next_day(tiles);
        assert_eq!(tiles.len(), 49);
        tiles = next_day(tiles);
        assert_eq!(tiles.len(), 37);
        for _ in 0..10 {
            tiles = next_day(tiles);
        }
        assert_eq!(tiles.len(), 132);
        for _ in 0..10 {
            tiles = next_day(tiles);
        }
        assert_eq!(tiles.len(), 259);
        for _ in 0..10 {
            tiles = next_day(tiles);
        }
        assert_eq!(tiles.len(), 406);
        for _ in 0..10 {
            tiles = next_day(tiles);
        }
        assert_eq!(tiles.len(), 566);
        for _ in 0..10 {
            tiles = next_day(tiles);
        }
        assert_eq!(tiles.len(), 788);
        for _ in 0..10 {
            tiles = next_day(tiles);
        }
        assert_eq!(tiles.len(), 1106);
        for _ in 0..10 {
            tiles = next_day(tiles);
        }
        assert_eq!(tiles.len(), 1373);
        for _ in 0..10 {
            tiles = next_day(tiles);
        }
        assert_eq!(tiles.len(), 1844);
        for _ in 0..10 {
            tiles = next_day(tiles);
        }
        assert_eq!(tiles.len(), 2208);
    }
}
