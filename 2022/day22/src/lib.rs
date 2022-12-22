use std::cmp::{min, max};
use std::collections::{HashSet, HashMap, VecDeque};
use std::error::Error;
use std::fs;

#[derive(Copy, Clone, Debug)]
enum Command {
    MoveC(isize), //MoveC(m)   - Command saying we should move d steps forward
    TurnLeftC,    //TurnLeftC  - Command saying we should turn left
    TurnRightC    //TurnRightC - Command saying we should turn right
}
use crate::Command::{MoveC, TurnLeftC, TurnRightC};

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone, Debug)]
struct Location {
    row: isize, //Rows are numbered 0, 1, 2, ... starting from top to bottom
    col: isize  //Columns are numbered 0, 1, 2, ... starting from left to right
}

#[derive(Copy, Clone, Debug)]
enum Direction {
    RightD = 0,
    DownD = 1,
    LeftD = 2,
    UpD = 3
}
use crate::Direction::{RightD, DownD, LeftD, UpD};

const DIRS: [Direction; 4] = [RightD, DownD, LeftD, UpD];
//DIR_TPLS[i] = (change in row, change in col) if you go one step in the direction represented by `i`
const DIR_TPLS: [(isize, isize); 4] = [(0, 1), (1, 0), (0, -1), (-1, 0)];

fn rotate_left(dr: Direction) -> Direction {
    match dr {
        RightD => UpD,
        DownD => RightD,
        LeftD => DownD,
        UpD => LeftD,
    }
}

fn rotate_right(dr: Direction) -> Direction {
    match dr {
        RightD => DownD,
        DownD => LeftD,
        LeftD => UpD,
        UpD => RightD,
    }
}

#[derive(Copy, Clone, Debug)]
struct Orientation {
    up_pole: (isize, isize, isize),    //3D unit vector representing the direction from the center of the cube
                                       //to the face of the cube that is currently on top
    right_pole: (isize, isize, isize), //3D unit vector representing the direction from the center of the cube
                                       //to the face of the cube that is on the right
}

fn cross_product(pt1: (isize, isize, isize), pt2: (isize, isize, isize)) -> (isize, isize, isize) {
    let (a,b,c) = pt1;
    let (d,e,f) = pt2;
    //Source: https://www.wolframalpha.com/input?i=cross+product+of+%28a%2Cb%2Cc%29+and+%28d%2Ce%2Cf%29
    (-c*e+b*f, c*d-a*f, -b*d+a*e)
}

//Parsing expression grammer for parsing the input
//Outputs tuple containing
// - HashMap<Location, bool>,
//   representing all the locations which have either a . or # in the net
//   The location maps to true if there is a . at the location, or false if there is a # at the location
// - VecDeque<Command>, representing the list of commands on how we should move in the net
peg::parser! {
    grammar parser() for str {
        rule isize() -> isize
          = n:$(['0'..='9']+) {
            n.parse().unwrap()
        }

        rule line() -> HashMap<isize, bool>
          = s:$([' ' | '.' | '#']+) {
          let mut mp = HashMap::new();
          for (i, ch) in s.chars().enumerate() {
              if ch == '.' {
                  mp.insert(i as isize, true);
              } else if ch == '#' {
                  mp.insert(i as isize, false);
              }
          }
          mp
        }

        rule maze() -> HashMap<Location, bool> = precedence! {
          newl:(line()) "\n" mz:(maze()) {
            let mut mp: HashMap<Location, bool> = newl.into_iter().map(|(x, b)| (Location { row: 0, col: x }, b)).collect();
            for (Location { row, col }, b) in mz.into_iter() {
                mp.insert(Location { row: row+1, col: col }, b);
            }
            mp
          }
          newl:(line()) {
            newl.into_iter().map(|(x, b)| (Location { row: 0, col: x }, b)).collect()
          }
        }

        rule comms() -> VecDeque<Command> = precedence! {
          mv:(isize()) c:$(['L' | 'R']) cs:(comms()) {
            let mut new_cs = cs;
            new_cs.push_front(if c == "L" { TurnLeftC } else { TurnRightC });
            new_cs.push_front(MoveC(mv));
            new_cs
          }
          mv:(isize()) {
            let mut cs = VecDeque::new();
            cs.push_front(MoveC(mv));
            cs
          }
        }

        pub(crate) rule parse() -> (HashMap<Location, bool>, VecDeque<Command>)
          = l:(maze()) "\n\n" cs:(comms()) "\n"? { (l, cs) }
    }
}

//Using the maze and the commands from the puzzle input,
//and a function telling us our new direction/location when we fall off the edge of the net,
//find our final direction and location.
//This function essentially abstracts out the common code between Part 1 and Part 2,
//so the only difference between Part 1 and Part 2 is the wraparound_cell function.
fn calc_endpoint<F>(cur_maze: &HashMap<Location, bool>, comms: &VecDeque<Command>, wraparound_cell: F) -> (Direction, Location)
  where F: Fn(Direction, Location) -> (Direction, Location) {
    let mut cur_facing = RightD;
    let mut cur_loc = cur_maze.keys().min().unwrap().clone();
    for c in comms {
        match c {
            MoveC(mv) => {
                for _ in 0..*mv {
                    //New location = cur_loc + DIR_TPLS[cur_facing]
                    let new_cell = Location {
                        row: cur_loc.row+DIR_TPLS[cur_facing as usize].0,
                        col: cur_loc.col+DIR_TPLS[cur_facing as usize].1
                    };
                    //Call wraparound_cell if the new location is not in cur_maze,
                    //since that means the new location is out of the bounds of the net
                    let (new_facing, actual_new_cell) = if !cur_maze.contains_key(&new_cell) {
                        wraparound_cell(cur_facing, cur_loc)
                    } else {
                        (cur_facing, new_cell)
                    };
                    //If this cell has a . in it, then update the current location and direction,
                    //otherwise break from the loop and go to the next command
                    if cur_maze[&actual_new_cell] {
                        cur_loc = actual_new_cell;
                        cur_facing = new_facing;
                    } else {
                        break;
                    }
                }
            },
            TurnLeftC => cur_facing = rotate_left(cur_facing),
            TurnRightC => cur_facing = rotate_right(cur_facing),
        }
    }
    (cur_facing, cur_loc)
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let (cur_maze, comms) = parser::parse(&fs::read_to_string("in.txt")?)?;

    // -- Part 1 Code --
    //Calculate the minimum and maximum column in every row,
    //and the minimum and maximum row in every column
    let mut mn_by_row: HashMap<isize, isize> = HashMap::new();
    let mut mx_by_row: HashMap<isize, isize> = HashMap::new();
    let mut mn_by_col: HashMap<isize, isize> = HashMap::new();
    let mut mx_by_col: HashMap<isize, isize> = HashMap::new();
    for Location { row, col } in cur_maze.keys() {
        mn_by_row.entry(*row).and_modify(|val| *val = min(*val, *col)).or_insert(*col);
        mx_by_row.entry(*row).and_modify(|val| *val = max(*val, *col)).or_insert(*col);
        mn_by_col.entry(*col).and_modify(|val| *val = min(*val, *row)).or_insert(*row);
        mx_by_col.entry(*col).and_modify(|val| *val = max(*val, *row)).or_insert(*row);
    }
    let p1wraparound = |cur_facing: Direction, cur_loc: Location| {
        match cur_facing {
            //Go off right edge -> wrap around by going to the leftmost column in current row
            RightD => (cur_facing, Location { row: cur_loc.row, col: mn_by_row[&cur_loc.row] }),
            //Go off bottom edge -> wrap around by going to the topmost row in current column
            DownD => (cur_facing, Location { row: mn_by_col[&cur_loc.col], col: cur_loc.col }),
            //Go off left edge -> wrap around by going to the rightmost column in current row
            LeftD => (cur_facing, Location { row: cur_loc.row, col: mx_by_row[&cur_loc.row] }),
            //Go off top edge -> wrap around by going to the bottommost row in current column
            UpD => (cur_facing, Location { row: mx_by_col[&cur_loc.col], col: cur_loc.col }),
        }
    };

    let (p1facing, Location { row: p1row, col: p1col }) = calc_endpoint(&cur_maze, &comms, p1wraparound);
    println!("Part 1: {}", 1000*(p1row+1)+4*(p1col+1)+((p1facing as usize) as isize));

    // -- Part 2 Code --
    
    //number of locations = 6*(side of cube)^2 -> side of cube = sqrt((number of locations)/6)
    let cube_dim = ((cur_maze.len()/6) as f64).sqrt() as isize;

    //Calculate the new orientation after we rotate the cube so that the face in the `cur_facing` direction is now on top
    let next_orientation = |orientation: Orientation, cur_facing: Direction| -> Orientation {
        let Orientation { up_pole, right_pole } = orientation;
        match cur_facing {
            RightD => Orientation {
                up_pole: right_pole,
                right_pole: (-up_pole.0, -up_pole.1, -up_pole.2)
            },
            DownD => Orientation {
                up_pole: cross_product(right_pole, up_pole),
                right_pole: right_pole
            },
            LeftD => Orientation {
                up_pole: (-right_pole.0, -right_pole.1, -right_pole.2),
                right_pole: up_pole
            },
            UpD => Orientation {
                up_pole: cross_product(up_pole, right_pole),
                right_pole: right_pole
            }
        }
    };

    //Leftmost location in the topmost row:
    let left_corner = cur_maze.keys().min().unwrap().clone();
    
    //Map from location of the topleft corner of every face in the net
    //to the orientation of the cube when that face is on top,
    //rotated so that the face looks the same way it does in the net when looking at the cube from above
    let mut orient: HashMap<Location, Orientation> = HashMap::new();
    //BFS queue we use to traverse all the topleft corners of the faces
    let mut bfs_q: VecDeque<(Location, Orientation)> = VecDeque::new();
    //Set of all topleft corners of faces we have seen so far
    let mut added_to_q: HashSet<Location> = HashSet::new();

    //Arbitrarily assign the face corresponding to left_corner the positive z-direction, with the positive x-direction being on right
    bfs_q.push_back((left_corner, Orientation { up_pole: (0, 0, 1), right_pole: (1, 0, 0) }));
    added_to_q.insert(left_corner);
    while let Some((cur_loc, cur_orientation)) = bfs_q.pop_front() {
        orient.insert(cur_loc, cur_orientation);
        for (i, (drow, dcol)) in DIR_TPLS.iter().enumerate() {
            let new_loc = Location { row: cur_loc.row+drow*cube_dim, col: cur_loc.col+dcol*cube_dim };
            if cur_maze.contains_key(&new_loc) && !added_to_q.contains(&new_loc) {
                bfs_q.push_back((new_loc, next_orientation(cur_orientation, DIRS[i])));
                added_to_q.insert(new_loc);
            }
        }
    }
    
    let p2wraparound = |mut cur_facing: Direction, cur_loc: Location| {
        //Get the topleft corner of the face we are currently on
        let face_corner = Location {
            row: (cur_loc.row/cube_dim)*cube_dim,
            col: (cur_loc.col/cube_dim)*cube_dim
        };

        //Get the current orientation of the cube
        let cur_orientation = orient[&face_corner];
        //Get the new orientation of the cube after rotating the cube
        //so that the face in the direction of `cur_facing` is now on top
        let Orientation { up_pole: new_up_pole, right_pole: mut cur_right_pole } = next_orientation(cur_orientation, cur_facing);

        //new_face_corner - topleft corner of the new face we will be on
        //new_right_pole  - direction from center of the cube to the face that should be on right
        //                  once the cube is rotated so that the new face looks the same way it does in the net
        //                  when the new face is on top and we are looking at the cube from above
        let (new_face_corner, Orientation { up_pole: _, right_pole: new_right_pole }) =
            orient.iter()
                  .find(|(_, Orientation { up_pole: poss_up_pole, right_pole: _ })| poss_up_pole == &new_up_pole)
                  .unwrap();

        //face_loc is our current location, relative to the topleft corner of the face we are on        
        let face_loc = Location { row: cur_loc.row % cube_dim, col: cur_loc.col % cube_dim };
        //New location relative to the topleft corner of the new face
        let mut new_face_loc = match cur_facing {
            //Go off right edge -> wrap around by going to the leftmost column in the face
            RightD => Location { row: face_loc.row, col: 0 },
            //Go off bottom edge -> wrap around by going to the topmost row in the face
            DownD => Location { row: 0, col: face_loc.col },
            //Go off left edge -> wrap around by going to the rightmost column in the face
            LeftD => Location { row: face_loc.row, col: cube_dim-1 },
            //Go off top edge -> wrap around by going to the bottommost row in the face
            UpD => Location { row: cube_dim-1, col: face_loc.col },
        };

        //Keep rotating the cube clockwise (when looking from the top)
        //until the cube is rotated so the right pole is `new_right_pole`,
        //since that means the new face has been rotated so that it looks the same way it does in the net
        while cur_right_pole != *new_right_pole {
            cur_right_pole = cross_product(cur_right_pole, new_up_pole);
            //Also rotate new_face_loc and the direction we are facing when we rotate the cube clockwise
            new_face_loc = Location { row: cube_dim-1-new_face_loc.col, col: new_face_loc.row };
            cur_facing = rotate_left(cur_facing);
        }

        //New location is new_face_corner + new_face_loc
        let new_loc = Location { row: new_face_corner.row+new_face_loc.row, col: new_face_corner.col+new_face_loc.col };
        (cur_facing, new_loc)
    };
    
    let (p2facing, Location { row: p2row, col: p2col }) = calc_endpoint(&cur_maze, &comms, p2wraparound);
    println!("Part 2: {}", 1000*(p2row+1)+4*(p2col+1)+((p2facing as usize) as isize));
    
    Ok(())
}