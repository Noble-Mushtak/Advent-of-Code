use std::cmp::{min, max};
use std::collections::{HashSet, HashMap, VecDeque};
use std::error::Error;
use std::fs;

#[derive(Clone, Debug)]
enum Direction {
    MoveD(isize),
    TurnLeftD,
    TurnRightD
}
use crate::Direction::{MoveD, TurnLeftD, TurnRightD};

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

        rule maze() -> HashMap<(isize, isize), bool> = precedence! {
          newl:(line()) "\n" mz:(maze()) {
            let mut mp: HashMap<(isize, isize), bool> = newl.into_iter().map(|(x, b)| ((0, x), b)).collect();
            for ((y, x), b) in mz.into_iter() {
                mp.insert((y+1, x), b);
            }
            mp
          }
          newl:(line()) {
            newl.into_iter().map(|(x, b)| ((0, x), b)).collect()
          }
        }

        rule dirs() -> VecDeque<Direction> = precedence! {
          mv:(isize()) c:$(['L' | 'R']) ds:(dirs()) {
            let mut new_ds = ds;
            new_ds.push_front(if c == "L" { TurnLeftD } else { TurnRightD });
            new_ds.push_front(MoveD(mv));
            new_ds
          }
          mv:(isize()) {
            let mut ds = VecDeque::new();
            ds.push_front(MoveD(mv));
            ds
          }
        }

        pub(crate) rule parse() -> (HashMap<(isize, isize), bool>, VecDeque<Direction>)
          = l:(maze()) "\n\n" ds:(dirs()) "\n"? { (l, ds) }
    }
}


const DIR_TPLS: [(isize, isize); 4] = [(0, 1), (1, 0), (0, -1), (-1, 0)];
const CUBE_DIM: isize = 50;

fn calc_endpoint<F>(cur_maze: &HashMap<(isize, isize), bool>, dirs: &VecDeque<Direction>, wraparound_cell: F) -> ((isize, isize), isize)
  where F: Fn(usize, (isize, isize)) -> (usize, (isize, isize)) {
    let mut cur_facing = 0;
    let mut cur_loc = cur_maze.keys().min().unwrap().clone();
    for d in dirs {
        match d {
            MoveD(mv) => {
                for i in 0..*mv {
                    let new_cell = (cur_loc.0+DIR_TPLS[cur_facing].0, cur_loc.1+DIR_TPLS[cur_facing].1);
                    let (new_facing, new_new_cell) = if !cur_maze.contains_key(&new_cell) {
                        wraparound_cell(cur_facing, cur_loc)
                    } else {
                        (cur_facing, new_cell)
                    };
                    if cur_maze[&new_new_cell] {
                        cur_loc = new_new_cell;
                        cur_facing = new_facing;
                    } else {
                        break;
                    }
                }
            },
            TurnLeftD => cur_facing = ((cur_facing as isize)-1).rem_euclid(DIR_TPLS.len() as isize) as usize,
            TurnRightD => cur_facing = ((cur_facing as isize)+1).rem_euclid(DIR_TPLS.len() as isize) as usize,
        }
    }
    (cur_loc, cur_facing as isize)
}

fn cross_product(pt1: (isize, isize, isize), pt2: (isize, isize, isize)) -> (isize, isize, isize) {
    let (a,b,c) = pt1;
    let (d,e,f) = pt2;
    //Source: https://www.wolframalpha.com/input?i=cross+product+of+%28a%2Cb%2Cc%29+and+%28d%2Ce%2Cf%29
    (-c*e+b*f, c*d-a*f, -b*d+a*e)
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let (cur_maze, dirs) = parser::parse(&fs::read_to_string("in.txt")?)?;
    
    let mut mn_mx_by_row: HashMap<isize, (isize, isize)> = HashMap::new();
    let mut mn_mx_by_col: HashMap<isize, (isize, isize)> = HashMap::new();
    for (row, col) in cur_maze.keys() {
        mn_mx_by_row.entry(*row).and_modify(|(mn, mx)| {
            *mn = min(*mn, *col);
            *mx = max(*mx, *col);
        })
        .or_insert((*col, *col));
        mn_mx_by_col.entry(*col).and_modify(|(mn, mx)| {
            *mn = min(*mn, *row);
            *mx = max(*mx, *row);
        })
        .or_insert((*row, *row));
    }
    let p1wraparound = |cur_facing, cur_loc: (isize, isize)| {
        match cur_facing {
            0 => (cur_facing, (cur_loc.0, mn_mx_by_row[&cur_loc.0].0)),
            1 => (cur_facing, (mn_mx_by_col[&cur_loc.1].0, cur_loc.1)),
            2 => (cur_facing, (cur_loc.0, mn_mx_by_row[&cur_loc.0].1)),
            3 => (cur_facing, (mn_mx_by_col[&cur_loc.1].1, cur_loc.1)),
            _ => unreachable!()
        }
    };

    let ((p1row, p1col), p1facing) = calc_endpoint(&cur_maze, &dirs, p1wraparound);
    println!("Part 1: {}", 1000*(p1row+1)+4*(p1col+1)+p1facing);

    let next_pole = |up_pole: (isize, isize, isize), right_pole: (isize, isize, isize), cur_facing| {
        match cur_facing {
            0 => (right_pole, (-up_pole.0, -up_pole.1, -up_pole.2)),
            1 => (cross_product(right_pole, up_pole), right_pole),
            2 => ((-right_pole.0, -right_pole.1, -right_pole.2), up_pole),
            3 => (cross_product(up_pole, right_pole), right_pole),
            _ => unreachable!()
        }
    };
    
    let left_corner = cur_maze.keys().min().unwrap().clone();
    let mut bfs_q: VecDeque<((isize, isize), ((isize, isize, isize), (isize, isize, isize)))> = VecDeque::new();
    let mut orient: HashMap<(isize, isize), ((isize, isize, isize), (isize, isize, isize))> = HashMap::new();
    let mut added_to_q: HashSet<(isize, isize)> = HashSet::new();
    
    bfs_q.push_back((left_corner, ((0, 0, 1), (1, 0, 0))));
    added_to_q.insert(left_corner);
    while let Some((cur_loc, (up_pole, right_pole))) = bfs_q.pop_front() {
        orient.insert(cur_loc, (up_pole, right_pole));
        for (i, (drow, dcol)) in DIR_TPLS.iter().enumerate() {
            let new_loc = (cur_loc.0+drow*CUBE_DIM, cur_loc.1+dcol*CUBE_DIM);
            if cur_maze.contains_key(&new_loc) && !added_to_q.contains(&new_loc) {
                bfs_q.push_back((new_loc, next_pole(up_pole, right_pole, i)));
                added_to_q.insert(new_loc);
            }
        }
    }
    
    let p2wraparound = |mut cur_facing, cur_loc: (isize, isize)| {
        let face_corner = ((cur_loc.0/CUBE_DIM)*CUBE_DIM, (cur_loc.1/CUBE_DIM)*CUBE_DIM);
        let (up_pole, right_pole) = orient[&face_corner];
        let (new_up_pole, mut cur_right_pole) = next_pole(up_pole, right_pole, cur_facing);
        let (new_face_corner, (_, new_right_pole)) = orient.iter().find(|(_, (poss_up_pole, _))| poss_up_pole == &new_up_pole).unwrap();
        
        let face_loc = (cur_loc.0 % CUBE_DIM, cur_loc.1 % CUBE_DIM);
        let mut new_face_loc = match cur_facing {
            0 => (face_loc.0, 0),
            1 => (0, face_loc.1),
            2 => (face_loc.0, CUBE_DIM-1),
            3 => (CUBE_DIM-1, face_loc.1),
            _ => unreachable!()
        };

        while cur_right_pole != *new_right_pole {
            cur_right_pole = cross_product(cur_right_pole, new_up_pole);
            new_face_loc = (CUBE_DIM-1-new_face_loc.1, new_face_loc.0);
            cur_facing = ((cur_facing as isize)-1).rem_euclid(DIR_TPLS.len() as isize) as usize;
        }

        let new_loc = (new_face_corner.0+new_face_loc.0, new_face_corner.1+new_face_loc.1);
        (cur_facing, new_loc)
    };
    
    let ((p2row, p2col), p2facing) = calc_endpoint(&cur_maze, &dirs, p2wraparound);
    println!("Part 2: {}", 1000*(p2row+1)+4*(p2col+1)+p2facing);
    
    Ok(())
}