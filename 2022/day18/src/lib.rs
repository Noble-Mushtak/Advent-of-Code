use std::cmp::{min, max};
use std::collections::{HashSet, VecDeque};
use std::error::Error;
use std::fs;

peg::parser! {
    grammar parser() for str {
        rule isize() -> isize
          = n:$(['0'..='9']+) {
            n.parse().unwrap()
        }

        rule point() -> (isize, isize, isize)
          = x:isize() "," y:isize() "," z:isize() { (x, y, z) }

        pub(crate) rule parse() -> Vec<(isize, isize, isize)>
          = l:(point()**"\n") "\n"? { l }
    }
}

const DIRS: [(isize, isize, isize); 6] = [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)];

fn surface_area(point_set: &HashSet<(isize, isize, isize)>) -> usize {
    point_set.iter().map(|(x, y, z)| {
        DIRS.iter().filter(|(dx, dy, dz)| !point_set.contains(&(x+dx, y+dy, z+dz))).count()
    }).sum()
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let points = parser::parse(&fs::read_to_string("in.txt")?)?;

    let point_set: HashSet<(isize, isize, isize)> = points.iter().copied().collect();
    println!("Part 1: {}", surface_area(&point_set));

    let (xmin, xmax, ymin, ymax, zmin, zmax) = points.iter().fold((1000, 0, 1000, 0, 1000, 0),
        |(xmn, xmx, ymn, ymx, zmn, zmx), &(x, y, z)| {
            (min(xmn, x-1), max(xmx, x+1), min(ymn, y-1), max(ymx, y+1), min(zmn, z-1), max(zmx, z+1))
        });
    
    let mut bfs_q: VecDeque<((isize, isize, isize), (isize, isize, isize))> = VecDeque::new();
    bfs_q.push_back(((xmin, ymin, zmin), (0, 0, 0)));
    let mut vis: HashSet<((isize, isize, isize), (isize, isize, isize))> = HashSet::new();
    let mut filtered_point_set: HashSet<((isize, isize, isize), (isize, isize, isize))> = HashSet::new();
    while let Some(((cur_x, cur_y, cur_z), dir_tpl)) = bfs_q.pop_front() {
        if point_set.contains(&(cur_x, cur_y, cur_z)) {
            filtered_point_set.insert(((cur_x, cur_y, cur_z), dir_tpl));
        } else {
            for (dx, dy, dz) in DIRS {
                let new_pt = (cur_x+dx, cur_y+dy, cur_z+dz);
                if !vis.contains(&(new_pt, (dx, dy, dz))) && (xmin..=xmax).contains(&new_pt.0) && (ymin..=ymax).contains(&new_pt.1) && (zmin..=zmax).contains(&new_pt.2) {
                    bfs_q.push_back((new_pt, (dx, dy, dz)));
                    vis.insert((new_pt, (dx, dy, dz)));
                }
            }
        }
    }
    println!("Part 2: {}", filtered_point_set.len());
    
    Ok(())
}