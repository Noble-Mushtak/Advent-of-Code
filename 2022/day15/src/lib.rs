use std::cmp::max;
use std::collections::HashSet;
use std::error::Error;
use std::fs;

peg::parser! {
    grammar parser() for str {
        rule isize() -> isize = precedence! {
          n:$(['0'..='9']+) { n.parse().unwrap() }
          "-" n:$(['0'..='9']+) { -(n.parse::<isize>().unwrap()) }
        }
        
        rule sensor() -> ((isize, isize), (isize, isize))
          = "Sensor at x=" x1:isize() ", y=" y1:isize() ": closest beacon is at x=" x2:isize() ", y=" y2:isize() { ((x1, y1), (x2, y2)) }

        pub(crate) rule parse() -> Vec<((isize, isize), (isize, isize))>
          = l:(sensor()**"\n") "\n"? { l }
    }
}

fn taken_x_coords(sensors: &[((isize, isize), (isize, isize))], cur_y: isize) -> Vec<(isize, isize)> {
    let mut intervals = vec![];
    for ((x1, y1), (x2, y2)) in sensors {
        let dst = (x1-x2).abs()+(y1-y2).abs();
        if (y1-cur_y).abs() <= dst {
            let space_left = dst-(y1-cur_y).abs();
            intervals.push((x1-space_left, x1+space_left));
        }
    }
    intervals.sort();
    let mut merged_intervals = vec![];
    for (st, nd) in intervals {
        let (new_st, new_nd) = match merged_intervals.iter().rev().next() {
            Some((old_st, old_nd)) if old_nd+1 >= st => (Some(*old_st), max(*old_nd, nd)),
            Some((_, old_nd)) => (None, max(*old_nd, nd)),
            _ => (None, nd)
        };
        match new_st {
            Some(new_val) => {
                merged_intervals.pop();
                merged_intervals.push((new_val, new_nd));
            },
            None => merged_intervals.push((st, new_nd))
        };
    }
    merged_intervals
}

fn find_missing_coord(sensors: &[((isize, isize), (isize, isize))], MAXY: isize) -> Option<(isize, isize)> {
    for y in 0..=MAXY {
        let intervals = taken_x_coords(sensors, y);
        for cur_slice in intervals.windows(2) {
            if let [(st1, nd1), (st2, nd2)] = cur_slice {
                if nd1+1 < *st2 && 0 <= nd1+1 && nd1+1 <= MAXY {
                    return Some((nd1+1, y));
                }
            }
        }
    }
    None
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let sensors = parser::parse(&fs::read_to_string("in.txt")?)?;

    let beacon_set: HashSet<(isize, isize)> = sensors.iter().map(|(_, pt2)| pt2).copied().collect();

    let PART1Y = 2000000;
    let intervals = taken_x_coords(&sensors, PART1Y);
    let beacons_in_interval: isize = intervals.iter().map(|(st, nd)| {
        beacon_set.iter().filter(|(x, y)| {
            y == &PART1Y && st <= x && x <= nd
        }).count()
    }).sum::<usize>() as isize;
    println!("Part 1: {}", intervals.iter().map(|(st, nd)| nd-st+1).sum::<isize>()-beacons_in_interval);

    let MAXY = 4000000;
    let (x, y) = find_missing_coord(&sensors, MAXY).unwrap();
    println!("Part 2: {}", x*MAXY+y);
    
    Ok(())
}