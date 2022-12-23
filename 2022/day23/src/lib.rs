use std::cmp::{min, max};
use std::collections::HashSet;
use std::error::Error;
use std::fs;

peg::parser! {
    grammar parser() for str {
        rule cell() -> bool
          = c:$(['.' | '#']) { c == "#" }

        rule row() -> Vec<bool>
          = l:(cell()*) { l }

        pub(crate) rule parse() -> Vec<Vec<bool>>
          = l:(row()**"\n") "\n"? { l }
    }
}

#[repr(usize)]
#[derive(Clone, Copy, Debug)]
enum Direction {
    NorthD = 0,
    SouthD = 1,
    EastD = 2,
    WestD = 3
}
use crate::Direction::{NorthD, SouthD, EastD, WestD};

fn next_dir(d: Direction) -> Direction {
    match d {
        NorthD => SouthD,
        SouthD => EastD,
        EastD => WestD,
        WestD => NorthD,
    }
}

fn dir_orders(d: Direction) -> [Direction; 4] {
    [d, next_dir(d), next_dir(next_dir(d)), next_dir(next_dir(next_dir(d)))]
}

const NEIGHBOR_TPLS: [(isize, isize); 8] = [(-1, 0), (1, 0), (0, -1), (0, 1), (-1, 1), (1, -1), (-1, -1), (1, 1)];
const MOVE_TPLS: [(isize, isize); 4] = [(-1, 0), (1, 0), (0, -1), (0, 1)];
const CHECK_TPLS: [&[(isize, isize)]; 4] = [&[(-1, 0), (-1, 1), (-1, -1)], &[(1, 0), (1, -1), (1, 1)], &[(0, -1), (1, -1), (-1, -1)], &[(0, 1), (1, 1), (-1, 1)]];

pub fn run() -> Result<(), Box<dyn Error>> {
    let grid = parser::parse(&fs::read_to_string("in.txt")?)?;

    let mut elves = vec![];
    for (i, row) in grid.iter().enumerate() {
        for (j, b) in row.iter().enumerate() {
            if *b {
                elves.push((i as isize, j as isize));
            }
        }
    }

    let mut first_dir = NorthD;
    let mut some_move_occurred = true;
    let mut num_rounds = 0;
    while some_move_occurred {
        let elf_set: HashSet<(isize, isize)> = elves.iter().copied().collect();
        let mut proposals: HashSet<(isize, isize)> = HashSet::new();
        let mut bad_proposals: HashSet<(isize, isize)> = HashSet::new();
        let mut proposed_moves = vec![None; elves.len()];
        for (i, (row, col)) in elves.iter().enumerate() {
            if NEIGHBOR_TPLS.iter().any(|(drow, dcol)| elf_set.contains(&(row+drow, col+dcol))) {
                for d in dir_orders(first_dir) {
                    if CHECK_TPLS[d as usize].iter().all(|(drow, dcol)| !elf_set.contains(&(row+drow, col+dcol))) {
                        let new_proposal = (*row+MOVE_TPLS[d as usize].0, *col+MOVE_TPLS[d as usize].1);
                        if proposals.contains(&new_proposal) {
                            bad_proposals.insert(new_proposal);
                        } else {
                            proposals.insert(new_proposal);
                            proposed_moves[i] = Some(new_proposal);
                        }
                        break;
                    }
                }
            }
        }
        some_move_occurred = false;
        for (i, opt_proposal) in proposed_moves.iter().enumerate() {
            if let Some(proposal) = opt_proposal {
                if !bad_proposals.contains(proposal) {
                    elves[i] = *proposal;
                    some_move_occurred = true;
                }
            }
        }
        first_dir = next_dir(first_dir);
        num_rounds += 1;

        if num_rounds == 10 {
            let (mn_row, mx_row, mn_col, mx_col) = elves.iter()
                .fold((elves[0].0, elves[0].0, elves[0].1, elves[0].1),
                    |(mnr, mxr, mnc, mxc), (r, c)| {
                        (min(mnr, *r), max(mxr, *r), min(mnc, *c), max(mxc, *c))
                    });
            println!("Part 1: {}", (mx_row-mn_row+1)*(mx_col-mn_col+1)-(elves.len() as isize));
        }
    }
    println!("Part 2: {}", num_rounds);
    
    Ok(())
}