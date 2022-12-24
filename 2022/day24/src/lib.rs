use std::collections::{HashSet, HashMap, VecDeque};
use std::error::Error;
use std::fs;

#[derive(Copy, Clone, Debug)]
enum Direction {
    RightD,
    DownD,
    LeftD,
    UpD
}
use crate::Direction::{RightD, DownD, LeftD, UpD};

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone, Debug)]
struct State {
    time: usize,
    row: isize,
    col: isize,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
enum Goal {
    ReachGoalForFirstTime,
    GoBackToStart,
    ReachGoalForSecondTime,
}
use crate::Goal::{ReachGoalForFirstTime, GoBackToStart, ReachGoalForSecondTime};

peg::parser! {
    grammar parser() for str {
        rule cell() -> Option<Direction>
          = c:($(['>' | 'v' | '<' | '^' | '.'])) {
            match c {
                ">" => Some(RightD),
                "v" => Some(DownD),
                "<" => Some(LeftD),
                "^" => Some(UpD),
                _   => None,
            }
        }
        
        rule row() -> Vec<Option<Direction>>
          = "#" l:(cell()+) "#" { l }

        pub(crate) rule parse() -> Vec<Vec<Option<Direction>>>
          = "#." "#"* "\n" l:(row()**"\n") "\n" "#"* ".#" "\n"? { l }
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let grid = parser::parse(&fs::read_to_string("in.txt")?)?;

    let mut blizzards: HashMap<(isize, isize), Vec<Direction>> = HashMap::new();
    for (row_idx, row) in grid.iter().enumerate() {
        for (col_idx, cell) in row.iter().enumerate() {
            if let Some(dir) = cell {
                blizzards.entry((row_idx as isize, col_idx as isize)).or_insert(vec![]).push(*dir);
            }
        }
    }
    
    let max_row = (grid.len() as isize)-1;
    let max_col = (grid[0].len() as isize)-1;
    let get_neighbors = |row: isize, col: isize| {
        let mut neighbors: Vec<(isize, isize)> =
            [(row-1, col), (row+1, col), (row, col-1), (row, col+1)]
                .into_iter()
                .filter(|(r, c)| (0..=max_row).contains(r) && (0..=max_col).contains(c))
                .collect();
        if row == 0 && col == 0 { neighbors.push((-1, 0)); }
        if row == max_row && col == max_col { neighbors.push((max_row+1, max_col)); }
        neighbors.push((row, col));
        neighbors
    };

    let mut time_to_look_at = 0;
    let mut cur_goal = ReachGoalForFirstTime;
    let mut bfs_q: VecDeque<State> = VecDeque::new();
    let mut added_to_q: HashSet<(isize, isize)> = HashSet::new();
    
    let init_state = State { time: 0, row: -1, col: 0 };
    bfs_q.push_back(init_state);
    added_to_q.insert((init_state.row, init_state.col));
    while let Some(State { time: cur_time, row: cur_row, col: cur_col }) = bfs_q.pop_front() {
        if cur_row == max_row+1 && cur_col == max_col {
            if cur_goal == ReachGoalForFirstTime {
                println!("Part 1: {}", cur_time);
                bfs_q = VecDeque::new();
                added_to_q = HashSet::new();
                cur_goal = GoBackToStart;
            } else if cur_goal == ReachGoalForSecondTime {
                println!("Part 2: {}", cur_time);
                break;
            }
        }
        if cur_row == -1 && cur_col == 0 && cur_goal == GoBackToStart {
            bfs_q = VecDeque::new();
            added_to_q = HashSet::new();
            cur_goal = ReachGoalForSecondTime;
        }
        
        if cur_time+1 > time_to_look_at {
            time_to_look_at = cur_time+1;
            added_to_q = HashSet::new();
            
            let mut new_blizzards: HashMap<(isize, isize), Vec<Direction>> = HashMap::new();
            for ((bl_row, bl_col), ds) in blizzards.iter() {
                for dir in ds {
                    let (new_bl_row, new_bl_col) = match dir {
                        RightD => (*bl_row, bl_col+1),
                        DownD => (bl_row+1, *bl_col),
                        LeftD => (*bl_row, bl_col-1),
                        UpD => (bl_row-1, *bl_col)
                    };
                    let (new_bl_row, new_bl_col) = (
                        new_bl_row.rem_euclid(max_row+1),
                        new_bl_col.rem_euclid(max_col+1),
                    );
                    new_blizzards.entry((new_bl_row, new_bl_col)).or_insert(vec![]).push(*dir);
                }
            }
            blizzards = new_blizzards;
        }
        
        for (new_row, new_col) in get_neighbors(cur_row, cur_col) {
            let new_st = State { time: cur_time+1, row: new_row, col: new_col };
            if !blizzards.contains_key(&(new_row, new_col)) && !added_to_q.contains(&(new_row, new_col)) {
                bfs_q.push_back(new_st);
                added_to_q.insert((new_row, new_col));
            }
        }
    }
    
    Ok(())
}