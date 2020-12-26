use std::collections::HashMap;
use std::error::Error;
use std::fs;

#[derive(PartialEq, Debug, Clone)]
struct GameState {
    starting_list: Vec<usize>,
    last_num: Option<usize>,
    nums_so_far: HashMap<usize, usize>,
    loc: usize,
}

impl GameState {
    fn new(starting_list: Vec<usize>) -> GameState {
        GameState {
            starting_list,
            last_num: None,
            nums_so_far: HashMap::new(),
            loc: 0,
        }
    }
}

impl Iterator for GameState {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        let cur_loc = self.loc;
        self.loc += 1;

        let res = if cur_loc < self.starting_list.len() {
            self.starting_list[cur_loc]
        } else {
            match self.nums_so_far.get(&self.last_num?) {
                None => 0,
                Some(old_loc) => cur_loc - 1 - old_loc,
            }
        };
        if let Some(last_num) = self.last_num {
            self.nums_so_far.insert(last_num, cur_loc - 1);
        }
        self.last_num = Some(res);
        Some(res)
    }
}

peg::parser! {
    grammar parser() for str {
        rule usize() -> usize
          = n:$(['0'..='9']+) {
            n.parse().unwrap()
        }

        pub(crate) rule parse() -> GameState
          = l:(usize()**",") { GameState::new(l) }
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let mut start_state = parser::parse(&fs::read_to_string("in.txt")?[..])?;

    let mut state_p1 = start_state.clone();
    println!(
        "Part 1: {}",
        state_p1
            .nth(2020 - 1)
            .expect("Failed to find answer for part 1")
    );
    println!(
        "Part 2: {}",
        start_state
            .nth(30000000 - 1)
            .expect("Failed to find answer for part 2")
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_p1() {
        assert_eq!(GameState::new(vec![0, 3, 6]).nth(2020 - 1), Some(436));
        assert_eq!(GameState::new(vec![1, 3, 2]).nth(2020 - 1), Some(1));
        assert_eq!(GameState::new(vec![2, 1, 3]).nth(2020 - 1), Some(10));
        assert_eq!(GameState::new(vec![1, 2, 3]).nth(2020 - 1), Some(27));
        assert_eq!(GameState::new(vec![2, 3, 1]).nth(2020 - 1), Some(78));
        assert_eq!(GameState::new(vec![3, 2, 1]).nth(2020 - 1), Some(438));
        assert_eq!(GameState::new(vec![3, 1, 2]).nth(2020 - 1), Some(1836));
    }
}
