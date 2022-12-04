use std::cmp::{min, max};
use std::error::Error;
use std::fs;

struct ElfPair {
    first_st: usize,
    first_nd: usize,
    second_st: usize,
    second_nd: usize
}

peg::parser! {
    grammar parser() for str {
        rule usize() -> usize
          = c:$(['0'..='9']+) {
            c.parse().unwrap()
        }
        
        rule elfpair() -> ElfPair
          = first_st:usize() _:"-" first_nd:usize() _:"," second_st:usize() _:"-" second_nd:usize() {
          ElfPair { first_st, first_nd, second_st, second_nd }
        }

        pub(crate) rule parse() -> Vec<ElfPair>
          = l:(elfpair()**"\n") _:"\n"? { l }
    }
}

fn interval_contains(interval1: (usize, usize), interval2: (usize, usize)) -> bool {
    interval2.0 <= interval1.0 && interval1.1 <= interval2.1
}

fn one_interval_contains_other<'a>(cur_pair: &'a &ElfPair) -> bool {
    interval_contains((cur_pair.first_st, cur_pair.first_nd), (cur_pair.second_st, cur_pair.second_nd)) ||
    interval_contains((cur_pair.second_st, cur_pair.second_nd), (cur_pair.first_st, cur_pair.first_nd))
}

fn intervals_overlap<'a>(cur_pair: &'a &ElfPair) -> bool {
    max(cur_pair.first_st, cur_pair.second_st) <= min(cur_pair.first_nd, cur_pair.second_nd)
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let elfpairs = parser::parse(&fs::read_to_string("in.txt")?[..])?;

    println!("Part 1: {}", elfpairs.iter().filter(one_interval_contains_other).count());
    println!("Part 2: {}", elfpairs.iter().filter(intervals_overlap).count());
    Ok(())
}
