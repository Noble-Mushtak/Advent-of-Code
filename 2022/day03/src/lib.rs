#![feature(slice_group_by)]
use std::collections::HashSet;
use std::error::Error;
use std::fs;

#[derive(PartialEq, Debug, Clone)]
struct Rucksack {
    first_component: HashSet<char>,
    second_component: HashSet<char>
}

impl Rucksack {
    fn new() -> Rucksack {
        Rucksack {
            first_component: HashSet::new(),
            second_component: HashSet::new()
        }
    }
}

peg::parser! {
    grammar parser() for str {
        rule item() -> char
          = c:$(['a'..='z' | 'A'..='Z']) {
            c.parse().unwrap()
        }
        
        rule rucksack() -> Rucksack
          = n:$(item()*) {
            let mut res = Rucksack::new();
            let len = n.len();
            for (i, cur_item) in n.chars().enumerate() {
                if 2*i < len {
                    res.first_component.insert(cur_item);
                } else {
                    res.second_component.insert(cur_item);
                }
            }
            res
        }

        pub(crate) rule parse() -> Vec<Rucksack>
          = l:(rucksack()**"\n") { l }
    }
}

fn priority(ch: char) -> isize {
    let val = ch as u8;
    if val >= ('a' as u8) && val <= ('z' as u8) {
        (val-('a' as u8)+1).into()
    } else {
        (val-('A' as u8)+27).into()
    }
}

fn sum_priorities_of_common_parts(rucksacks: &[Rucksack]) -> isize {
    rucksacks.iter().map(|r| {
        for cur_item in r.first_component.iter() {
            if r.second_component.contains(cur_item) {
                return priority(*cur_item);
            }
        }
        0
    }).sum()
}

fn sum_priorities_of_badges(rucksacks: &[Rucksack]) -> isize {
    rucksacks.iter()
        .enumerate()
        .collect::<Vec<_>>()
        .group_by(|pr1, pr2| pr1.0/3 == pr2.0/3)
        .map(|rs| {
            if rs.len() < 3 { 0 } //Occurs because of empty lines
            else {
                let possible_badges: HashSet<_> =
                    rs[0].1.first_component.union(&rs[0].1.second_component).map(|ch| ch.clone()).collect::<HashSet<_>>()
                    .intersection(&rs[1].1.first_component.union(&rs[1].1.second_component).map(|ch| ch.clone()).collect::<HashSet<_>>())
                    .map(|ch| ch.clone()).collect::<HashSet<_>>()
                    .intersection(&rs[2].1.first_component.union(&rs[2].1.second_component).map(|ch| ch.clone()).collect::<HashSet<_>>())
                    .map(|ch| ch.clone()).collect();
                possible_badges.iter().next().map_or(0, |ch| priority(*ch))
            }
        })
        .sum()
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let rucksacks = parser::parse(&fs::read_to_string("in.txt")?)?;

    println!("Part 1: {}", sum_priorities_of_common_parts(&rucksacks));
    println!("Part 2: {}", sum_priorities_of_badges(&rucksacks));
    Ok(())
}