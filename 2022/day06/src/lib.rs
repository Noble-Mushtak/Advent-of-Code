use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::error::Error;
use std::fs;

peg::parser! {
    grammar parser() for str {
        pub(crate) rule parse() -> String
          = s:$(['a'..='z']+) "\n"? { s.to_string() }
    }
}

fn find_first_distinct(message: &str, num_chs: usize) -> Option<usize> {
    let chs = message.chars().collect::<Vec<_>>();
    let mut cnt: HashMap<char, usize> = HashMap::new();
    for (i, ch) in chs.iter().enumerate() {
        *(cnt.entry(*ch).or_insert(0)) += 1;
        if i >= num_chs {
            if let Entry::Occupied(o) = cnt.entry(chs[i-num_chs]).and_modify(|cnt| { *cnt -= 1 }) {
                if o.get() == &0 {
                    o.remove_entry();
                }
            }
        }
        if cnt.len() >= num_chs {
            return Some(i+1);
        }
    }
    None
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let message = parser::parse(&fs::read_to_string("in.txt")?)?;

    println!("Part 1: {}", find_first_distinct(&message, 4).unwrap());
    println!("Part 2: {}", find_first_distinct(&message, 14).unwrap());
    Ok(())
}