use std::error::Error;
use std::fs;

peg::parser! {
    grammar parser() for str {
        rule usize() -> usize
          = n:$(['0'..='9']+) {
            n.parse().unwrap()
        }

        rule elf() -> Vec<usize>
          = l:(usize()**"\n") { l }

        pub(crate) rule parse() -> Vec<Vec<usize>>
          = l:(elf()**"\n\n") _:"\n"? { l }
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let elves = parser::parse(&fs::read_to_string("in.txt")?)?;
    
    let mut total_calories_per_elf = elves.iter().map(|v| v.iter().sum::<usize>()).collect::<Vec<_>>();
    total_calories_per_elf.sort();

    println!("Part 1: {}", total_calories_per_elf[total_calories_per_elf.len()-1]);
    println!("Part 2: {}", total_calories_per_elf[total_calories_per_elf.len()-3]+total_calories_per_elf[total_calories_per_elf.len()-2]+total_calories_per_elf[total_calories_per_elf.len()-1]);
    Ok(())
}