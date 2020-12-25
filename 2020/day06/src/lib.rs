use std::collections::HashSet;
use std::error::Error;
use std::fs;
use std::str::FromStr;

#[derive(PartialEq, Debug)]
struct Person(HashSet<char>);

impl FromStr for Person {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(Person(input.chars().collect()))
    }
}

#[derive(PartialEq, Debug)]
struct Group(Vec<Person>);

impl FromStr for Group {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        input
            .lines()
            .map(str::parse)
            .collect::<Result<_, _>>()
            .map(Group)
    }
}

#[derive(PartialEq, Debug)]
struct Groups(Vec<Group>);

impl FromStr for Groups {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        input
            .split("\n\n")
            .map(str::parse)
            .collect::<Result<_, _>>()
            .map(Groups)
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let groups: Groups = fs::read_to_string("in.txt")?.parse()?;

    println!(
        "Part 1: {}",
        groups
            .0
            .iter()
            .map(|group| {
                group
                    .0
                    .iter()
                    .fold(HashSet::new(), |acc, person| {
                        acc.union(&person.0).copied().collect()
                    })
                    .len()
            })
            .sum::<usize>()
    );

    println!(
        "Part 2: {}",
        groups
            .0
            .iter()
            .map(|group| {
                let mut people_in_group = group.0.iter();
                let first_person = people_in_group
                    .next()
                    .expect("Every group must contain at least one person");
                people_in_group
                    .fold(first_person.0.clone(), |acc, person| {
                        acc.intersection(&person.0).copied().collect()
                    })
                    .len()
            })
            .sum::<usize>()
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_person() {
        assert_eq!(
            "xbca".parse::<Person>(),
            Ok(Person(['a', 'b', 'c', 'x'].iter().cloned().collect()))
        );
        assert_eq!(
            "".parse::<Person>(),
            Ok(Person([].iter().cloned().collect()))
        );
        assert_eq!(
            "f".parse::<Person>(),
            Ok(Person(['f'].iter().cloned().collect()))
        );
    }

    #[test]
    fn test_group() {
        assert_eq!(
            "abcdgjus".parse(),
            Ok(Group(vec!["abcdgjus".parse::<Person>().unwrap()]))
        );
        assert_eq!(
            "abcx\nabcy".parse(),
            Ok(Group(vec![
                "abcx".parse::<Person>().unwrap(),
                "abcy".parse::<Person>().unwrap()
            ]))
        );
    }

    fn example_groups() -> Groups {
        "abc

a
b
c

ab
ac

a
a
a
a

b"
        .parse()
        .unwrap()
    }

    #[test]
    fn test_groups() {
        let groups1 = example_groups();
        assert!(groups1.0.len() == 5);
        assert!(groups1.0[0].0.len() == 1);
        assert!(groups1.0[1].0.len() == 3);
        assert!(groups1.0[2].0.len() == 2);
        assert!(groups1.0[3].0.len() == 4);
        assert!(groups1.0[4].0.len() == 1);
    }
}
