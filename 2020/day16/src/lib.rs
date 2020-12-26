use itertools::Itertools;
use std::collections::{HashMap, HashSet, VecDeque};
use std::error::Error;
use std::fs;
use std::ops::RangeInclusive;

#[derive(PartialEq, Debug)]
struct Field {
    rule_name: String,
    lower_range: RangeInclusive<usize>,
    upper_range: RangeInclusive<usize>,
}

impl Field {
    fn contains(&self, val: &usize) -> bool {
        self.lower_range.contains(val) || self.upper_range.contains(val)
    }
}

#[derive(PartialEq, Debug)]
struct Ticket(Vec<usize>);

#[derive(PartialEq, Debug)]
struct Collection {
    fields: Vec<Field>,
    my_ticket: Ticket,
    nearby_tickets: Vec<Ticket>,
}

peg::parser! {
    grammar parser() for str {
        rule usize() -> usize
          = n:$(['0'..='9']+) {
            n.parse().unwrap()
        }

        rule field() -> Field
          = name:$(['a'..='z' | 'A'..='Z' | ' ']+)
            ": "
            min1:usize() "-" max1:usize()
            " or "
            min2:usize() "-" max2:usize() {
            Field {
                rule_name: name.to_string(),
                lower_range: (min1..=max1),
                upper_range: (min2..=max2),
            }
        }

        rule ticket() -> Ticket
          = nums:(usize()**",") {
            Ticket(nums)
        }

        pub(crate) rule parse() -> Collection
          = fields:(field()**"\n")
            "\n\n"
            "your ticket:\n" my_ticket:ticket()
            "\n\n"
            "nearby tickets:\n" nearby_tickets:(ticket()**"\n") {
            Collection {
                fields,
                my_ticket,
                nearby_tickets,
            }
        }
    }
}

impl Collection {
    fn is_valid(&self, val: &usize) -> bool {
        self.fields.iter().any(|field| field.contains(val))
    }

    fn invalid_vals(&self) -> impl Iterator<Item = &usize> + '_ {
        self.nearby_tickets
            .iter()
            .map(|tick| tick.0.iter())
            .flatten()
            .filter(move |val| !self.is_valid(val))
    }

    fn valid_nearbys(&self) -> impl Iterator<Item = &Ticket> + '_ {
        self.nearby_tickets
            .iter()
            .filter(move |tick| tick.0.iter().all(|val| self.is_valid(val)))
    }

    fn possible_fields<'a>(&'a self, nums: &'a [usize]) -> impl Iterator<Item = usize> + 'a {
        self.fields
            .iter()
            .enumerate()
            .filter(move |(_, field)| nums.iter().all(|num| field.contains(num)))
            .map(|(i, _)| i)
    }

    fn find_valid_field_perm(&self) -> Option<Vec<usize>> {
        let valid_nearbys = self.valid_nearbys().collect::<Vec<_>>();

        let source = 2 * self.fields.len();
        let sink = 2 * self.fields.len() + 1;
        let mut capacities = HashMap::new();
        let mut edges: HashMap<_, Vec<usize>> = HashMap::new();
        let mut add_edge = |v1, v2| match edges.get_mut(&v1) {
            Some(vec) => {
                vec.push(v2);
            }
            None => {
                edges.insert(v1, vec![v2]);
            }
        };
        let mut add_edge_with_capacity = |v1, v2| {
            add_edge(v1, v2);
            add_edge(v2, v1);
            capacities.insert((v1, v2), 1);
        };
        for i in 0..self.fields.len() {
            add_edge_with_capacity(source, i);
            add_edge_with_capacity(self.fields.len() + i, sink);

            let nums = valid_nearbys
                .iter()
                .map(|tick| tick.0[i])
                .collect::<Vec<_>>();
            for j in self.possible_fields(&nums[..]) {
                add_edge_with_capacity(i, self.fields.len() + j);
            }
        }

        let mut flow = HashMap::new();
        let find_path_with_flow = |flow: &HashMap<_, _>| {
            let mut visited = HashSet::new();
            let mut last_vert = HashMap::new();
            let mut queue = VecDeque::new();

            visited.insert(source);
            queue.push_back(source);
            while let Some(cur_vert) = queue.pop_front() {
                if cur_vert == sink {
                    let mut path = vec![sink];
                    let mut cur_vert = &sink;
                    while let Some(prev_vert) = last_vert.get(cur_vert) {
                        path.insert(0, *prev_vert);
                        cur_vert = prev_vert;
                    }
                    return Some(path);
                }
                for &next_vert in edges.get(&cur_vert).unwrap_or(&vec![]) {
                    if !visited.contains(&next_vert)
                        && flow.get(&(cur_vert, next_vert)).unwrap_or(&0)
                            < capacities.get(&(cur_vert, next_vert)).unwrap_or(&0)
                    {
                        last_vert.insert(next_vert, cur_vert);
                        visited.insert(next_vert);
                        queue.push_back(next_vert);
                    }
                }
            }
            None
        };

        while let Some(path) = find_path_with_flow(&flow) {
            for (&v1, &v2) in path.iter().tuple_windows() {
                match flow.get_mut(&(v1, v2)) {
                    Some(val) => *val += 1,
                    None => {
                        flow.insert((v1, v2), 1);
                    }
                }
                match flow.get_mut(&(v2, v1)) {
                    Some(val) => *val -= 1,
                    None => {
                        flow.insert((v2, v1), -1);
                    }
                }
            }
        }

        let mut perm = vec![None; self.fields.len()];
        for ((i, j), fl) in flow {
            if fl > 0 {
                if let Some(val) = perm.get_mut(i) {
                    *val = Some(j - self.fields.len());
                }
            }
        }
        perm.into_iter().collect::<Option<_>>()
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let collection = parser::parse(&fs::read_to_string("in.txt")?[..])?;

    println!("Part 1: {}", collection.invalid_vals().sum::<usize>());
    let field_perm = collection
        .find_valid_field_perm()
        .expect("Could not find answer for part 2");
    println!(
        "Part 2: {}",
        collection
            .my_ticket
            .0
            .iter()
            .enumerate()
            .filter(|&(i, _)| collection.fields[field_perm[i]]
                .rule_name
                .starts_with("departure"))
            .map(|(_, val)| val)
            .fold(1, std::ops::Mul::mul)
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn example_data() -> Collection {
        Collection {
            fields: vec![
                Field {
                    rule_name: String::from("class"),
                    lower_range: (1..=3),
                    upper_range: (5..=7),
                },
                Field {
                    rule_name: String::from("row"),
                    lower_range: (6..=11),
                    upper_range: (33..=44),
                },
                Field {
                    rule_name: String::from("seat"),
                    lower_range: (13..=40),
                    upper_range: (45..=50),
                },
            ],
            my_ticket: Ticket(vec![7, 1, 14]),
            nearby_tickets: vec![
                Ticket(vec![7, 3, 47]),
                Ticket(vec![40, 4, 50]),
                Ticket(vec![55, 2, 20]),
                Ticket(vec![38, 6, 12]),
            ],
        }
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            parser::parse(
                "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12"
            ),
            Ok(example_data())
        );
    }

    #[test]
    fn test_invalid() {
        assert_eq!(
            example_data().invalid_vals().collect::<Vec<_>>(),
            vec![&4, &55, &12]
        );
    }

    #[test]
    fn test_valid() {
        assert_eq!(
            example_data().valid_nearbys().collect::<Vec<_>>(),
            vec![&Ticket(vec![7, 3, 47])]
        );
    }

    fn example_data2() -> Collection {
        parser::parse(
            "class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9",
        )
        .unwrap()
    }

    #[test]
    fn test_fields() {
        let coll = example_data2();
        let nums1 = vec![3, 15, 5];
        assert_eq!(coll.possible_fields(&nums1).collect::<Vec<_>>(), vec![1]);
        let nums2 = vec![9, 1, 14];
        assert_eq!(coll.possible_fields(&nums2).collect::<Vec<_>>(), vec![0, 1]);
    }

    #[test]
    fn test_perm() {
        let coll = example_data2();
        assert_eq!(coll.find_valid_field_perm(), Some(vec![1, 0, 2]));
    }
}
