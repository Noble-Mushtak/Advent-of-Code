#![feature(str_split_once)]

use snafu::Snafu;
use std::collections::{HashMap, HashSet, VecDeque};
use std::convert::From;
use std::error::Error;
use std::fs;
use std::str::FromStr;

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
struct Color(String);

impl From<String> for Color {
    fn from(input: String) -> Self {
        Self(input)
    }
}

#[derive(PartialEq, Debug)]
struct ColorQuantity {
    color: Color,
    quantity: usize,
}

#[derive(PartialEq, Debug, Snafu)]
enum ParseQuantityError {
    #[snafu(display("\"{}\" does not end in \" bags\" or \" bag\".", input))]
    BadEnding { input: String },
    #[snafu(display("\"{}\" does not contain a space.", input))]
    NoSpace { input: String },
    #[snafu(display("\"{}\" does not begin with an integer.", input))]
    NonInteger { input: String },
}

impl FromStr for ColorQuantity {
    type Err = ParseQuantityError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let input = if input.ends_with(" bags") {
            input.get(..input.len() - 5).unwrap()
        } else if input.ends_with(" bag") {
            input.get(..input.len() - 4).unwrap()
        } else {
            return Err(ParseQuantityError::BadEnding {
                input: input.to_string(),
            });
        };
        let (quant_str, color) =
            input
                .split_once(" ")
                .ok_or_else(|| ParseQuantityError::NoSpace {
                    input: input.to_string(),
                })?;
        let quantity = quant_str
            .parse()
            .map_err(|_| ParseQuantityError::NonInteger {
                input: input.to_string(),
            })?;
        Ok(ColorQuantity {
            color: color.to_string().into(),
            quantity,
        })
    }
}

#[derive(PartialEq, Debug)]
struct ColorGraph(HashMap<Color, Vec<ColorQuantity>>);

#[derive(PartialEq, Debug, Snafu)]
enum ParseGraphError {
    #[snafu(display("{}", err))]
    Quantity { err: ParseQuantityError },
    #[snafu(display("\"{}\" is not a valid rule.", line))]
    BadRuleFormat { line: String },
}

impl From<ParseQuantityError> for ParseGraphError {
    fn from(err: ParseQuantityError) -> ParseGraphError {
        ParseGraphError::Quantity { err }
    }
}

impl ColorGraph {
    fn new() -> Self {
        ColorGraph(HashMap::new())
    }

    fn parse_rule_and_update_graph(&mut self, input: &str) -> Result<(), ParseGraphError> {
        let (color, quantities) =
            input
                .split_once(" bags contain ")
                .ok_or_else(|| ParseGraphError::BadRuleFormat {
                    line: input.to_string(),
                })?;
        let quantities = if quantities.ends_with('.') {
            quantities.get(..quantities.len() - 1).unwrap()
        } else {
            return Err(ParseGraphError::BadRuleFormat {
                line: input.to_string(),
            });
        };
        self.0.insert(
            color.to_string().into(),
            if quantities == "no other bags" {
                vec![]
            } else {
                quantities
                    .split(", ")
                    .map(str::parse)
                    .collect::<Result<_, _>>()?
            },
        );
        Ok(())
    }
}

impl FromStr for ColorGraph {
    type Err = ParseGraphError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut graph = ColorGraph::new();
        for line in input.trim().split('\n') {
            graph.parse_rule_and_update_graph(line)?
        }
        Ok(graph)
    }
}

impl ColorGraph {
    fn update_dual(&mut self, (key, quantities): (&Color, &Vec<ColorQuantity>)) {
        for quantity in quantities {
            let new_quantity = ColorQuantity {
                color: key.clone(),
                quantity: quantity.quantity,
            };
            match self.0.get_mut(&quantity.color) {
                Some(vec) => {
                    vec.push(new_quantity);
                }
                None => {
                    self.0.insert(quantity.color.clone(), vec![new_quantity]);
                }
            };
        }
    }

    fn dual(&self) -> Self {
        let mut dual_graph = ColorGraph::new();
        for kv_pair in self.0.iter() {
            dual_graph.update_dual(kv_pair);
        }
        dual_graph
    }

    fn reachable(&self, color: &Color) -> HashSet<Color> {
        let mut reachable_colors = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(color.clone());
        while let Some(cur_color) = queue.pop_front() {
            for quantity in self.0.get(&cur_color).iter().copied().flatten() {
                if !reachable_colors.contains(&quantity.color) {
                    queue.push_back(quantity.color.clone());
                }
            }
            reachable_colors.insert(cur_color);
        }
        reachable_colors
    }

    fn bags_containing(&self, color: &Color) -> usize {
        self.0
            .get(&color)
            .iter()
            .copied()
            .flatten()
            .map(|quantity| quantity.quantity * (1 + self.bags_containing(&quantity.color)))
            .sum()
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let graph: ColorGraph = fs::read_to_string("in.txt")?.parse()?;
    let desired_color = Color(String::from("shiny gold"));

    //Subtract 1: Don't count desired_color
    println!(
        "Part 1: {}",
        graph.dual().reachable(&desired_color).len() - 1
    );

    println!("Part 2: {}", graph.bags_containing(&desired_color));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integration_test() {
        let desired_color = Color(String::from("shiny gold"));
        let example_graph = "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."
            .parse::<ColorGraph>()
            .unwrap();
        assert_eq!(example_graph.dual().reachable(&desired_color).len() - 1, 4);
        assert_eq!(example_graph.bags_containing(&desired_color), 32);
    }
}
