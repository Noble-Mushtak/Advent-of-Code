#![feature(str_split_once)]

use snafu::Snafu;
use std::error::Error;
use std::fs;
use std::str::FromStr;

#[derive(PartialEq, Debug)]
struct PasswordPolicy {
    min_occurs: usize,
    max_occurs: usize,
    needed_char: char,
    password: String,
}

#[derive(PartialEq, Debug, Snafu)]
enum ParsePolicyError {
    #[snafu(display("\"{}\" could not be parsed", line))]
    Line { line: String },
    #[snafu(display("The needed char in \"{}\" could not be parsed", line))]
    Char { line: String },
    #[snafu(display("The minimum number in \"{}\" could not be parsed", line))]
    Min { line: String },
    #[snafu(display("The maximum number in \"{}\" could not be parsed", line))]
    Max { line: String },
}

impl FromStr for PasswordPolicy {
    type Err = ParsePolicyError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        use ParsePolicyError::*;

        let (min_str, rest) = input.split_once("-").ok_or(Line {
            line: input.to_string(),
        })?;
        let (max_str, rest) = rest.split_once(" ").ok_or(Line {
            line: input.to_string(),
        })?;
        let (needed_char_str, password) = rest.split_once(": ").ok_or(Line {
            line: input.to_string(),
        })?;
        let mut needed_char_iter = needed_char_str.chars();
        let needed_char = needed_char_iter.next().ok_or(Char {
            line: input.to_string(),
        })?;
        match needed_char_iter.next() {
            None => (),
            Some(_) => {
                return Err(Char {
                    line: input.to_string(),
                });
            }
        };
        Ok(PasswordPolicy {
            min_occurs: min_str.parse().map_err(|_| Min {
                line: input.to_string(),
            })?,
            max_occurs: max_str.parse().map_err(|_| Max {
                line: input.to_string(),
            })?,
            needed_char,
            password: password.to_string(),
        })
    }
}

#[derive(PartialEq, Debug)]
struct PasswordPolicies(Vec<PasswordPolicy>);

impl FromStr for PasswordPolicies {
    type Err = ParsePolicyError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        input
            .lines()
            .map(|line| line.parse())
            .collect::<Result<_, _>>()
            .map(PasswordPolicies)
    }
}

fn valid_policy1(policy: &PasswordPolicy) -> bool {
    let num_matches = policy.password.matches(policy.needed_char).count();
    policy.min_occurs <= num_matches && num_matches <= policy.max_occurs
}

fn valid_policy2(policy: &PasswordPolicy) -> bool {
    let password_chars: Vec<_> = policy.password.chars().collect();
    (password_chars[policy.min_occurs - 1] == policy.needed_char)
        ^ (password_chars[policy.max_occurs - 1] == policy.needed_char)
}

fn calc_answer<F>(policies: &PasswordPolicies, valid_policy: F) -> usize
where
    F: Fn(&PasswordPolicy) -> bool,
{
    policies
        .0
        .iter()
        .map(|policy| valid_policy(&policy) as usize)
        .sum()
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let policies: PasswordPolicies = fs::read_to_string("in.txt")?.parse()?;
    println!("Part 1: {}", calc_answer(&policies, valid_policy1));
    println!("Part 2: {}", calc_answer(&policies, valid_policy2));
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn example_policies() -> PasswordPolicies {
        PasswordPolicies(vec![
            PasswordPolicy {
                min_occurs: 1,
                max_occurs: 3,
                needed_char: 'a',
                password: "abcde".to_string(),
            },
            PasswordPolicy {
                min_occurs: 1,
                max_occurs: 3,
                needed_char: 'b',
                password: "cdefg".to_string(),
            },
            PasswordPolicy {
                min_occurs: 2,
                max_occurs: 9,
                needed_char: 'c',
                password: "ccccccccc".to_string(),
            },
        ])
    }

    #[test]
    fn test_parse_policy() {
        use ParsePolicyError::*;

        let query = "2-9 cernui".to_string();
        assert_eq!(query.parse::<PasswordPolicy>(), Err(Line { line: query }));
        let query = "dsf rf9gv8 cernui".to_string();
        assert_eq!(query.parse::<PasswordPolicy>(), Err(Line { line: query }));
        let query = "10-9 : sdugu".to_string();
        assert_eq!(query.parse::<PasswordPolicy>(), Err(Char { line: query }));
        let query = "10-9 fdiuh: sdugu".to_string();
        assert_eq!(query.parse::<PasswordPolicy>(), Err(Char { line: query }));
        let query = "c-9 d: sdugu".to_string();
        assert_eq!(query.parse::<PasswordPolicy>(), Err(Min { line: query }));
        let query = "10-9djfh d: sdugu".to_string();
        assert_eq!(query.parse::<PasswordPolicy>(), Err(Max { line: query }));
        assert_eq!(
            "1-3 a: abcde".parse(),
            Ok(PasswordPolicy {
                min_occurs: 1,
                max_occurs: 3,
                needed_char: 'a',
                password: "abcde".to_string()
            })
        );
        assert_eq!(
            "2-9 c: cccccccc".parse(),
            Ok(PasswordPolicy {
                min_occurs: 2,
                max_occurs: 9,
                needed_char: 'c',
                password: "cccccccc".to_string()
            })
        );
    }

    #[test]
    fn test_parse_policies() {
        assert!("2-9 cernui\n1-3 b: cdefg\n"
            .parse::<PasswordPolicies>()
            .is_err());
        assert_eq!(
            "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc\n".parse::<PasswordPolicies>(),
            Ok(example_policies())
        );
    }

    #[test]
    fn test_valid_policy1() {
        let examples = example_policies();
        assert!(valid_policy1(&examples.0[0]));
        assert!(!valid_policy1(&examples.0[1]));
        assert!(valid_policy1(&examples.0[2]));
    }

    #[test]
    fn test_valid_policy2() {
        let examples = example_policies();
        assert!(valid_policy2(&examples.0[0]));
        assert!(!valid_policy2(&examples.0[1]));
        assert!(!valid_policy2(&examples.0[2]));
    }

    #[test]
    fn test_calc_answer() {
        assert_eq!(calc_answer(&example_policies(), valid_policy1), 2);
        assert_eq!(calc_answer(&example_policies(), valid_policy2), 1);
    }
}
