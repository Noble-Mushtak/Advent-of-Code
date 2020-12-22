#![feature(str_split_once)]

use std::fs;
use std::error::Error;
use std::str::FromStr;
use snafu::Snafu;

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
    ParseLineError { line: String },
    #[snafu(display("The needed char in \"{}\" could not be parsed", line))]
    ParseCharError { line: String },
    #[snafu(display("The minimum number in \"{}\" could not be parsed", line))]
    ParseMinError { line: String },
    #[snafu(display("The maximum number in \"{}\" could not be parsed", line))]
    ParseMaxError { line: String },
}

impl FromStr for PasswordPolicy {
    type Err = ParsePolicyError;
    
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        use ParsePolicyError::*;
        
        let (min_str, rest) = input.split_once("-").ok_or(ParseLineError { line: input.to_string() })?;
        let (max_str, rest) = rest.split_once(" ").ok_or(ParseLineError { line: input.to_string() })?;
        let (needed_char_str, password) = rest.split_once(": ").ok_or(ParseLineError { line: input.to_string() })?;
        let mut needed_char_iter = needed_char_str.chars();
        let needed_char = needed_char_iter.next().ok_or(ParseCharError{ line: input.to_string() })?;
        match needed_char_iter.next() {
            None => (),
            Some(_) => Err(ParseCharError { line: input.to_string() })?
        };
        Ok(PasswordPolicy {
            min_occurs: min_str.parse().map_err(|_| ParseMinError { line: input.to_string() })?,
            max_occurs: max_str.parse().map_err(|_| ParseMaxError { line: input.to_string() })?,
            needed_char: needed_char,
            password: password.to_string(),
        })
    }
}

#[derive(PartialEq, Debug)]
struct PasswordPolicies(Vec<PasswordPolicy>);

impl FromStr for PasswordPolicies {
    type Err = ParsePolicyError;
    
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(PasswordPolicies(
            input.trim()
                 .split("\n")
                 .map(|line| line.parse())
                 .collect::<Result<_, _>>()?
        ))
    }
}

fn valid_policy1(policy: &PasswordPolicy) -> bool {
    let num_matches = policy.password.matches(policy.needed_char).count();
    policy.min_occurs <= num_matches && num_matches <= policy.max_occurs
}

fn valid_policy2(policy: &PasswordPolicy) -> bool {
    let password_chars: Vec<_> = policy.password.chars().collect();
    (password_chars[policy.min_occurs-1] == policy.needed_char) ^
    (password_chars[policy.max_occurs-1] == policy.needed_char)
}

fn calc_answer<F>(policies: &PasswordPolicies, valid_policy: F) -> usize
    where F: Fn(&PasswordPolicy) -> bool {
    policies.0.iter()
              .map(|policy| -> usize { valid_policy(&policy).into() })
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
            PasswordPolicy { min_occurs: 1, max_occurs: 3, needed_char: 'a', password: "abcde".to_string() },
            PasswordPolicy { min_occurs: 1, max_occurs: 3, needed_char: 'b', password: "cdefg".to_string() },
            PasswordPolicy { min_occurs: 2, max_occurs: 9, needed_char: 'c', password: "ccccccccc".to_string() },
        ])
    }
    
    #[test]
    fn test_parse_policy() {
        use ParsePolicyError::*;

        let query = "2-9 cernui".to_string();
        assert_eq!(query.parse::<PasswordPolicy>(), Err(ParseLineError { line: query }));
        let query = "dsf rf9gv8 cernui".to_string();
        assert_eq!(query.parse::<PasswordPolicy>(), Err(ParseLineError { line: query }));
        let query = "10-9 : sdugu".to_string();
        assert_eq!(query.parse::<PasswordPolicy>(), Err(ParseCharError { line: query }));
        let query = "10-9 fdiuh: sdugu".to_string();
        assert_eq!(query.parse::<PasswordPolicy>(), Err(ParseCharError { line: query }));
        let query = "c-9 d: sdugu".to_string();
        assert_eq!(query.parse::<PasswordPolicy>(), Err(ParseMinError { line: query }));
        let query = "10-9djfh d: sdugu".to_string();
        assert_eq!(query.parse::<PasswordPolicy>(), Err(ParseMaxError { line: query }));
        assert_eq!("1-3 a: abcde".parse(), Ok(PasswordPolicy { min_occurs: 1, max_occurs: 3, needed_char: 'a', password: "abcde".to_string() }));
        assert_eq!("2-9 c: cccccccc".parse(), Ok(PasswordPolicy { min_occurs: 2, max_occurs: 9, needed_char: 'c', password: "cccccccc".to_string() }));
    }
    
    #[test]
    fn test_parse_policies() {
        assert!("2-9 cernui\n1-3 b: cdefg\n".parse::<PasswordPolicies>().is_err());
        assert_eq!("1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc\n".parse::<PasswordPolicies>(), Ok(example_policies()));
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
