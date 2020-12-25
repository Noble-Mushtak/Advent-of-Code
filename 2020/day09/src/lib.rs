use std::cmp::Ordering;
use std::error::Error;
use std::fs;
use std::num::ParseIntError;
use std::str::FromStr;

#[derive(PartialEq, Debug)]
struct XmasData(Vec<usize>);

impl FromStr for XmasData {
    type Err = ParseIntError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        input
            .lines()
            .map(str::parse)
            .collect::<Result<_, _>>()
            .map(XmasData)
    }
}

impl XmasData {
    fn find_first_invalid(&self, &preamble_len: &usize) -> Option<usize> {
        (preamble_len..self.0.len()).find(|&i| {
            let slice = &self.0[i - preamble_len..i];
            let desired_val = self.0[i];
            !slice
                .iter()
                .map(|x| slice.iter().map(move |y| (x, y)))
                .flatten()
                .any(|(x, y)| x + y == desired_val)
        })
    }

    fn find_slice_with_sum(&self, &desired_sum: &usize) -> Option<&[usize]> {
        for i in 0..self.0.len() {
            let mut sum = 0;
            for j in i..self.0.len() {
                sum += self.0[j];
                match sum.cmp(&desired_sum) {
                    Ordering::Equal => return Some(&self.0[i..j + 1]),
                    Ordering::Greater => continue,
                    Ordering::Less => (),
                };
            }
        }
        None
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let data: XmasData = fs::read_to_string("in.txt")?.parse()?;

    let answer1 = data.0[data
        .find_first_invalid(&25)
        .expect("Answer for part 1 not found")];
    println!("Part 1: {}", answer1);

    let desired_slice = data
        .find_slice_with_sum(&answer1)
        .expect("Answer for part 2 not found");
    println!(
        "Part 2: {}",
        desired_slice.iter().min().expect("Slice can not be empty")
            + desired_slice.iter().max().expect("Slice can not be empty")
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn example_data() -> XmasData {
        XmasData(vec![
            35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309,
            576,
        ])
    }

    #[test]
    fn test_parse() {
        assert_eq!("1\n2\n3\n".parse(), Ok(XmasData(vec![1, 2, 3])));
        assert!("1\n2\ndfg\n".parse::<XmasData>().is_err());
    }

    #[test]
    fn test_invalid() {
        let data = example_data();
        let invalid_idx = data.find_first_invalid(&5).unwrap();
        assert_eq!(data.0[invalid_idx], 127);
    }

    #[test]
    fn test_slice() {
        let data = example_data();
        assert_eq!(data.find_slice_with_sum(&127), Some(&[15, 25, 47, 40][..]));
    }
}
