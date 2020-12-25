use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::num::ParseIntError;
use std::str::FromStr;

#[derive(PartialEq, Debug)]
struct JoltageData(Vec<usize>);

impl FromStr for JoltageData {
    type Err = ParseIntError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        input
            .lines()
            .map(str::parse)
            .collect::<Result<_, _>>()
            .map(JoltageData)
    }
}

impl JoltageData {
    fn relative_diff_distribution(&mut self) -> HashMap<usize, usize> {
        self.0.sort_unstable();

        let mut distribution = HashMap::new();
        for i in 1..self.0.len() {
            let diff = self.0[i] - self.0[i - 1];
            assert!((1..=3).contains(&diff));

            match distribution.get_mut(&diff) {
                Some(val) => *val += 1,
                None => {
                    distribution.insert(diff, 1);
                }
            };
        }
        distribution
    }

    fn count_arrangements(&self) -> usize {
        //Assume sorted

        let mut num_arrangements = vec![1];
        for i in 1..self.0.len() {
            num_arrangements.push(
                (0..i)
                    .rev()
                    .take_while(|&j| self.0[i] - self.0[j] <= 3)
                    .map(|j| num_arrangements[j])
                    .sum(),
            );
        }
        num_arrangements[self.0.len() - 1]
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let mut data: JoltageData = fs::read_to_string("in.txt")?.parse()?;
    data.0.push(0);

    let distribution = data.relative_diff_distribution();
    let num1s = distribution.get(&1).unwrap_or(&0);
    let num3s = distribution.get(&3).unwrap_or(&0) + 1;
    println!("Part 1: {}", num1s * num3s);
    println!("Part 2: {}", data.count_arrangements());

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!("1\n2\n3\n".parse(), Ok(JoltageData(vec![1, 2, 3])));
        assert!("1\n2\ndfg\n".parse::<JoltageData>().is_err());
    }

    fn example_data1() -> JoltageData {
        JoltageData(vec![16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4])
    }

    fn example_data2() -> JoltageData {
        JoltageData(vec![
            28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35,
            8, 17, 7, 9, 4, 2, 34, 10, 3,
        ])
    }

    #[test]
    fn test_calc_distr() {
        let mut data1 = example_data1();
        assert_eq!(
            data1.relative_diff_distribution(),
            [(1, 6), (3, 4)].iter().cloned().collect()
        );
        let mut data2 = example_data2();
        assert_eq!(
            data2.relative_diff_distribution(),
            [(1, 21), (3, 9)].iter().cloned().collect()
        );
    }

    #[test]
    fn test_count_arrangements() {
        let mut data1 = example_data1();
        data1.0.push(0);
        data1.0.sort_unstable();
        assert_eq!(data1.count_arrangements(), 8);

        let mut data2 = example_data2();
        data2.0.push(0);
        data2.0.sort_unstable();
        assert_eq!(data2.count_arrangements(), 19208);
    }
}
