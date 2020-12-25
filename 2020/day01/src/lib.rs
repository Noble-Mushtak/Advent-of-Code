use snafu::Snafu;
use std::error::Error;
use std::fs;
use std::str::FromStr;

#[derive(PartialEq, Debug)]
struct ExpenseReport {
    expenses: Vec<u64>,
}

impl ExpenseReport {
    fn new(v: Vec<u64>) -> ExpenseReport {
        ExpenseReport { expenses: v }
    }
}

#[derive(PartialEq, Debug, Snafu)]
enum ParseReportError {
    #[snafu(display("One of the lines in the input file could not be parsed"))]
    ParseLineError,
}

impl FromStr for ExpenseReport {
    type Err = ParseReportError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.lines()
            .map(|line| line.parse().map_err(|_| Self::Err::ParseLineError))
            .collect::<Result<_, _>>()
            .map(ExpenseReport::new)
    }
}

#[derive(PartialEq, Debug, Snafu)]
enum CalcError {
    #[snafu(display("Pair summing to 2020 not found"))]
    PairNotFound,
    #[snafu(display("Triplet summing to 2020 not found"))]
    TripletNotFound,
}

fn calc_p1_answer(report: &ExpenseReport) -> Result<u64, CalcError> {
    for n1 in &report.expenses {
        for n2 in &report.expenses {
            if n1 + n2 == 2020 {
                return Ok(n1 * n2);
            }
        }
    }
    Err(CalcError::PairNotFound)
}

fn calc_p2_answer(report: &ExpenseReport) -> Result<u64, CalcError> {
    for n1 in &report.expenses {
        for n2 in &report.expenses {
            for n3 in &report.expenses {
                if n1 + n2 + n3 == 2020 {
                    return Ok(n1 * n2 * n3);
                }
            }
        }
    }
    Err(CalcError::TripletNotFound)
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let report: ExpenseReport = fs::read_to_string("in.txt")?.parse()?;
    println!("Part 1: {}", calc_p1_answer(&report)?);
    println!("Part 2: {}", calc_p2_answer(&report)?);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_report() {
        assert_eq!("".parse(), Ok(ExpenseReport::new(vec![])));
        assert_eq!("1\n".parse(), Ok(ExpenseReport::new(vec![1])));
        assert_eq!("1".parse(), Ok(ExpenseReport::new(vec![1])));
        assert_eq!("2\n3\n".parse(), Ok(ExpenseReport::new(vec![2, 3])));
        assert_eq!("2\n3".parse(), Ok(ExpenseReport::new(vec![2, 3])));
        assert_eq!(
            "1721\n979\n366\n299\n".parse(),
            Ok(ExpenseReport::new(vec![1721, 979, 366, 299]))
        );
        assert!("efrgfhj\n12349".parse::<ExpenseReport>().is_err());
        assert!("1\n2\nd\n2\n4\n".parse::<ExpenseReport>().is_err());
    }

    #[test]
    fn test_p1_answer() {
        assert!(calc_p1_answer(&ExpenseReport::new(vec![])).is_err());
        assert!(
            calc_p1_answer(&ExpenseReport::new(vec![1780, 1693, 1830, 1756, 1858, 100])).is_err()
        );
        assert_eq!(
            calc_p1_answer(&ExpenseReport::new(vec![1010])),
            Ok(1010 * 1010)
        );
        assert_eq!(
            calc_p1_answer(&ExpenseReport::new(vec![1721, 979, 366, 299, 675, 1456])),
            Ok(1721 * 299)
        );
    }

    #[test]
    fn test_p2_answer() {
        assert!(calc_p2_answer(&ExpenseReport::new(vec![])).is_err());
        assert!(calc_p2_answer(&ExpenseReport::new(vec![1, 2, 1923])).is_err());
        assert_eq!(
            calc_p2_answer(&ExpenseReport::new(vec![1721, 979, 366, 299, 675, 1456])),
            Ok(979 * 366 * 675)
        );
    }
}
