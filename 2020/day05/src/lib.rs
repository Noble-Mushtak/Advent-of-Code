use snafu::Snafu;
use std::convert::{TryFrom, TryInto};
use std::error::Error;
use std::fs;
use std::str::FromStr;

#[derive(PartialEq, Debug)]
enum FrontBack {
    Front,
    Back,
}

impl TryFrom<char> for FrontBack {
    type Error = ();

    fn try_from(val: char) -> Result<Self, Self::Error> {
        match val {
            'F' => Ok(FrontBack::Front),
            'B' => Ok(FrontBack::Back),
            _ => Err(()),
        }
    }
}

#[derive(PartialEq, Debug)]
enum LeftRight {
    Left,
    Right,
}

impl TryFrom<char> for LeftRight {
    type Error = ();

    fn try_from(val: char) -> Result<Self, Self::Error> {
        match val {
            'L' => Ok(LeftRight::Left),
            'R' => Ok(LeftRight::Right),
            _ => Err(()),
        }
    }
}

#[derive(PartialEq, Debug)]
struct SeatIdentifier {
    row_id: [FrontBack; 7],
    column_id: [LeftRight; 3],
}

#[derive(PartialEq, Debug, Snafu)]
enum ParseSeatError {
    #[snafu(display("\"{}\" is not a valid seat identifier", identifier))]
    BadIdentifier { identifier: String },
}

impl FromStr for SeatIdentifier {
    type Err = ParseSeatError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut input_chars = input.chars();

        let fb1: FrontBack = input_chars
            .next()
            .ok_or(ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?
            .try_into()
            .map_err(|_| ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?;
        let fb2: FrontBack = input_chars
            .next()
            .ok_or(ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?
            .try_into()
            .map_err(|_| ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?;
        let fb3: FrontBack = input_chars
            .next()
            .ok_or(ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?
            .try_into()
            .map_err(|_| ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?;
        let fb4: FrontBack = input_chars
            .next()
            .ok_or(ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?
            .try_into()
            .map_err(|_| ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?;
        let fb5: FrontBack = input_chars
            .next()
            .ok_or(ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?
            .try_into()
            .map_err(|_| ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?;
        let fb6: FrontBack = input_chars
            .next()
            .ok_or(ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?
            .try_into()
            .map_err(|_| ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?;
        let fb7: FrontBack = input_chars
            .next()
            .ok_or(ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?
            .try_into()
            .map_err(|_| ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?;
        let lr1: LeftRight = input_chars
            .next()
            .ok_or(ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?
            .try_into()
            .map_err(|_| ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?;
        let lr2: LeftRight = input_chars
            .next()
            .ok_or(ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?
            .try_into()
            .map_err(|_| ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?;
        let lr3: LeftRight = input_chars
            .next()
            .ok_or(ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?
            .try_into()
            .map_err(|_| ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })?;
        if input_chars.next().is_some() {
            Err(ParseSeatError::BadIdentifier {
                identifier: input.to_string(),
            })
        } else {
            Ok(SeatIdentifier {
                row_id: [fb1, fb2, fb3, fb4, fb5, fb6, fb7],
                column_id: [lr1, lr2, lr3],
            })
        }
    }
}

#[derive(PartialEq, Debug)]
struct SeatIdentifiers(Vec<SeatIdentifier>);

impl FromStr for SeatIdentifiers {
    type Err = ParseSeatError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(SeatIdentifiers(
            input
                .trim()
                .split('\n')
                .map(str::parse)
                .collect::<Result<_, _>>()?,
        ))
    }
}

fn seat_id_num(identifier: &SeatIdentifier) -> usize {
    8 * identifier
        .row_id
        .iter()
        .fold(0, |res, fb| 2 * res + ((fb == &FrontBack::Back) as usize))
        + identifier
            .column_id
            .iter()
            .fold(0, |res, fb| 2 * res + ((fb == &LeftRight::Right) as usize))
}

const MAX_SEAT_ID_NUM: usize = 1023;

fn find_correct_num(nums: &mut Vec<usize>) -> Option<usize> {
    nums.sort_unstable();
    let res = nums
        .iter()
        .try_fold(MAX_SEAT_ID_NUM + 1, |last_id, cur_id| {
            if &(last_id + 2) == cur_id {
                Err(last_id + 1)
            } else {
                Ok(*cur_id)
            }
        });
    match res {
        Err(num) => Some(num),
        Ok(_) => None,
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let ids: SeatIdentifiers = fs::read_to_string("in.txt")?.parse()?;
    let mut seat_id_nums = ids.0.iter().map(seat_id_num).collect::<Vec<_>>();
    println!("Part 1: {}", seat_id_nums.iter().fold(0, |a, b| a.max(*b)));
    println!(
        "Part 2: {}",
        find_correct_num(&mut seat_id_nums).ok_or("Could not find answer to part 2")?
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::FrontBack::*;
    use super::LeftRight::*;
    use super::*;

    #[test]
    fn test_parse_seat_id() {
        assert!("FBFFBFFRL".parse::<SeatIdentifier>().is_err());
        assert!("FBFFBFFRLLL".parse::<SeatIdentifier>().is_err());
        assert!("FBFFXFFRLL".parse::<SeatIdentifier>().is_err());
        assert!("FBFFBFFRLX".parse::<SeatIdentifier>().is_err());
        assert_eq!(
            "FBFFBFFRLL".parse(),
            Ok(SeatIdentifier {
                row_id: [Front, Back, Front, Front, Back, Front, Front],
                column_id: [Right, Left, Left],
            })
        );
    }

    #[test]
    fn test_parse_seat_ids() {
        assert_eq!(
            "FBFFBFFRLL
BFFFFBBRLR
FFFBBBBRLR"
                .parse::<SeatIdentifiers>(),
            Ok(SeatIdentifiers(vec![
                SeatIdentifier {
                    row_id: [Front, Back, Front, Front, Back, Front, Front],
                    column_id: [Right, Left, Left]
                },
                SeatIdentifier {
                    row_id: [Back, Front, Front, Front, Front, Back, Back],
                    column_id: [Right, Left, Right]
                },
                SeatIdentifier {
                    row_id: [Front, Front, Front, Back, Back, Back, Back],
                    column_id: [Right, Left, Right]
                }
            ]))
        );
        assert!("FBFFBFFXLL
BFFFFBBRLR
FFFBBBBRLR"
            .parse::<SeatIdentifiers>()
            .is_err());
    }

    #[test]
    fn test_calc_seat_id() {
        assert_eq!(seat_id_num(&"FBFBBFFRLR".parse().unwrap()), 357);
        assert_eq!(seat_id_num(&"BFFFBBFRRR".parse().unwrap()), 567);
        assert_eq!(seat_id_num(&"FFFBBBFRRR".parse().unwrap()), 119);
        assert_eq!(seat_id_num(&"BBFFBBFRLL".parse().unwrap()), 820);
    }

    #[test]
    fn test_correct_num() {
        assert_eq!(find_correct_num(&mut vec![0, 1, 5]), None);
        assert_eq!(find_correct_num(&mut vec![3, 0, 6]), None);
        assert_eq!(find_correct_num(&mut vec![1, 3, 4, 5, 6]), Some(2));
        assert_eq!(find_correct_num(&mut vec![6, 3, 1, 5, 4]), Some(2));
    }
}
