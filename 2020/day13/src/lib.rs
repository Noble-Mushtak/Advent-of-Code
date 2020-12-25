use snafu::Snafu;
use std::error::Error;
use std::fs;
use std::str::FromStr;

#[derive(PartialEq, Debug, Clone, Copy)]
enum Bus {
    InService { id: u128 },
    OutOfService,
}

#[derive(PartialEq, Debug, Snafu)]
#[snafu(display("\"{}\" does not represent a valid bus.", line))]
struct ParseBusError {
    line: String,
}

impl FromStr for Bus {
    type Err = ParseBusError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match input.parse() {
            Ok(num) => Ok(Bus::InService { id: num }),
            Err(_) => match input {
                "x" => Ok(Bus::OutOfService),
                _ => Err(ParseBusError {
                    line: input.to_string(),
                }),
            },
        }
    }
}

#[derive(PartialEq, Debug)]
struct BusSchedule {
    departure_time: u128,
    buses: Vec<Bus>,
}

#[derive(PartialEq, Debug, Snafu)]
enum ParseScheduleError {
    #[snafu(display("A string representing a BusSchedule must have two lines"))]
    NotEnoughLines,
    #[snafu(display("\"{}\" is not an unsigned integer.", line))]
    NonInteger { line: String },
    #[snafu(display("{}", err))]
    BadBus { err: ParseBusError },
}

impl FromStr for BusSchedule {
    type Err = ParseScheduleError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        use ParseScheduleError::*;

        let mut lines = input.lines();
        let time_line = lines.next().ok_or(NotEnoughLines)?;
        let departure_time = time_line.parse().map_err(|_| NonInteger {
            line: time_line.to_string(),
        })?;
        let buses_line = lines.next().ok_or(NotEnoughLines)?;
        buses_line
            .split(',')
            .map(str::parse)
            .collect::<Result<_, _>>()
            .map(|buses| BusSchedule {
                departure_time,
                buses,
            })
            .map_err(|err| BadBus { err })
    }
}

impl BusSchedule {
    fn find_earliest_bus_id_and_departure_time(&self) -> Option<(u128, u128)> {
        self.buses
            .iter()
            .copied()
            .filter_map(|bus| match bus {
                Bus::InService { id } => match self.departure_time % id {
                    0 => Some((id, self.departure_time)),
                    rem => Some((id, self.departure_time + (id - rem))),
                },
                Bus::OutOfService => None,
            })
            .min_by_key(|&(_, time)| time)
    }
}

#[derive(PartialEq, Debug)]
struct ModuloClass {
    residue: u128,
    modulo: u128,
}

#[derive(PartialEq, Debug)]
struct EuclidResult {
    coeff1: i128,
    coeff2: i128,
    gcd: i128,
}

fn extended_euclid(mod1: &u128, mod2: &u128) -> EuclidResult {
    let mut mod1 = *mod1 as i128;
    let mut mod2 = *mod2 as i128;

    let mut r1 = 1;
    let mut s1 = 0;
    let mut r2 = 0;
    let mut s2 = 1;
    if mod1 < mod2 {
        std::mem::swap(&mut mod1, &mut mod2);
        std::mem::swap(&mut r1, &mut r2);
        std::mem::swap(&mut s1, &mut s2);
    }
    while mod2 > 0 {
        let coeff = mod1 / mod2;
        let shift_tuple = |x1: &mut i128, x2: &mut i128| {
            let temp = *x2;
            *x2 = (*x1) - coeff * (*x2);
            *x1 = temp;
        };
        shift_tuple(&mut mod1, &mut mod2);
        shift_tuple(&mut r1, &mut r2);
        shift_tuple(&mut s1, &mut s2);
    }
    EuclidResult {
        coeff1: r1,
        coeff2: s1,
        gcd: mod1,
    }
}

fn chinese_remainder_theorem(cls1: ModuloClass, cls2: ModuloClass) -> Option<ModuloClass> {
    let (x1, x2) = (cls1.residue as i128, cls2.residue as i128);
    let (m1, m2) = (cls1.modulo as i128, cls2.modulo as i128);
    let EuclidResult {
        coeff1,
        coeff2,
        gcd,
    } = extended_euclid(&cls1.modulo, &cls2.modulo);
    if (x1 % gcd, x2 % gcd) == (0, 0) {
        let lcm = (m1 * m2 / gcd) as u128;
        let ans = x2 / gcd * coeff1 * m1 + x1 / gcd * coeff2 * m2;
        Some(ModuloClass {
            residue: ans.rem_euclid(lcm as i128) as u128,
            modulo: lcm,
        })
    } else {
        None
    }
}

impl BusSchedule {
    fn find_earliest_timestamp(&self) -> Option<u128> {
        let mut bus_tpls = self
            .buses
            .iter()
            .enumerate()
            .filter_map(|(i, bus)| match bus {
                Bus::InService { id } => Some(ModuloClass {
                    residue: (-(i as i128)).rem_euclid(*id as i128) as u128,
                    modulo: *id,
                }),
                Bus::OutOfService => None,
            });
        let initial_class = bus_tpls.next()?;
        Some(
            bus_tpls
                .try_fold(initial_class, chinese_remainder_theorem)?
                .residue,
        )
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let sched: BusSchedule = fs::read_to_string("in.txt")?.parse()?;

    let (bus_id, depart_time) = sched
        .find_earliest_bus_id_and_departure_time()
        .expect("No answer for part 1 found");
    println!("Part 1: {}", bus_id * (depart_time - sched.departure_time));

    println!(
        "Part 2: {}",
        sched
            .find_earliest_timestamp()
            .expect("No answer for part 2 found")
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn example_data() -> BusSchedule {
        use Bus::*;
        BusSchedule {
            departure_time: 939,
            buses: vec![
                InService { id: 7 },
                InService { id: 13 },
                OutOfService,
                OutOfService,
                InService { id: 59 },
                OutOfService,
                InService { id: 31 },
                InService { id: 19 },
            ],
        }
    }

    #[test]
    fn test_parse() {
        assert_eq!("939\n7,13,x,x,59,x,31,19".parse(), Ok(example_data()));
    }

    #[test]
    fn test_part1() {
        assert_eq!(
            example_data().find_earliest_bus_id_and_departure_time(),
            Some((59, 944))
        );
    }

    #[test]
    fn test_part2() {
        assert_eq!(example_data().find_earliest_timestamp(), Some(1068781));
    }
}
