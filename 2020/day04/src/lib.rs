#![feature(str_split_once)]

use snafu::Snafu;
use std::error::Error;
use std::fs;
use std::str::FromStr;

#[derive(PartialEq, Debug)]
struct Passport {
    birth_year: Option<String>,
    issue_year: Option<String>,
    expiration_year: Option<String>,
    height: Option<String>,
    hair_color: Option<String>,
    eye_color: Option<String>,
    passport_id: Option<String>,
    country_id: Option<String>,
}

const EMPTY_PASSPORT: Passport = Passport {
    birth_year: None,
    issue_year: None,
    expiration_year: None,
    height: None,
    hair_color: None,
    eye_color: None,
    passport_id: None,
    country_id: None,
};

#[derive(PartialEq, Debug, Snafu)]
enum ParsePassportError {
    #[snafu(display("\"{}\" is not a valid key-value pair", bad_kv_pair))]
    KeyValuePair { bad_kv_pair: String },
    #[snafu(display("\"{}\" is not a valid key", bad_key))]
    BadKey { bad_key: String },
}

impl Passport {
    fn update(self, key: &str, value: &str) -> Result<Passport, ParsePassportError> {
        use ParsePassportError::*;

        let value = value.to_string();
        match key {
            "byr" => Ok(Passport {
                birth_year: Some(value),
                ..self
            }),
            "iyr" => Ok(Passport {
                issue_year: Some(value),
                ..self
            }),
            "eyr" => Ok(Passport {
                expiration_year: Some(value),
                ..self
            }),
            "hgt" => Ok(Passport {
                height: Some(value),
                ..self
            }),
            "hcl" => Ok(Passport {
                hair_color: Some(value),
                ..self
            }),
            "ecl" => Ok(Passport {
                eye_color: Some(value),
                ..self
            }),
            "pid" => Ok(Passport {
                passport_id: Some(value),
                ..self
            }),
            "cid" => Ok(Passport {
                country_id: Some(value),
                ..self
            }),
            _ => Err(BadKey {
                bad_key: key.to_string(),
            }),
        }
    }
}

impl FromStr for Passport {
    type Err = ParsePassportError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        use ParsePassportError::*;

        input
            .trim()
            .split_whitespace()
            .fold(Ok(EMPTY_PASSPORT), |res, kv_pair| {
                res.and_then(|passport| {
                    let (key, value) = kv_pair.split_once(':').ok_or(KeyValuePair {
                        bad_kv_pair: kv_pair.to_string(),
                    })?;
                    passport.update(key, value)
                })
            })
    }
}

#[derive(PartialEq, Debug)]
struct PassportBatch(Vec<Passport>);

impl FromStr for PassportBatch {
    type Err = ParsePassportError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        input
            .trim()
            .split("\n\n")
            .map(str::parse)
            .collect::<Result<_, _>>()
            .map(PassportBatch)
    }
}

#[derive(PartialEq, Debug)]
struct SemivalidPassport {
    birth_year: String,
    issue_year: String,
    expiration_year: String,
    height: String,
    hair_color: String,
    eye_color: String,
    passport_id: String,
    country_id: Option<String>,
}

fn to_semivalid_passport(passport: Passport) -> Option<SemivalidPassport> {
    match (
        passport.birth_year,
        passport.issue_year,
        passport.expiration_year,
        passport.height,
        passport.hair_color,
        passport.eye_color,
        passport.passport_id,
    ) {
        (Some(byr), Some(iyr), Some(eyr), Some(hgt), Some(hcl), Some(ecl), Some(pid)) => {
            Some(SemivalidPassport {
                birth_year: byr,
                issue_year: iyr,
                expiration_year: eyr,
                height: hgt,
                hair_color: hcl,
                eye_color: ecl,
                passport_id: pid,
                country_id: passport.country_id,
            })
        }
        _ => None,
    }
}

fn is_valid_passport(passport: SemivalidPassport) -> Option<()> {
    let birth_year: usize = passport.birth_year.parse().ok()?;
    if !(1920..=2002).contains(&birth_year) {
        return None;
    }
    let issue_year: usize = passport.issue_year.parse().ok()?;
    if !(2010..=2020).contains(&issue_year) {
        return None;
    }
    let expiration_year: usize = passport.expiration_year.parse().ok()?;
    if !(2020..=2030).contains(&expiration_year) {
        return None;
    }

    if passport.height.ends_with("cm") {
        let height: usize = passport
            .height
            .get(..passport.height.len() - 2)?
            .parse()
            .ok()?;
        if !(150..=193).contains(&height) {
            return None;
        }
    } else if passport.height.ends_with("in") {
        let height: usize = passport
            .height
            .get(..passport.height.len() - 2)?
            .parse()
            .ok()?;
        if !(59..=76).contains(&height) {
            return None;
        }
    } else {
        return None;
    }

    if passport.hair_color.starts_with('#') {
        let color_hex = passport.hair_color.get(1..)?;
        if color_hex.len() != 6 || color_hex.chars().any(|char| !('0'..='f').contains(&char)) {
            return None;
        }
    } else {
        return None;
    }

    let eye_colors = vec!["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];
    if !eye_colors.contains(&&passport.eye_color[..]) {
        return None;
    }

    if passport.passport_id.len() != 9
        || passport
            .passport_id
            .chars()
            .any(|char| !('0'..='9').contains(&char))
    {
        return None;
    }

    Some(())
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let batch: PassportBatch = fs::read_to_string("in.txt")?.parse()?;
    let semivalid_passports: Vec<SemivalidPassport> = batch
        .0
        .into_iter()
        .filter_map(to_semivalid_passport)
        .collect();
    println!("Part 1: {}", semivalid_passports.len());
    let valid_passports = semivalid_passports
        .into_iter()
        .filter_map(is_valid_passport);
    println!("Part 2: {}", valid_passports.count());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn example_passport() -> Passport {
        Passport {
            eye_color: Some("gry".to_string()),
            birth_year: Some("1937".to_string()),
            passport_id: Some("860033327".to_string()),
            country_id: Some("147".to_string()),
            hair_color: Some("#fffffd".to_string()),
            expiration_year: Some("2020".to_string()),
            height: Some("183cm".to_string()),
            issue_year: Some("2017".to_string()),
        }
    }

    #[test]
    fn test_update() {
        assert_eq!(
            EMPTY_PASSPORT.update("ecl", "gry"),
            Ok(Passport {
                eye_color: Some("gry".to_string()),
                ..EMPTY_PASSPORT
            })
        );
        assert_eq!(
            EMPTY_PASSPORT
                .update("ecl", "gry")
                .unwrap()
                .update("pid", "860033327")
                .unwrap()
                .update("eyr", "2020")
                .unwrap()
                .update("hcl", "#fffffd")
                .unwrap()
                .update("byr", "1937")
                .unwrap()
                .update("iyr", "2017")
                .unwrap()
                .update("cid", "147")
                .unwrap()
                .update("hgt", "183cm"),
            Ok(example_passport())
        );
        assert!(EMPTY_PASSPORT.update("sdf", "gry").is_err());
    }

    const PASSPORT_STRING1: &str = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm";
    const PASSPORT_STRING2: &str = "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929";

    #[test]
    fn test_parse_passport() {
        assert_eq!(PASSPORT_STRING1.parse::<Passport>(), Ok(example_passport()));
        assert!(PASSPORT_STRING2.parse::<Passport>().is_ok());
        assert_eq!(
            "ecl:gry".parse::<Passport>(),
            Ok(Passport {
                eye_color: Some("gry".to_string()),
                ..EMPTY_PASSPORT
            })
        );
        assert!("ecl:gry fgj".parse::<Passport>().is_err());
        assert!("ec:gry".parse::<Passport>().is_err());
    }

    fn example_passport2() -> Passport {
        PASSPORT_STRING2.parse().unwrap()
    }

    #[test]
    fn test_parse_batch() {
        assert_eq!(
            "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929"
                .parse(),
            Ok(PassportBatch(vec![example_passport(), example_passport2()]))
        );
        assert!("ecls:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929"
            .parse::<PassportBatch>()
            .is_err());
    }

    #[test]
    fn test_semivalid() {
        assert!(to_semivalid_passport(example_passport()).is_some());
        assert!(to_semivalid_passport(example_passport2()).is_none());
    }

    fn represents_valid_passport(input: &str) -> bool {
        match input.parse() {
            Err(_) => false,
            Ok(passport) => match to_semivalid_passport(passport) {
                None => false,
                Some(s_passport) => is_valid_passport(s_passport).is_some(),
            },
        }
    }

    #[test]
    fn test_valid() {
        assert!(represents_valid_passport(
            "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f"
        ));
        assert!(represents_valid_passport(
            "eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
        ));
        assert!(represents_valid_passport(
            "hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022"
        ));
        assert!(represents_valid_passport(
            "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
        ));
        assert!(!represents_valid_passport(
            "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
        ));
        assert!(!represents_valid_passport(
            "iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946"
        ));
        assert!(!represents_valid_passport(
            "hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
        ));
        assert!(!represents_valid_passport(
            "hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007"
        ));
    }
}
