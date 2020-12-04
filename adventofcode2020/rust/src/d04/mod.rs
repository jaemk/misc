use crate::utils::err;
use crate::utils::file;
use itertools::Itertools;
use regex::Regex;
use std::collections::HashMap;

static REQUIRED_FIELDS: &[&str] = &[
    "byr", // (Birth Year) - four digits; at least 1920 and at most 2002
    "iyr", // (Issue Year) - four digits; at least 2010 and at most 2020
    "eyr", // (Expiration Year) - four digits; at least 2020 and at most 2030.
    "hgt", // (Height) - a number followed by either cm or in:
    // If cm, the number must be at least 150 and at most 193.
    // If in, the number must be at least 59 and at most 76
    "hcl", // (Hair Color) - a # followed by exactly six characters 0-9 or a-f
    "ecl", // (Eye Color) - exactly one of: amb blu brn gry grn hzl oth
    "pid", // (Passport ID) - a nine-digit number, including leading zeroes
];

lazy_static::lazy_static! {
    static ref HCL_RE: Regex = Regex::new("^#[a-f0-9]{6}$").unwrap();
    static ref ECL_RE: Regex = Regex::new("^(amb|blu|brn|gry|grn|hzl|oth)$").unwrap();
    static ref PID_RE: Regex = Regex::new("^[0-9]{9}$").unwrap();
}

struct PP<'a> {
    inner: HashMap<&'a str, &'a str>,
}
impl<'a> PP<'a> {
    #[inline]
    fn has_required_fields(&self) -> bool {
        REQUIRED_FIELDS.iter().all(|f| self.inner.contains_key(f))
    }

    #[inline]
    fn get_unchecked(&'a self, k: &str) -> &'a str {
        self.inner.get(k).unwrap()
    }

    #[inline]
    fn in_range(&self, k: &str, min: u32, max: u32) -> bool {
        match self.get_unchecked(k).parse::<u32>() {
            Err(_) => return false,
            Ok(byr) => {
                if byr < min || byr > max {
                    return false;
                }
            }
        }
        true
    }

    #[inline]
    fn is_valid(&self) -> bool {
        if !self.has_required_fields() {
            return false;
        }

        // (Birth Year) - four digits; at least 1920 and at most 2002
        if !self.in_range("byr", 1920, 2002) {
            return false;
        }

        // (Issue Year) - four digits; at least 2010 and at most 2020
        if !self.in_range("iyr", 2010, 2020) {
            return false;
        }

        // (Expiration Year) - four digits; at least 2020 and at most 2030.
        if !self.in_range("eyr", 2020, 2030) {
            return false;
        }

        // (Height) - a number followed by either cm or in:
        // If cm, the number must be at least 150 and at most 193.
        // If in, the number must be at least 59 and at most 76
        let hgt = self.get_unchecked("hgt");
        if hgt.ends_with("cm") {
            match hgt.trim_end_matches("cm").parse::<u32>() {
                Err(_) => return false,
                Ok(hgt) => {
                    if hgt < 150 || hgt > 193 {
                        return false;
                    }
                }
            }
        } else if hgt.ends_with("in") {
            match hgt.trim_end_matches("in").parse::<u32>() {
                Err(_) => return false,
                Ok(hgt) => {
                    if hgt < 59 || hgt > 76 {
                        return false;
                    }
                }
            }
        } else {
            return false;
        }

        // (Hair Color) - a # followed by exactly six characters 0-9 or a-f
        let hcl = self.get_unchecked("hcl");
        if !HCL_RE.is_match(hcl) {
            return false;
        }

        // (Eye Color) - exactly one of: amb blu brn gry grn hzl oth
        let ecl = self.get_unchecked("ecl");
        if !ECL_RE.is_match(ecl) {
            return false;
        }

        // (Passport ID) - a nine-digit number, including leading zeroes
        let pid = self.get_unchecked("pid");
        if !PID_RE.is_match(pid) {
            return false;
        }

        true
    }
}

fn parse(input: &str) -> err::Result<Vec<PP>> {
    Ok(input
        .trim()
        .split("\n\n")
        .map(|group| {
            let map = group
                .split_whitespace()
                .fold(map!(size = 8), |mut acc, piece| {
                    let (key, value) = piece
                        .split(':')
                        .collect_tuple()
                        .expect("invalid piece format");
                    acc.insert(key, value);
                    acc
                });
            PP { inner: map }
        })
        .collect())
}

fn part1(input: &[PP]) -> err::Result<u32> {
    Ok(input
        .iter()
        .map(|pp| if pp.has_required_fields() { 1 } else { 0 })
        .sum())
}

fn part2(input: &[PP]) -> err::Result<u32> {
    Ok(input
        .iter()
        .map(|pp| if pp.is_valid() { 1 } else { 0 })
        .sum())
}

pub fn run() -> err::Result<()> {
    let input = time!(
        file::read("../input/d04.txt")?,
        (ms) -> println!("  -> read[{}ms]", ms),
    );
    let input = time!(
        parse(&input)?,
        (ms) -> println!("  -> parse[{}ms]", ms),
    );

    let (ms, res) = time!(part1(&input)?);
    println!("  -> p1[{}ms]: {}", ms, res);
    let (ms, res) = time!(part2(&input)?);
    println!("  -> p2[{}ms]: {}", ms, res);

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    static INPUT: &str = r##"
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
"##;

    #[test]
    fn test_p1() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(part1(&input).expect("p1 fail"), 2);
    }

    static INVALID_INPUT: &str = r##"
eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007
"##;
    #[test]
    fn test_p2_invalid() {
        let input = parse(INVALID_INPUT).expect("parse fail");
        assert_eq!(part2(&input).expect("p2 fail"), 0);
    }

    static VALID_INPUT: &str = r##"
pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
"##;

    #[test]
    fn test_p2_valid() {
        let input = parse(VALID_INPUT).expect("parse fail");
        assert_eq!(part2(&input).expect("p2 fail"), 4);
    }
}
