use utils::{StdResult, StdError};
use std::str::FromStr;
use std::collections::{HashMap, HashSet};


struct Point {
    pub x: u32,
    pub y: u32,
}

struct Claim {
    pub id: u32,
    pub start: Point,
    pub end: Point,
}
impl FromStr for Claim {
    type Err = StdError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let msg = "invalid format";
        let mut line = s.split_whitespace();
        let id = line.next().expect(msg).trim_start_matches("#").parse::<u32>().expect(msg);
        line.next();
        let (x, y) = line.next().map(|xy| {
            let mut xy = xy.trim_end_matches(":").split(",");
            let x = xy.next().unwrap().parse::<u32>().unwrap();
            let y = xy.next().unwrap().parse::<u32>().unwrap();
            (x, y)
        }).expect(msg);
        let (w, h) = line.next().map(|wh| {
            let mut wh = wh.split("x");
            let w = wh.next().unwrap().parse::<u32>().unwrap();
            let h = wh.next().unwrap().parse::<u32>().unwrap();
            (w, h)
        }).expect(msg);
        Ok(Claim { id, start: Point { x, y }, end: Point { x: x + w, y: y + h }})
    }
}
impl Claim {
    fn coords(&self) -> Vec<(u32, u32)> {
        let mut coords = Vec::new();
        for x in self.start.x..self.end.x {
            for y in self.start.y..self.end.y {
                coords.push((x, y));
            }
        }
        coords
    }
}


fn parse(input: &str) -> StdResult<Vec<Claim>> {
    let claims = input
        .lines()
        .map(|line| line.parse::<Claim>())
        .collect::<StdResult<Vec<_>>>()?;
    Ok(claims)
}


struct Ans {
    p1: u32,
    p2: u32,
}


fn solve(claims: &[Claim]) -> StdResult<Ans> {
    let mut claim_coords = HashMap::new();
    let mut field = HashMap::new();
    for claim in claims {
        let coords = claim.coords();
        claim_coords.insert(claim.id, coords.clone());
        for coord in coords {
            let e = field.entry(coord).or_insert(vec![]);
            e.push(claim.id);
        }
    }

    let mut count = 0;
    let mut single_ids = HashSet::new();
    let mut single_coords = HashSet::new();
    for (coord, coord_ids) in field.iter() {
        if coord_ids.len() > 1 { count += 1 }
        else if coord_ids.len() == 1 {
            single_ids.insert(coord_ids[0].clone());
            single_coords.insert(coord);
        }
    }

    let mut single_claim = None;
    for id in single_ids.iter() {
        let coords = claim_coords.get(id).unwrap();
        if coords.iter().all(|coord| single_coords.contains(&coord)) {
            single_claim = Some(id);
            break;
        }
    }

    Ok(Ans { p1: count, p2: *single_claim.expect("part 2 fail") })
}


pub fn run() -> StdResult<()> {
    info!("*** Day 3 ***");
    let input = input_file!("d03.txt");

    let (ms, res) = time!({
        let claims = parse(&input)?;
        solve(&claims)?
    });

    info!("p1: {:?}", res.p1);
    info!("p2: {:?}", res.p2);
    info!("[Day 3 runtimes] both: {}ms\n", ms);
    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;
    static INPUT: &'static str = "#1 @ 1,3: 4x4\n\
                                #2 @ 3,1: 4x4\n\
                                #3 @ 5,5: 2x2";

    #[test]
    fn all_parts() {
        let claims = parse(INPUT).expect("parse fail");
        let res = solve(&claims).expect("solve fail");
        assert_eq!(res.p1, 4);
        assert_eq!(res.p2, 3);
    }
}
