use std::collections::HashMap;
use std::collections::HashSet;
use itertools::Itertools;
use crate::utils::{self, StdResult, StdError};


#[derive(Clone, Hash, PartialEq, Eq, Debug)]
struct Point {
    id: usize,
    x: usize,
    y: usize,
}
impl std::str::FromStr for Point {
    type Err = StdError;
    fn from_str(s: &str) -> StdResult<Self> {
        let mut parts = s
            .split(",")
            .map(|n| n.trim());
        Ok(Self {
            id: 0,
            x: parts.next().unwrap().parse::<usize>()?,
            y: parts.next().unwrap().parse::<usize>()?,
        })
    }
}
impl Point {
    fn new(x: usize, y: usize) -> Self {
        Self { id: 0, x, y }
    }

    fn dist(&self, other: &Self) -> usize {
        let x = if self.x > other.x { self.x - other.x } else { other.x - self.x };
        let y = if self.y > other.y { self.y - other.y } else { other.y - self.y };
        x + y
    }
}


fn parse_points(input: &str) -> StdResult<Vec<Point>> {
    Ok(input
        .trim()
        .lines()
        .enumerate()
        .map(|(i, s)| {
            let mut p = s.parse::<Point>()?;
            p.id = i + 1;
            Ok(p)
        })
        .collect::<StdResult<_>>()?)
}


fn part_1(points: &[Point], field_size: &(usize, usize)) -> StdResult<u32> {
    let &(max_x, max_y) = field_size;
    let mut field = map!();
    for x in 0..max_x {
        for y in 0..max_y {
            let space = Point::new(x, y);
            let e = field.entry(space.clone()).or_insert((0usize, usize::max_value()));
            let (mut id,  mut dist) = e;
            for p in points {
                let point_dist = space.dist(p);
                if point_dist == dist {
                    id = 0;
                } else if point_dist < dist {
                    id = p.id;
                    dist = point_dist;
                }
            }
            *e = (id, dist);
        }
    }

    let mut edge_ids = set!();
    for x in 0..max_x {
        let (id, _) = field.get(&Point::new(x, 0)).unwrap();
        edge_ids.insert(id);
        let (id, _) = field.get(&Point::new(x, max_y-1)).unwrap();
        edge_ids.insert(id);
    }
    for y in 0..max_y {
        let (id, _) = field.get(&Point::new(0, y)).unwrap();
        edge_ids.insert(id);
        let (id, _) = field.get(&Point::new(max_x-1, y)).unwrap();
        edge_ids.insert(id);
    }

    let counts = utils::freqs(field.values().map(|(id, _)| *id));
    let (_k, v) = counts
        .iter()
        .filter(|(k, _v)| !edge_ids.contains(k))
        .max_by_key(|(_k, v)| *v).expect("no max");
    Ok(*v)
}


fn part_2(points: &[Point], field_size: &(usize, usize), limit: usize) -> StdResult<u32> {
    let &(max_x, max_y) = field_size;
    Ok((0..max_x).cartesian_product(0..max_y)
        .fold(0, |acc, (x, y)| {
            let space = Point::new(x, y);
            if points.iter().map(|p| space.dist(p)).sum::<usize>() < limit {
                acc + 1
            } else {
                acc
            }
        }))
}


pub fn run() -> StdResult<()> {
    info!("*** Day 6 ***");
    let input = input_file!("d06.txt");
    let points = parse_points(input)?;
    let max_x = points.iter().max_by_key(|p| p.x).unwrap().x + 5;
    let max_y = points.iter().max_by_key(|p| p.y).unwrap().y + 5;
    let field_size = (max_x, max_y);

    let (ms1, res1) = time!({ part_1(&points, &field_size)? });
    info!("p1: {}", res1);

    let (ms2, res2) = time!({ part_2(&points, &field_size, 10000)? });
    info!("p2: {}", res2);

    info!("[Day 6 runtimes] p1: {}ms, p2: {}ms\n", ms1, ms2);
    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;

    static INPUT: &'static str = r##"
1, 1
1, 6
8, 3
3, 4
5, 5
8, 9
"##;

    #[test]
    fn test_part_1() {
        let points = parse_points(INPUT).unwrap();
        let max_x = points.iter().max_by_key(|p| p.x).unwrap().x + 5;
        let max_y = points.iter().max_by_key(|p| p.y).unwrap().y + 5;
        let field_size = (max_x, max_y);
        let res = part_1(&points, &field_size).unwrap();
        assert_eq!(res, 17);
    }

    #[test]
    fn test_part_2() {
        let points = parse_points(INPUT).unwrap();
        let res = part_2(&points, &field_size, 32).unwrap();
        assert_eq!(res, 16);
    }
}
