/*!
http://adventofcode.com/2017/day/11
*/

static INPUT: &'static str = include_str!("../input.txt");


enum Move {
    N,
    Nw,
    Ne,
    S,
    Sw,
    Se,
}
impl std::str::FromStr for Move {
    type Err = Box<std::error::Error>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Move::*;
        Ok(match s {
            "n" => N,
            "nw" => Nw,
            "ne" => Ne,
            "s" => S,
            "sw" => Sw,
            "se" => Se,
            _ => Err(format!("invalid move: {:?}", s))?,
        })
    }
}
impl Move {
    fn parse_moves(input: &str) -> Result<Vec<Self>, Box<std::error::Error>> {
        input.trim()
            .split(",")
            .map(str::trim)
            .map(|s| s.parse::<Self>())
            .collect()
    }
}


/// Axial coordinates on a flat-topped hexagonal grid
/// Thanks: https://www.redblobgames.com/grids/hexagons/
///
/// Columns(x) are vertical and increasing to the right of the origin
/// Diagonals(z) are right-diagonal (NW to SE) rows decreasing in the right-up (NE) direction
/// (y) (calculated) Opposite diagonals are left-diagonal (NE to SW) rows decreasing in the right-down (SE) direction
#[derive(Debug)]
struct AxialPos {
    x: isize,   // column
    z: isize,   // (right) diag row
}
impl AxialPos {
    fn origin() -> Self {
        Self { x: 0, z: 0 }
    }

    fn step(&mut self, m: &Move) {
        use Move::*;
        match *m {
            N   => self.z -= 1,
            Ne  => {
                self.x += 1;
                self.z -= 1;
            }
            Nw  => self.x -= 1,
            S   => self.z += 1,
            Se  => self.x += 1,
            Sw  => {
                self.x -= 1;
                self.z += 1;
            }
        }
    }

    /// Returning x, y, z
    fn as_cubic(&self) -> (isize, isize, isize) {
        let y = (-self.x) - self.z;
        (self.x, y, self.z)
    }

    /// Manhattan distance
    fn step_dist_from(&self, other: &Self) -> usize {
        let (x1, y1, z1) = self.as_cubic();
        let (x2, y2, z2) = other.as_cubic();
        (x1 - x2).abs()
            .max((y1 - y2).abs())
            .max((z1 - z2).abs()) as usize
    }
}


fn part1(input: &str) -> usize {
    let moves = Move::parse_moves(input).expect("invalid move");
    let mut point = AxialPos::origin();
    for m in &moves {
        point.step(m);
    }
    let origin = AxialPos::origin();
    point.step_dist_from(&origin)
}


fn part2(input: &str) -> usize {
    let moves = Move::parse_moves(input).expect("invalid move");
    let origin = AxialPos::origin();
    let mut point = AxialPos::origin();
    let mut max_dist = 0;
    for m in &moves {
        point.step(m);
        let dist = point.step_dist_from(&origin);
        max_dist = max_dist.max(dist);
    }
    max_dist
}


pub fn main() {
    println!("d11-p1: {}", part1(INPUT));
    println!("d11-p2: {}", part2(INPUT));
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn step_dist() {
        [
            ("ne,ne,ne", 3),
            ("ne,ne,sw,sw", 0),
            ("ne,ne,s,s", 2),
            ("se,sw,se,sw,sw", 3),
        ].iter().for_each(|&(input, expected)| {
            assert_eq!(part1(input), expected);
        })
    }
}

