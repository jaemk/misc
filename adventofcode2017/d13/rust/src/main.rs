/*!
http://adventofcode.com/2017/day/13
*/
static INPUT: &'static str = include_str!("../../input.txt");


type AnyErr = Box<std::error::Error>;
type Result<T> = std::result::Result<T, AnyErr>;


struct FireWall {
    scanners: Vec<ScanLayer>,
}
impl std::str::FromStr for FireWall {
    type Err = AnyErr;
    fn from_str(s: &str) -> Result<FireWall> {
        let mut scanners = vec![];
        for line in s.trim().lines() {
            let scanner = line.trim().parse::<ScanLayer>()?;
            scanners.push(scanner);
        }
        Ok(FireWall { scanners })
    }
}


struct ScanLayer {
    depth: usize,     // picosecond depth
    range: usize,     // size of scan layer
    start_pos: usize, // starting position of the scanner
}
impl std::str::FromStr for ScanLayer {
    type Err = AnyErr;
    fn from_str(s: &str) -> Result<ScanLayer> {
        let mut parts = s.split(":");
        let depth = parts
            .next()
            .ok_or_else(|| "missing depth")?
            .parse::<usize>()?;

        let range = parts
            .next()
            .ok_or_else(|| "missing range")?
            .trim()
            .parse::<usize>()?;
        Ok(ScanLayer {
            depth,
            range,
            start_pos: 0,
        })
    }
}
impl ScanLayer {
    /// Simulate the scanner bouncing up and down and return its
    /// position at a given picosecond
    fn pos_at_time(&self, pico_seconds: usize) -> usize {
        let layer_steps = self.range - 1;
        let mut face_down = true;
        let mut offset = pico_seconds + self.start_pos;
        while offset >= layer_steps {
            offset -= layer_steps;
            face_down = !face_down;
        }
        if face_down {
            offset
        } else {
            layer_steps - offset
        }
    }

    /// Calculate if the scanner will be at the zero position
    /// at the given picosecond
    fn is_at_zero_at_time(&self, pico_seconds: usize) -> bool {
        let steps_in_dir = self.range - 1;
        let step_multiple_at_zero = steps_in_dir * 2;
        (pico_seconds + self.start_pos) % step_multiple_at_zero == 0
    }

    fn severity(&self) -> usize {
        self.depth * self.range
    }
}


fn part1(input: &str) -> Result<usize> {
    let wall = input.parse::<FireWall>()?;
    let mut severity = 0;
    for scan in wall.scanners.iter() {
        if scan.pos_at_time(scan.depth) == 0 {
            severity += scan.severity();
        }
    }
    Ok(severity)
}


fn part2(input: &str) -> Result<usize> {
    let wall = input.parse::<FireWall>()?;
    let mut delay = 0;
    loop {
        if wall.scanners
            .iter()
            .all(|scanner| !scanner.is_at_zero_at_time(delay + scanner.depth))
        {
            break;
        }
        delay += 1;
    }
    Ok(delay)
}


fn run() -> Result<()> {
    println!("d13-p1: {}", part1(INPUT)?);
    println!("d13-p2: {}", part2(INPUT)?);
    Ok(())
}


pub fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    static TEST_INPUT: &'static str = r##"
0: 3
1: 2
4: 4
6: 4
"##;

    #[test]
    fn p1() {
        assert_eq!(part1(TEST_INPUT).unwrap(), 24);
    }

    #[test]
    fn p2() {
        assert_eq!(part2(TEST_INPUT).unwrap(), 10);
    }
}
