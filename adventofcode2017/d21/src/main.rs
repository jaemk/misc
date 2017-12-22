/*!
http://adventofcode.com/2017/day/21
*/
use std::collections::HashMap;

mod range_step;
use range_step::RangeStep;

type Error = Box<std::error::Error>;
type Result<T> = std::result::Result<T, Error>;


static START: &'static str = ".#./..#/###";
static RULES: &'static str = include_str!("../input.txt");


/// Direction to split a field and flip over
#[derive(Debug)]
enum Split {
    Horiz,
    Vert,
}


/// A fractal art pattern
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
struct Pattern {
    field: Vec<Vec<bool>>,
}
impl std::str::FromStr for Pattern {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let field = s.trim()
            .split("/")
            .map(|row| {
                row.chars()
                    .map(|c| if c == '#' { true } else { false } )
                    .collect()
            })
            .collect();
        Ok(Self { field })
    }
}
impl Pattern {
    fn from_field(field: Vec<Vec<bool>>) -> Self {
        Self { field }
    }

    fn size(&self) -> usize {
        self.field.len()
    }

    fn is_even(&self) -> bool {
        self.size() % 2 == 0
    }

    fn count(&self) -> usize {
        self.field.iter().fold(0, |sum, row| {
            sum + row.iter().fold(0, |sum, elem| sum + if *elem { 1 } else { 0 })
        })
    }

    /// Rotate field clockwise
    fn rotate(&mut self) {
        let size = self.size();
        let mut new_field = Vec::with_capacity(size);
        for col in 0..size {
            let mut new_row = Vec::with_capacity(size);
            for row in (0..size).rev() {
                new_row.push(self.field[row][col]);
            }
            new_field.push(new_row);
        }
        self.field = new_field;
    }

    /// Flip field about the horizontal or vertical center line
    fn flip(&mut self, split: &Split) {
        let size = self.size();
        let half = size / 2;
        let even = size % 2 == 0;

        let left_inds = 0..half;
        let right_inds = if even { (half..size).rev() } else { (half+1..size).rev() };
        match *split {
            Split::Horiz => {
                for (a, b) in left_inds.zip(right_inds) {
                    self.field.swap(a, b);
                }
            }
            Split::Vert => {
                for (a, b) in left_inds.zip(right_inds) {
                    for row in self.field.iter_mut() {
                        row.swap(a, b);
                    }
                }
            }
        }
    }

    /// Return appropriately sized chunks of a field starting from the top left,
    /// moving right across each row of chunks
    fn chunks(&self) -> Vec<Self> {
        let step_size = if self.is_even() { 2 } else { 3 };
        let size = self.size();
        let n_chunks = (size / step_size).pow(2);
        let mut chunks = Vec::with_capacity(n_chunks);
        for row_start_ind in RangeStep(0, size, step_size) {
            for col_start_ind in RangeStep(0, size, step_size) {
                let mut chunk = vec![];
                for row_ind in row_start_ind..row_start_ind+step_size {
                    let row = self.field[row_ind][col_start_ind..col_start_ind+step_size].to_vec();
                    chunk.push(row);
                }
                chunks.push(Self::from_field(chunk));
            }
        }
        chunks
    }
}


#[derive(Debug)]
struct Rules {
    size_2: HashMap<Pattern, Pattern>,
    size_3: HashMap<Pattern, Pattern>,
}
impl std::str::FromStr for Rules {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let mut size_2 = HashMap::new();
        let mut size_3 = HashMap::new();
        for line in s.trim().lines() {
            let mut pats = line.trim().split("=>");
            let from = pats.next().expect("Missing `from` pattern").parse::<Pattern>()?;
            let to = pats.next().expect("Missing `to` pattern").parse::<Pattern>()?;
            match from.size() {
                2 => size_2.insert(from, to),
                3 => size_3.insert(from, to),
                n => Err(format!("Invalid pattern size: {}", n))?,
            };
        }
        Ok(Self { size_2, size_3 })
    }
}
impl Rules {
    fn enchance(&self, pattern: &mut Pattern) -> Result<()> {
        let (rules, step_size) = if pattern.is_even() { (&self.size_2, 2) } else { (&self.size_3, 3) };
        let chunks_per_row = pattern.size() / step_size;

        let mut chunks = pattern.chunks();
        'next_chunk: for chunk in chunks.iter_mut() {
            if let Some(new) = rules.get(&chunk) {
                chunk.field = new.field.clone();
                continue 'next_chunk
            }

            for _ in 0..4 {
                for split in &[Split::Horiz, Split::Vert] {
                    chunk.flip(split);
                    if let Some(new) = rules.get(&chunk) {
                        chunk.field = new.field.clone();
                        continue 'next_chunk
                    }
                    chunk.flip(split);
                }
                chunk.rotate();
            }
            unreachable!();
        }

        // Now we have a list of enhanced chunks [top-left, top-right, bottom-left, bottom-right]
        // and we need to join them back into one large 2d field
        let chunk_size = chunks[0].size();
        let new_size = chunk_size * chunks_per_row;
        let mut joined = Vec::with_capacity(new_size);
        for chunk_start in RangeStep(0, chunks.len(), chunks_per_row) {
            for chunk_row in 0..chunk_size {
                let mut row = Vec::with_capacity(new_size);
                for chunk in &chunks[chunk_start..chunk_start+chunks_per_row] {
                    row.extend_from_slice(&chunk.field[chunk_row]);
                }
                joined.push(row);
            }
        }
        pattern.field = joined;
        Ok(())
    }
}


#[allow(non_snake_case)]
fn ENHANCEEEE(start: &str, rules: &str, iterations: usize) -> Result<usize> {
    let rules = rules.parse::<Rules>()?;
    let mut pattern = start.parse::<Pattern>()?;
    for _ in 0..iterations {
        rules.enchance(&mut pattern)?;
    }
    Ok(pattern.count())
}


fn time<F: Fn() -> Result<T>, T>(f: F) -> Result<(f64, T)> {
    use std::time;
    let start = time::Instant::now();
    let ans = f()?;
    let elap = start.elapsed().subsec_nanos() as f64 / 1_000_000.;
    Ok((elap, ans))
}


fn run() -> Result<()> {
    let (elap, p1) = time(|| {
        let ans = ENHANCEEEE(START, RULES, 5)?;
        Ok(ans)
    })?;
    println!("d21-p1: [{:>11}ms] {}", elap, p1);

    let (elap, p2) = time(|| {
        let ans = ENHANCEEEE(START, RULES, 18)?;
        Ok(ans)
    })?;
    println!("d21-p2: [{:>11}ms] {}", elap, p2);
    Ok(())
}


pub fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    static TEST_RULES: &'static str = r##"
../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#
"##;

    #[test]
    fn it_rotates() {
        let mut pat = START.parse::<Pattern>().unwrap();

        pat.rotate();
        let rotated = "#../#.#/##.".parse::<Pattern>().unwrap();
        assert_eq!(pat, rotated);

        pat.rotate();
        let rotated = "###/#../.#.".parse::<Pattern>().unwrap();
        assert_eq!(pat, rotated);

        pat.rotate();
        let rotated = ".##/#.#/..#".parse::<Pattern>().unwrap();
        assert_eq!(pat, rotated);

        pat.rotate();
        let rotated = START.parse::<Pattern>().unwrap();
        assert_eq!(pat, rotated);
    }

    #[test]
    fn it_flips_horiz_split() {
        let mut pat = START.parse::<Pattern>().unwrap();

        pat.flip(&Split::Horiz);
        let flipped = "###/..#/.#.".parse::<Pattern>().unwrap();
        assert_eq!(pat, flipped);

        pat.flip(&Split::Horiz);
        let flipped = START.parse::<Pattern>().unwrap();
        assert_eq!(pat, flipped);
    }

    #[test]
    fn it_flips_vert_split() {
        let mut pat = START.parse::<Pattern>().unwrap();

        pat.flip(&Split::Vert);
        let flipped = ".#./#../###".parse::<Pattern>().unwrap();
        assert_eq!(pat, flipped);

        pat.flip(&Split::Vert);
        let flipped = START.parse::<Pattern>().unwrap();
        assert_eq!(pat, flipped);
    }

    #[test]
    fn enhancement() {
        let res = ENHANCEEEE(START, TEST_RULES, 2).unwrap();
        assert_eq!(res, 12);
    }
}

