

static INPUT: &'static str = include_str!("../input.txt");
static INPUT2: &'static str = include_str!("../input2.txt");


struct Disc {
    num: usize,
    pos: usize,  // 0..sides
    sides: usize,
}
impl Disc {
    fn pos_after_n_spins(&self, spins: usize) -> usize {
        (0..self.sides).cycle().skip(self.pos).skip(spins).next().unwrap()
    }
}


fn parse_disc_start(input: &str) -> Vec<Disc> {
    input.trim().lines().map(|line| {
        let line = line.split_whitespace().collect::<Vec<_>>();
        Disc {
            num: line[1].trim_left_matches("#").parse().expect("invalid disc number"),
            sides: line[3].parse().expect("invalid number of sides"),
            pos: line[11].trim_right_matches(".").parse().expect("invalid start position"),
        }
    }).collect()
}


fn find_valid_start_time(input: &str) -> usize {
    let discs = parse_disc_start(input);
    let max_disc = discs.iter().max_by_key(|disc| disc.sides).unwrap();
    let max_disc_relative_pos = max_disc.pos_after_n_spins(max_disc.num);
    let mut time = max_disc.sides - max_disc_relative_pos;
    loop {
        let aligned = discs.iter()
            .map(|disc| disc.pos_after_n_spins(time + disc.num))
            .all(|pos| pos == 0);
        if aligned { return time }
        time += max_disc.sides;
    }
}


pub fn main() {
    println!("d15-p1: {}", find_valid_start_time(INPUT));
    println!("d15-p2: {}", find_valid_start_time(INPUT2));
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_spinning_disc() {
        let disc = Disc { num: 0, pos: 1, sides: 5 };
        assert_eq!(disc.pos_after_n_spins(2), 3);
        assert_eq!(disc.pos_after_n_spins(4), 0);
        assert_eq!(disc.pos_after_n_spins(5), 1);
        assert_eq!(disc.pos_after_n_spins(0), 1);
    }

    static TEST_INPUT: &'static str = r##"
Disc #1 has 5 positions; at time=0, it is at position 4.
Disc #2 has 2 positions; at time=0, it is at position 1.
"##;

    #[test]
    fn p1() {
        assert_eq!(find_valid_start_time(TEST_INPUT), 5);
    }
}
