/*! Day1
http://adventofcode.com/2017/day/1

*/
extern crate d01;


static INPUT: &'static str = include_str!("../../input.txt");


pub fn main() {
    println!("day1-part1: {}", d01::solve1(INPUT));
    println!("day1-part2: {}", d01::solve2(INPUT));

    println!("extras...");
    println!("day1-bench_part1_chars_nocopy:            {}", d01::solve1_nocopy(INPUT));
    println!("day1-bench_part1_chars_nocopy_indexchain: {}", d01::solve1_indexchain(INPUT));
    println!("day1-bench_part1_bytes_copy:              {}", d01::solve1_bytes(INPUT));
    println!("day1-bench_part1_bytes_nocopy:            {}", d01::solve1_bytes_nocopy(INPUT));
    println!("day1-bench_part1_bytes_nocopy_indexchain: {}", d01::solve1_bytes_indexchain(INPUT));
    println!("day1-bench_part1_bytes_nocharcast:        {}", d01::solve1_bytes_nocast(INPUT));
    println!("day1-bench_part1_bytes_nocopy_iterator:   {}", d01::solve1_bytes_nocopy_iterator(INPUT));
    println!("day1-bench_part1_bytes_nocopy_iterator_nocharcast:   {}", d01::solve1_bytes_nocopy_iterator_nocharcast(INPUT));
}


#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test1() {
        [("1122", 3), ("1111", 4), ("1234", 0), ("91212129", 9)].iter().for_each(|&(input, ans)| {
            assert_eq!(d01::solve1(input), ans, "input `{}` expected output `{}`", input, ans);
        })
    }

    #[test]
    fn test2() {
        [("1212", 6), ("1221", 0), ("123425", 4), ("123123", 12), ("12131415", 4)].iter().for_each(|&(input, ans)| {
            assert_eq!(d01::solve2(input), ans, "input `{}` expected output `{}`", input, ans);
        })
    }
}
