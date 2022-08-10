let sample =
  {s|
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
|s}

let part1_sample1 () =
  Alcotest.(check int)
    "part 1 sample 1" 198
    (Advent.D03.part1 @@ Advent.D03.parse sample)

let part1_real () =
  Alcotest.(check int)
    "part 1 real" 1082324
    (Advent.D03.part1 @@ Advent.D03.parse (Advent.D03.input ()))

let collect_tests () =
  let open Alcotest in
  [
    ( "Day 3",
      [
        test_case "Part 1 sample 1" `Quick part1_sample1;
        test_case "Part 1 real" `Quick part1_real;
      ] );
  ]
