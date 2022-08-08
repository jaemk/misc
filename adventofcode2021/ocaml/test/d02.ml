let sample = {s|
forward 5
down 5
forward 8
up 3
down 8
forward 2
|s}

let move = Alcotest.testable Advent.D02.move_pp Advent.D02.move_equal

let d2_parse () =
  Alcotest.check
    Alcotest.(list move)
    "parses sample"
    [
      Advent.D02.create_move Advent.D02.Forward 5;
      Advent.D02.create_move Advent.D02.Down 5;
      Advent.D02.create_move Advent.D02.Forward 8;
      Advent.D02.create_move Advent.D02.Up 3;
      Advent.D02.create_move Advent.D02.Down 8;
      Advent.D02.create_move Advent.D02.Forward 2;
    ]
    (Advent.D02.parse sample)

let d2_part1_sample1 () =
  Alcotest.(check int)
    "sample 1" 150
    (Advent.D02.part1 @@ Advent.D02.parse sample)

let d2_part1_real () =
  Alcotest.(check int)
    "part 1 real" 1648020
    (Advent.D02.input () |> Advent.D02.parse |> Advent.D02.part1)

let collect_tests () =
  let open Alcotest in
  [
    ( "Day 2",
      [
        test_case "Parse" `Quick d2_parse;
        test_case "Part 1 sample 1" `Quick d2_part1_sample1;
        test_case "Part 1 real" `Quick d2_part1_real;
      ] );
  ]
