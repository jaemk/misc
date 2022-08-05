let d1_p1_1 () =
  Alcotest.(check int) "2 increases" 2 (Advent.D01.part1 [ 1; 2; 3 ])

let d1_p1_2 () =
  Alcotest.(check int)
    "6 increases" 6
    (Advent.D01.part1 [ 1; 2; 3; 1; 2; 3; 1; 2; 3 ])

let d1_p1_3 () =
  Alcotest.(check int)
    "0 increases" 0
    (Advent.D01.part1 [ 3; 3; 3; 3; 3; 3; 3; 2; 1 ])

let d1_p1_sample_1 () =
  Alcotest.(check int)
    "7 increases" 7
    (Advent.D01.part1 [ 199; 200; 208; 210; 200; 207; 240; 269; 260; 263 ])

let d1_p1_real () =
  Alcotest.(check int)
    "real input 1" 1548
    (Advent.D01.part1 (Advent.D01.input ()))

let d1_p2_sample_1 () =
  Alcotest.(check int)
    "5 increases" 5
    (Advent.D01.part2 [ 199; 200; 208; 210; 200; 207; 240; 269; 260; 263 ])

let d1_p2_real () =
  Alcotest.(check int)
    "real input 2" 1589
    (Advent.D01.part2 (Advent.D01.input ()))

let run_tests () =
  print_endline (Filename.dirname (Sys.getcwd ()));
  let open Alcotest in
  run "advent"
    [
      ( "Day 1",
        [
          test_case "Part 1-1" `Quick d1_p1_1;
          test_case "Part 1-2" `Quick d1_p1_2;
          test_case "Part 1-3" `Quick d1_p1_3;
          test_case "Part 1-sample-1" `Quick d1_p1_sample_1;
          test_case "Part 1-real" `Quick d1_p1_real;
          test_case "Part 2-sample-1" `Quick d1_p2_sample_1;
          test_case "Part 2-real" `Quick d1_p2_real;
        ] );
    ]
