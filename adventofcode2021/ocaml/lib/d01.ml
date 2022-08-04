let input () =
  let ic = In_channel.open_text "../input/d01.txt" in
  let s = In_channel.input_all ic in
  In_channel.close ic;
  let lines = Core.String.split_lines s in
  List.map int_of_string lines

let part1 _ = "done1"

let part2 () = "done2"

let run () =
  let data = input () in
  Printf.printf "** Day 1 **\n";
  let p1 = part1 data in
  Printf.printf "Part 1: %s\n" p1;
  let p2 = part2 () in
  Printf.printf "Part 2: %s\n" p2
