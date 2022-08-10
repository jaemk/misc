let input ?(file = Sys.getenv "INPUT_DIR" ^ "d03.txt") () =
  let ic = In_channel.open_text file in
  let s = In_channel.input_all ic in
  In_channel.close ic;
  s

(* parse to array instead of list *)
let parse s =
  Core.String.strip s |> Core.String.split_lines
  |> List.map (fun s -> Core.String.strip s |> Core.String.to_array)

let part1 nums =
  let ga = ref "" in
  let ep = ref "" in
  let hd = List.hd nums in
  let width = Array.length hd in
  for i = 0 to width - 1 do
    let ones = ref 0 in
    let zeros = ref 0 in
    nums
    |> List.iter (fun num -> if num.(i) = '0' then incr zeros else incr ones);
    if !ones > !zeros then (
      ga := !ga ^ "1";
      ep := !ep ^ "0")
    else (
      ga := !ga ^ "0";
      ep := !ep ^ "1")
  done;
  let ga = int_of_string ("0b" ^ !ga) in
  let ep = int_of_string ("0b" ^ !ep) in
  ga * ep

let part2 _ = 2

let run () =
  let data = input () |> parse in
  Printf.printf "** Day 3 **\n";
  let p1 = part1 data in
  Printf.printf "Part 1: %d\n" p1;
  let p2 = part2 data in
  Printf.printf "Part 2: %d\n" p2
