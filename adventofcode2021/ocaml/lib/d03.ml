let input ?(file = Sys.getenv "INPUT_DIR" ^ "d03.txt") () =
  let ic = In_channel.open_text file in
  let s = In_channel.input_all ic in
  In_channel.close ic;
  s

let parse s =
  Core.String.strip s |> Core.String.split_lines
  |> List.map (fun s -> Core.String.strip s |> Core.String.to_array)

let part1 nums =
  let ga = String.to_seq "0b" |> Buffer.of_seq in
  let ep = String.to_seq "0b" |> Buffer.of_seq in
  let hd = List.hd nums in
  let width = Array.length hd in
  for i = 0 to width - 1 do
    let ones = ref 0 in
    let zeros = ref 0 in
    nums
    |> List.iter (fun num -> if num.(i) = '0' then incr zeros else incr ones);
    if !ones > !zeros then (
      Buffer.add_char ga '1';
      Buffer.add_char ep '0')
    else (
      Buffer.add_char ga '0';
      Buffer.add_char ep '1')
  done;
  let ga = int_of_string (Buffer.contents ga) in
  let ep = int_of_string (Buffer.contents ep) in
  ga * ep

let scan ?(most_common = true) nums index =
  let ones = ref 0 in
  let zeros = ref 0 in
  nums
  |> List.iter (fun num -> if num.(index) = '0' then incr zeros else incr ones);
  if most_common then if !ones >= !zeros then '1' else '0'
  else if !zeros <= !ones then '0'
  else '1'

let process ?(most_common = true) nums =
  let nums = ref nums in
  let index = ref 0 in
  while List.length !nums > 1 do
    let keep = scan ~most_common !nums !index in
    nums := List.filter (fun num -> num.(!index) = keep) !nums;
    incr index
  done;
  List.hd !nums |> Array.to_seq |> Seq.cons 'b' |> Seq.cons '0' |> Buffer.of_seq
  |> Buffer.contents

let part2 nums =
  let a = process ~most_common:true nums in
  let b = process ~most_common:false nums in
  int_of_string a * int_of_string b

let run () =
  let data = input () |> parse in
  Printf.printf "** Day 3 **\n";
  let p1, elapsed = Utils.time_ms (fun () -> part1 data) in
  Printf.printf "Part 1: %d (%.2f ms)\n" p1 elapsed;
  let p2, elapsed = Utils.time_ms (fun () -> part2 data) in
  Printf.printf "Part 2: %d (%.2f ms)\n" p2 elapsed
