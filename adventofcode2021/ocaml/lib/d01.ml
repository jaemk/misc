let input ?(file = Sys.getenv "INPUT_DIR" ^ "d01.txt") () =
  let ic = In_channel.open_text file in
  let s = In_channel.input_all ic in
  In_channel.close ic;

  let lines = Core.String.split_lines s in
  List.map int_of_string lines

let part1 data =
  let c = ref 0 in
  let last = ref (List.hd data) in
  List.iter
    (fun x ->
      if x > !last then incr c;
      last := x)
    (List.tl data);
  !c

let part2 data =
  let sum xs = List.fold_left (fun a b -> a + b) 0 xs in
  let window, more = Core.List.split_n data 3 in
  let c = ref 0 in
  let last = ref (sum window) in
  let window = ref window in
  List.iter
    (fun x ->
      window := List.tl !window @ [ x ];
      let sum' = sum !window in
      if sum' > !last then incr c;
      last := sum')
    more;
  !c

let run () =
  let data = input () in
  Printf.printf "** Day 1 **\n";
  let p1, elapsed = Utils.time_ms (fun () -> part1 data) in
  Printf.printf "Part 1: %d (%.2f ms)\n" p1 elapsed;
  let p2, elapsed = Utils.time_ms (fun () -> part2 data) in
  Printf.printf "Part 2: %d (%.2f ms)\n" p2 elapsed
