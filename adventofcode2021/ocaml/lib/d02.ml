let input ?(file = Sys.getenv "INPUT_DIR" ^ "d02.txt") () =
  let ic = In_channel.open_text file in
  let s = In_channel.input_all ic in
  In_channel.close ic;
  s

type direction = Up | Down | Forward

let direction_of_string s =
  match s with
  | "forward" -> Forward
  | "up" -> Up
  | "down" -> Down
  | s -> failwith (Format.sprintf "invalid direction %s" s)

let direction_pp dir =
  match dir with Forward -> "forward" | Up -> "up" | Down -> "down"

type move = { dir : direction; amnt : int }

let create_move dir amnt = { dir; amnt }
let move_pp ppf { dir; amnt } = Fmt.pf ppf "[%s|%d]" (direction_pp dir) amnt
let move_equal a b = a.dir = b.dir && a.amnt = b.amnt

let parse s =
  Core.String.strip s |> Core.String.split_lines
  |> List.map (fun s ->
         let parts = Core.String.split s ~on:' ' in
         let dir, amnt = Core.List.split_n parts 1 in
         let dir = List.hd dir |> direction_of_string in
         let amnt = List.hd amnt |> int_of_string in
         create_move dir amnt)

let part1 moves =
  let pos = ref 0 in
  let dep = ref 0 in
  moves
  |> List.iter (fun m ->
         match m.dir with
         | Forward -> pos := !pos + m.amnt
         | Up -> dep := !dep - m.amnt
         | Down -> dep := !dep + m.amnt);
  !pos * !dep

let part2 _ = 2

let run () =
  let data = input () |> parse in
  Printf.printf "** Day 2 **\n";
  let p1 = part1 data in
  Printf.printf "Part 1: %d\n" p1;
  let p2 = part2 data in
  Printf.printf "Part 2: %d\n" p2
