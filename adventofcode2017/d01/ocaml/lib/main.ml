module P = Printf
module B = Batteries


let read_file ?(strip = true) fname =
    let lines = B.File.lines_of fname in
    let buf = Buffer.create 0 in
    B.Enum.iter (fun s -> Buffer.add_string buf s) lines;
    let s = Buffer.contents buf in
    if strip then String.trim s else s


let string_of_list l fmt =
    let output = B.IO.output_string () in
    B.List.print
        ~first:"["
        ~sep:", "
        ~last:"]"
        (fun o x -> B.Printf.fprintf o fmt x)
        output
        l;
    B.IO.close_out output


let parse_input input_s =
    List.init
        (String.length input_s)
        (fun i ->
            String.get input_s i
            |> Char.escaped
            |> int_of_string)


let checksum input offset_fn =
    let check a b = match (a, b) with
        (Some a, Some b) -> if a = b then a else 0
        | _ -> 0
    in

    let pairs = offset_fn input in
    List.fold_left (fun acc (a, b) -> acc + (check a b)) 0 pairs


let checksum_a input =
    let offset_optional_pairs elems =
        let opts = List.map (fun x -> Some x) elems in
        let a = opts @ [None] in
        let b = (List.hd opts) :: opts in
        List.combine a b
    in
    checksum input offset_optional_pairs


let checksum_b input =
    let offset_optional_pairs elems =
        let half_size = (List.length elems) / 2 in
        let opts = List.map (fun x -> Some x) elems in
        let b_iter = B.List.enum opts in
        let first_half = B.Enum.take half_size b_iter |> B.List.of_enum in
        let last_half = B.Enum.take half_size b_iter |> B.List.of_enum in
        let flipped = last_half @ first_half in
        List.combine opts flipped
    in
    checksum input offset_optional_pairs
