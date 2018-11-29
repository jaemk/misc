module P = Printf
module B = Batteries


let parse_input input_s =
    List.init
        (String.length input_s)
        (fun i ->
            String.get input_s i
            |> Char.escaped
            |> int_of_string)


let captcha input offset_fn =
    let check a b = match (a, b) with
        (Some a, Some b) -> if a = b then a else 0
        | _ -> 0
    in
    let pairs = offset_fn input in
    List.fold_left (fun acc (a, b) -> acc + (check a b)) 0 pairs


let captcha_a input =
    let offset_optional_pairs elems =
        let opts = List.map (fun x -> Some x) elems in
        let a = opts @ [None] in
        let b = (List.hd opts) :: opts in
        List.combine a b
    in
    captcha input offset_optional_pairs


let captcha_b input =
    let offset_optional_pairs elems =
        let half_size = (List.length elems) / 2 in
        let opts = List.map (fun x -> Some x) elems in
        let b_iter = B.List.enum opts in
        let first_half = B.Enum.take half_size b_iter |> B.List.of_enum in
        let last_half = B.Enum.take half_size b_iter |> B.List.of_enum in
        let flipped = last_half @ first_half in
        List.combine opts flipped
    in
    captcha input offset_optional_pairs
