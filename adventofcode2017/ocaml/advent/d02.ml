
type min_max = {
    min: int option;
    max: int option;
}


let reduce_row_a row =
    List.fold_left
        (fun {min = min'; max = max'} x -> match (min', max') with
            (None, None) -> { min = Some x; max = Some x}
            | (Some min', Some max') -> { min = Some (min min' x); max = Some (max max' x)}
            | _ -> failwith "Invalid input")
        { min = None; max = None }
        row

let sum_row_a {min = min'; max = max'} =
    match (min', max') with
        (Some min', Some max') -> max' - min'
        | _ -> failwith "empty row"

let checksum_a row =
    sum_row_a @@ reduce_row_a row
