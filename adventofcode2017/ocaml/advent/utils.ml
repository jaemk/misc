module B = Batteries

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


let read_file ?(strip = true) fname =
    let lines = B.File.lines_of fname in
    let buf = Buffer.create 0 in
    B.Enum.iter (fun s -> Buffer.add_string buf s) lines;
    let s = Buffer.contents buf in
    if strip then String.trim s else s
