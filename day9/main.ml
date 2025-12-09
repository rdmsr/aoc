let read_all_lines f : string list =
  In_channel.with_open_text f In_channel.input_lines

let area (x, y) (x', y') = abs (x - x' + 1) * abs (y - y' + 1)
let parse_coord s = Scanf.sscanf s "%d,%d" (fun a b -> (a, b))

let for_each_edge coords f =
  let rec aux xs =
    match xs with
    | [] -> ()
    | x :: xs' ->
        List.iter (f x) xs';
        aux xs'
  in
  aux coords

let part1 lines =
  let coords = List.map parse_coord lines in
  let max_area = ref 0 in
  for_each_edge coords (fun a b ->
      let area' = area a b in
      if area' > !max_area then max_area := area');
  !max_area

let () =
  let lines = read_all_lines Sys.argv.(1) in
  let result1 = part1 lines in
  Printf.printf "1: %d\n" result1
