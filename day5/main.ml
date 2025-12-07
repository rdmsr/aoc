let read_all_lines f : string list =
  In_channel.with_open_text f In_channel.input_lines

let process_range line = Scanf.sscanf line "%d-%d" (fun a b -> (a, b))

let process_lines lines =
  let rec aux lines ids id_list range_list =
    match lines with
    | [] -> (id_list, range_list)
    | x :: xs ->
        if String.length x = 0 then aux xs true id_list range_list
        else if ids = true then
          aux xs ids (int_of_string x :: id_list) range_list
        else
          let range = process_range x in
          aux xs ids id_list (range :: range_list)
  in
  aux lines false [] []

let part1 id_list range_list =
  let in_any_range id ranges =
    List.exists (fun (l, u) -> l <= id && id <= u) ranges
  in
  List.fold_left
    (fun acc elem -> if in_any_range elem range_list then acc + 1 else acc)
    0 id_list

let part2 range_list =
  let sorted = List.sort compare range_list in
  let merged =
    List.fold_left
      (fun acc (l, u) ->
        match acc with
        | [] -> [ (l, u) ]
        | (l0, u0) :: xs ->
            if l <= u0 then (l0, max u0 u) :: xs else (l, u) :: acc)
      [] sorted
  in
  List.fold_left (fun acc (l, u) -> acc + (u - l + 1)) 0 merged

let () =
  let lines = read_all_lines Sys.argv.(1) in
  let id_list, range_list = process_lines lines in
  let result1 = part1 id_list range_list in
  let result2 = part2 range_list in
  Printf.printf "1: %d\n" result1;
  Printf.printf "2: %d\n" result2
