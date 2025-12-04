let read_all_lines f: string list = In_channel.with_open_text f In_channel.input_lines

let parse_lines lines =
  let height = List.length lines in
  let width = String.length (List.hd lines) in
  let grid = Array.make_matrix height width false in
  List.iteri (fun y line ->
    String.iteri (fun x c ->
      grid.(y).(x) <- (c = '@')
    ) line
  ) lines;
  grid


let has_elem grid x y =
  if y < 0 || y >= Array.length grid || x < 0 || x >= Array.length grid.(0) then
    false
  else
    grid.(y).(x)


let count_neighbors grid x y =
  let deltas = [(-1, -1); (0, -1); (1, -1);
                (-1, 0);          (1, 0);
                (-1, 1); (0, 1); (1, 1)] in
  List.fold_left (fun acc (dx, dy) ->
    if has_elem grid (x + dx) (y + dy) then acc + 1 else acc
  ) 0 deltas

let part1 grid =
  let rec aux acc x y =
    if y >= Array.length grid then
      acc
    else if x >= Array.length grid.(0) then
      aux acc 0 (y + 1)
    else
      if has_elem grid x y && count_neighbors grid x y < 4 then
        aux (acc + 1) (x + 1) y
      else
        aux acc (x + 1) y
  in
  aux 0 0 0

let part2 grid =
  let rec aux acc x y removed =
    if y >= Array.length grid then
      if removed = false then acc else
      aux acc 0 0 false
    else if x >= Array.length grid.(0) then
      aux acc 0 (y + 1) removed
    else
      if has_elem grid x y && count_neighbors grid x y < 4 then
        begin
          grid.(y).(x) <- false;
          aux (acc + 1) (x + 1) y true
        end
      else
        aux acc (x + 1) y removed
  in
  aux 0 0 0 false

let () = 
  let lines = read_all_lines Sys.argv.(1) in
  let grid = parse_lines lines in
  let result1 = part1 grid in
  let result2 = part2 grid in
  Printf.printf "1: %d\n" result1;
  Printf.printf "2: %d\n" result2;