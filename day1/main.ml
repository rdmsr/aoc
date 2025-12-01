let read_all_lines f: string list = In_channel.with_open_text f In_channel.input_lines

let parse_line line =
  Scanf.sscanf line "%c%d" (fun direction distance -> (direction, distance))

let move x direction distance =
  match direction with
  | 'L' -> x - distance
  | 'R' -> x + distance
  | _ -> x

let modulo x y = ((x mod y) + y) mod y

let move2 x direction distance =
  let new_x = move x direction distance in
  let passes = 
    if new_x <= 0 then abs(new_x / 100) + 1 - (if x = 0 then 1 else 0)
    else (new_x / 100) in 
  (new_x, passes)

let rec part1 instructions x n =
  match instructions with
  | [] -> n
  | line :: rest ->
      let (direction, distance) = parse_line line in
      let new_x = modulo (move x direction distance) 100 in
      part1 rest new_x (if new_x = 0 then n + 1 else n)


let rec part2 instrs x n = 
  match instrs with
  | [] -> n
  | line :: rest ->
      let (direction, distance) = parse_line line in
      let (new_x, passes) = (move2 x direction distance) in
      part2 rest (modulo new_x 100) (n + passes)
  
let () =
  let lines = read_all_lines Sys.argv.(1) in
  let (result1, result2) = (part1 lines 50 0, part2 lines 50 0) in
  Printf.printf "1: %d 2: %d\n" result1 result2