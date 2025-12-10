let read_all_lines f : string list =
  In_channel.with_open_text f In_channel.input_lines

let area (x, y) (x', y') = (abs (x - x') + 1) * (abs (y - y') + 1)
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

let make_edges vertices =
  let n = Array.length vertices in
  Array.init n (fun i ->
      let lx, ly = vertices.(i) in
      let lx', ly' = vertices.((i + 1) mod n) in
      (min lx lx', max lx lx', min ly ly', max ly ly'))

let in_polygon (x1, y1) (x2, y2) edge_bounds n =
  let x_lo = min x1 x2 in
  let x_hi = max x1 x2 in
  let y_lo = min y1 y2 in
  let y_hi = max y1 y2 in
  let rec check i =
    if i = n then true
    else
      let ex_min, ex_max, ey_min, ey_max = Array.unsafe_get edge_bounds i in
      if ex_max <= x_lo || ex_min >= x_hi || ey_max <= y_lo || ey_min >= y_hi
      then check (i + 1)
      else false
  in
  check 0

let part2 lines =
  let coords = List.map parse_coord lines in
  let vertices = Array.of_list coords in
  let edges = make_edges vertices in
  let n = Array.length edges in

  let best = ref 0 in
  for_each_edge coords (fun a b ->
      let ar = area a b in
      if ar > !best && in_polygon a b edges n then best := ar);
  !best

let () =
  let lines = read_all_lines Sys.argv.(1) in
  let result1 = part1 lines in
  let result2 = part2 lines in
  Printf.printf "1: %d\n" result1;
  Printf.printf "2: %d\n" result2
