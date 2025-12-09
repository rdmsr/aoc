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

let rect_bounds (x1, y1) (x2, y2) = (min x1 x2, max x1 x2, min y1 y2, max y1 y2)

let crosses (x_lo, x_hi, y_lo, y_hi) (lx, ly) (lx', ly') =
  not
    (max lx lx' <= x_lo
    || min lx lx' >= x_hi
    || max ly ly' <= y_lo
    || min ly ly' >= y_hi)

let in_polygon a b vertices =
  let bounds = rect_bounds a b in
  let n = Array.length vertices in

  let rec check_intersections i =
    if i = n then false
    else
      let lx, ly = vertices.(i) in
      let lx', ly' = vertices.((i + 1) mod n) in
      if crosses bounds (lx, ly) (lx', ly') then true
      else check_intersections (i + 1)
  in
  not (check_intersections 0)

let part2 lines =
  let coords = List.map parse_coord lines in
  let vertices = Array.of_list coords in
  let areas = ref [] in
  for_each_edge coords (fun a b -> areas := ((a, b), area a b) :: !areas);

  !areas
  |> List.sort (fun (_, a) (_, b) -> compare b a)
  |> List.find (fun ((a, b), _) -> in_polygon a b vertices)
  |> snd

let () =
  let lines = read_all_lines Sys.argv.(1) in
  let result1 = part1 lines in
  let result2 = part2 lines in
  Printf.printf "1: %d\n" result1;
  Printf.printf "2: %d\n" result2
