let read_all_lines f : string list =
  Stdlib.In_channel.with_open_text f In_channel.input_lines

let square x = x * x

let distance (x, y, z) (x', y', z') =
  square (x' - x) + square (y' - y) + square (z' - z)

let parse_coord s = Scanf.sscanf s "%d,%d,%d" (fun a b c -> (a, b, c))

module Coord = struct
  type t = int * int * int

  let compare = compare
end

module CoordSet = Set.Make (Coord)

let find_set elem arr =
  let rec aux i =
    if i = Array.length arr then None
    else if CoordSet.mem elem arr.(i) then Some (i, arr.(i))
    else aux (i + 1)
  in
  match aux 0 with Some x -> x | None -> failwith "wtf"

let rec make_coords acc = function
  | [] -> acc
  | line :: rest -> make_coords (parse_coord line :: acc) rest

let rec make_edges acc = function
  | [] -> acc
  | x :: xs ->
      let new_acc =
        xs |> List.fold_left (fun acc elem -> (x, elem) :: acc) []
      in
      make_edges (new_acc @ acc) xs

let insert_in_heap heap n elem =
  if (Pairing_heap.length heap) < n then
	Pairing_heap.add heap elem
else
	Pairing_heap.remove_top heap;
  Pairing_heap.add heap elem

let heap_reversed_list heap =
  let rec aux acc =
    match Pairing_heap.pop heap with
    | None -> acc
    | Some x -> aux (x :: acc)
  in
  List.rev (aux [])

let part1 lines n =
  let coords = make_coords [] lines in
  let coords_arr = Array.of_list coords in
  let arr =
    Array.init (List.length coords) (fun i ->
          CoordSet.empty |> CoordSet.add coords_arr.(i))
  in

  let rec connect_edges i arr = function
    | [] -> arr
    | (x1, x2) :: xs ->
        if i = 0 then arr
        else
          let iset1, set1 = find_set x1 arr in
          let iset2, set2 = find_set x2 arr in
          if iset1 <> iset2 then begin
            arr.(iset1) <- CoordSet.union set1 set2;
            arr.(iset2) <- CoordSet.empty
          end;
          connect_edges (i - 1) arr xs
  in
  let heap = Pairing_heap.create ~min_size:n ~cmp:(fun (a, b) (c, d) -> distance c d - distance a b) () in

  make_edges [] coords
	|> List.iter (fun x -> insert_in_heap heap n x);

  heap_reversed_list heap
  |> connect_edges n arr
  |> Array.sort (fun a b -> compare (CoordSet.cardinal b) (CoordSet.cardinal a));

  CoordSet.cardinal arr.(0)
  * CoordSet.cardinal arr.(1)
	* CoordSet.cardinal arr.(2)

let part2 lines =
  let coords = make_coords [] lines in
  let coords_arr = Array.of_list coords in
  let arr =
    Array.init (List.length coords) (fun i ->
        CoordSet.empty |> CoordSet.add coords_arr.(i))
  in
  let rec connect_edges acc arr = function
    | [] -> acc
    | (e1, e2) :: xs ->
        let x1, _, _ = e1 in
        let x2, _, _ = e2 in
        let iset1, set1 = find_set e1 arr in
        let iset2, set2 = find_set e2 arr in
        let acc =
          if iset1 <> iset2 then begin
            arr.(iset1) <- CoordSet.union set1 set2;
            arr.(iset2) <- CoordSet.empty;
            x1 * x2
          end
          else acc
        in
        connect_edges acc arr xs
  in
  make_edges [] coords
  |> List.sort (fun (a, b) (c, d) -> distance a b - distance c d)
  |> connect_edges 0 arr


let () =
  let lines = read_all_lines Sys.argv.(1) in
  let result1 = part1 lines 1000 in
  let result2 = part2 lines in
  Printf.printf "1: %d\n" result1;
  Printf.printf "2: %d\n" result2
