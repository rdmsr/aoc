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

let for_each_edge coords f =
  let rec aux xs =
    match xs with
    | [] -> ()
    | x :: xs' ->
        List.iter (f x) xs';
        aux xs'
  in
  aux coords

let rec make_coords acc = function
  | [] -> acc
  | line :: rest -> make_coords (parse_coord line :: acc) rest

let insert_in_heap heap n elem cmp =
  if Pairing_heap.length heap < n then Pairing_heap.add heap elem
  else begin
    match Pairing_heap.top heap with
    | Some top ->
        if cmp elem top < 0 then begin
          Pairing_heap.remove_top heap;
          Pairing_heap.add heap elem
        end
    | None -> Pairing_heap.add heap elem
  end

let heap_ordered_list heap =
  let rec aux acc =
    match Pairing_heap.pop heap with None -> acc | Some x -> aux (x :: acc)
  in
  aux []

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
  let heap =
    Pairing_heap.create ~min_size:n
      ~cmp:(fun (a, b) (c, d) -> distance c d - distance a b)
      ()
  in

  for_each_edge coords (fun x elem ->
      insert_in_heap heap n (x, elem) (fun (a, b) (c, d) ->
          distance a b - distance c d));

  heap_ordered_list heap |> connect_edges n arr
  |> Array.sort (fun a b -> compare (CoordSet.cardinal b) (CoordSet.cardinal a));

  CoordSet.cardinal arr.(0)
  * CoordSet.cardinal arr.(1)
  * CoordSet.cardinal arr.(2)

let part2 lines =
  let coords = make_coords [] lines in
  let coords_arr = Array.of_list coords in
  let n = Array.length coords_arr in
  let arr =
    Array.init n (fun i -> CoordSet.empty |> CoordSet.add coords_arr.(i))
  in

  let rec connect_edges i acc heap =
    if i = 1 then acc
    else
      match Pairing_heap.pop heap with
      | None -> acc
      | Some (e1, e2) ->
          let x1, _, _ = e1 in
          let x2, _, _ = e2 in
          let iset1, set1 = find_set e1 arr in
          let iset2, set2 = find_set e2 arr in
          if iset1 <> iset2 then begin
            arr.(iset1) <- CoordSet.union set1 set2;
            arr.(iset2) <- CoordSet.empty;
            connect_edges (i - 1) (x1 * x2) heap
          end
          else connect_edges i acc heap
  in
  let heap =
    Pairing_heap.create
      ~cmp:(fun (a, b) (c, d) -> distance a b - distance c d)
      ()
  in
  for_each_edge coords (fun x elem -> Pairing_heap.add heap (x, elem));

  connect_edges n 0 heap

let () =
  let lines = read_all_lines Sys.argv.(1) in
  let result1 = part1 lines 1000 in
  let result2 = part2 lines in
  Printf.printf "1: %d\n" result1;
  Printf.printf "2: %d\n" result2
