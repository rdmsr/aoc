let read_all_lines f : string list =
  In_channel.with_open_text f In_channel.input_lines

let part1 lines =
  let start_x = String.index (List.hd lines) 'S' in

  let rec aux acc beams = function
    | [] -> acc
    | line :: rest ->
        let hits, next =
          beams
          |> List.fold_left
               (fun (accn, accl) x ->
                 match line.[x] with
                 | '^' -> (accn + 1, (x - 1) :: (x + 1) :: accl)
                 | '.' -> (accn, x :: accl)
                 | _ -> failwith "Unexpected character")
               (0, [])
        in
        aux (acc + hits) (List.sort_uniq compare next) rest
  in
  aux 0 [ start_x ] (List.tl lines)

let part2 lines =
  let width = String.length (List.hd lines) in
  let start_x = String.index (List.hd lines) 'S' in

  let rec aux arr = function
    | [] -> Array.fold_left ( + ) 0 arr
    | line :: rest ->
        line
        |> String.iteri (fun i c ->
            match c with
            | '^' ->
                arr.(i - 1) <- arr.(i - 1) + arr.(i);
                arr.(i + 1) <- arr.(i + 1) + arr.(i);
                arr.(i) <- 0
            | '.' -> ()
            | _ -> failwith "Unexpected character");
        aux arr rest
  in
  let array = Array.make width 0 in
  array.(start_x) <- 1;
  aux array (List.tl lines)

let () =
  let lines = read_all_lines Sys.argv.(1) in
  let result1 = part1 lines in
  let result2 = part2 lines in
  Printf.printf "1: %d\n" result1;
  Printf.printf "2: %d\n" result2
