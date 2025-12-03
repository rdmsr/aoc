let read_all_lines f: string list = In_channel.with_open_text f In_channel.input_lines

let rec find_highest s =
  let rec aux idx highest highidx =
    if idx >= String.length s then (highidx, highest)
    else
      let c = String.get s idx in
      let digit = Char.code c - Char.code '0' in
        aux (idx + 1) (max highest digit) (if digit > highest then idx else highidx)
  in
  aux 0 (-1) (-1)


let rec process_line s n =
  let rec aux idx n acc =
    if n = 0 then acc
    else
      let (highidx, highest) = find_highest (String.sub s idx (String.length s - idx - n + 1)) in
      aux (idx + highidx + 1) (n - 1) (acc * 10 + highest)
  in
  aux 0 n 0


let () =
  let lines = read_all_lines Sys.argv.(1) in
  let result1 = List.fold_left (fun acc line -> acc + process_line line 2) 0 lines in
  let result2 = List.fold_left (fun acc line -> acc + process_line line 12) 0 lines in
  Printf.printf "1: %d\n" result1;
  Printf.printf "2: %d\n" result2;
