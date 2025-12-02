let read_file f = In_channel.with_open_text f In_channel.input_all


let parse_line l =
  String.trim l
  |> String.split_on_char '-'
  |> List.map int_of_string


let split_half s =
  let len = String.length s in
  let half_len = len / 2 in
  (String.sub s 0 half_len, String.sub s half_len half_len)

let rec count1 boundary n acc =
  match n with
  | x when x = boundary -> acc
  | _ ->
      let num_str = string_of_int n in
      if String.length num_str mod 2 = 0 then
        let (substr, substr_right) = split_half num_str in
        if String.equal substr substr_right then
          count1 boundary (n + 1) (acc + n)
        else
          count1 boundary (n + 1) acc
      else
        count1 boundary (n + 1) acc


let is_repeating s =
  let len = String.length s in
  let rec try_len sub_len =
    if sub_len > len / 2 then false
    else if len mod sub_len != 0 then try_len (sub_len + 1)
    else
      let sub = String.sub s 0 sub_len in
      let rec check i =
        if i = len then true
        else if String.sub s i sub_len = sub then
          check (i + sub_len)
        else
          false
      in
      if check sub_len then true
      else try_len (sub_len + 1)
  in
  try_len 1


let rec count2 boundary n acc =
  if n > boundary then acc
  else
    let s = string_of_int n in
    match is_repeating s with
    | true -> count2 boundary (n + 1) (acc + n)
    | false -> count2 boundary (n + 1) acc



let rec solve f ranges x n =
  match ranges with
  | [] -> n
  | range :: rest ->
     let occ = f (List.nth range 1) (List.nth range 0) 0 in
      solve f rest x (n + occ)


let () =
  let content = read_file Sys.argv.(1) in
  let ranges = List.map parse_line (String.split_on_char ',' content) in
  let result1 = solve count1 ranges 0 0 in
  let result2 = solve count2 ranges 0 0 in
  Printf.printf "2: %d\n" result2;
  Printf.printf "1: %d\n" result1