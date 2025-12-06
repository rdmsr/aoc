let read_all_lines f: string list = In_channel.with_open_text f In_channel.input_lines

let part1 lines =
  let rows = 
    lines 
    |> List.rev
    |> List.map (fun line ->
         line
         |> String.split_on_char ' '
         |> List.filter (fun s -> String.trim s <> "")) 
    in

  let rec transpose m =
    match m with
    | [] -> []
    | [] :: _ -> []
    | _ -> (List.map List.hd m) :: transpose (List.map List.tl m)
  in
  transpose rows
  |> List.map (function
    | op :: nums -> (String.get op 0, List.map int_of_string nums)
    | _ -> failwith "malformed column") 


let part2 lines =
  let width = String.length (List.hd lines) in

  let read_column idx =
    lines
    |> List.filter_map (fun line ->
         if idx < String.length line then
           let c = line.[idx] in
           if c = ' ' then None else Some c
         else None)
  in
  let rec get_cols acc n op idx =
    if idx >= width then ((op, n) :: acc)
    else 
      match read_column idx with
      | [] -> get_cols  ((op, n) :: acc) [] ' ' (idx+1)
      | chars -> let str = List.to_seq chars |> String.of_seq in
        let len = String.length str in
        let last_char = String.get str (len - 1) in
        let digits = if last_char = '+' || last_char = '*' then
          String.sub str 0 (len - 1) else
            str
          in
        let num = int_of_string digits in
          if last_char = '+' || last_char = '*' then
            get_cols acc (num :: n) last_char (idx+1)
          else
            get_cols acc (num :: n) op (idx+1)
  in get_cols [] [] ' ' 0


let fn_and_acc_for_op op =
  match op with
  | '+' -> (( + ), 0)
  | '*' -> (( * ), 1)
  | _ -> ((fun a b -> 0), 0)

let solve problems =
  List.fold_left(
    fun acc (op, numbers) ->
      let (fn, init) = fn_and_acc_for_op op in
        acc + List.fold_left fn init numbers
  ) 0 problems

let () =
  let lines = read_all_lines Sys.argv.(1) in
  let part1_problems = part1 lines in
  let part2_problems = part2 lines in
  let result1 = solve part1_problems in
  let result2 = solve part2_problems in
  Printf.printf "1: %d\n" result1;
  Printf.printf "2: %d\n" result2;