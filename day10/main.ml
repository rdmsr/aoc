open Angstrom
let read_all_lines f : string list =
  In_channel.with_open_text f In_channel.input_lines

type item =
  | Lights of string
  | Button of int list
  | Joltage of int list

let ws = skip_many (char ' ')

let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false)
  >>| int_of_string

let comma_sep p =
  sep_by (char ',' *> ws) p

let lights =
  char '[' *> take_till (fun c -> c = ']') <* char ']'
  >>| fun s -> Lights s

let button =
  char '(' *> ws *> comma_sep integer <* ws <* char ')'
  >>| fun xs -> Button xs

let joltage =
  char '{' *> ws *> comma_sep integer <* ws <* char '}'
  >>| fun xs -> Joltage xs

let item = lights <|> button <|> joltage

let items =
  ws *> many (item <* ws)


let parse_line s =
  match parse_string ~consume:Consume.All items s with
  | Ok v -> v
  | Error msg -> failwith msg

let light_to_mask s =
  String.fold_left (fun acc c ->
    match c with
    | '.' -> (acc lsl 1)
    | '#' -> (acc lsl 1) lor 1
    | _ -> acc
  ) 0 s

let part1 lines = 
  let parsed = List.map parse_line lines in

  List.fold_left (fun acc line ->
    let light = line |> List.find (function Lights _ -> true | _ -> false) in
    let light_mask =  
      match light with
      | Lights s -> light_to_mask s
      | _ -> 0
    in

    let all_buttons  = 
      List.filter (function Button _ -> true | _ -> false) line
    in
    ignore (all_buttons);
    ignore(light_mask);
    acc
  ) 0 parsed

let () =
  let lines = read_all_lines Sys.argv.(1) in
  let result1 = part1 lines in
  Printf.printf "1: %d\n" result1
