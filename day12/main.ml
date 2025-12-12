(* This works with the input trust me bro *)

open Angstrom

let read_all_lines f : string list =
  In_channel.with_open_text f In_channel.input_lines

let is_digit = function '0' .. '9' -> true | _ -> false
let blank_line = skip_many1 end_of_line

let index_line =
  lift2
    (fun n _ -> n)
    (take_while1 is_digit >>| int_of_string)
    (char ':' *> end_of_line)

let shape_row =
  take_while1 (function '.' | '#' -> true | _ -> false) <* end_of_line

let shape_block = many1 shape_row
let present = lift2 (fun idx rows -> (idx, rows)) index_line shape_block
let presents = sep_by1 blank_line present

let region_line =
  let int = take_while1 is_digit >>| int_of_string in
  let size = lift3 (fun w _ h -> (w, h)) int (char 'x') int in
  lift3
    (fun (w, h) _ nums -> ((w, h), nums))
    size (string ": ")
    (sep_by1 (char ' ') int)

let regions = sep_by1 end_of_line region_line
let full_file = lift2 (fun ps rs -> (ps, rs)) presents (blank_line *> regions)

let part1 presents regions =
  let present_array = Array.make (List.length presents) 0 in
  List.iter
    (fun (idx, shape) ->
      let area =
        List.fold_left
          (fun acc row ->
            acc
            + String.fold_left (fun c ch -> if ch = '#' then c + 1 else c) 0 row)
          0 shape
      in
      present_array.(idx) <- area)
    presents;

  List.fold_left
    (fun acc region ->
      let (w, h), nums = region in
      let region_area = w * h in
      let total_area =
        List.fold_left
          (fun (i, acc) n -> (acc + (present_array.(i) * n), i + 1))
          (0, 0) nums
        |> snd
      in

      if total_area < region_area then acc + 1 else acc)
    0 regions

let () =
  let lines = read_all_lines Sys.argv.(1) in
  let input = String.concat "\n" lines in
  let result =
    match parse_string ~consume:Consume.All full_file input with
    | Ok (presents, regions) -> part1 presents regions
    | Error msg -> failwith msg
  in
  Printf.printf "1: %d\n" result
