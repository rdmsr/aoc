open Angstrom
open Z3

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

let rec combinations k lst =
  if k = 0 then [ [] ]
  else
	match lst with
	| [] -> []
	| x :: xs ->
		let with_x = List.map (fun rest -> x :: rest) (combinations (k-1) xs) in
		let without_x = combinations k xs in
		with_x @ without_x

let rec mask_from_button mask len = function
  | [] -> mask
  | x :: xs ->
	  let new_mask = (mask lor (1 lsl (len - 1 - x))) in mask_from_button new_mask len xs


let part1 lines = 
  let parsed = List.map parse_line lines in

  List.fold_left (fun acc line ->
		let light = line |> List.find (function Lights _ -> true | _ -> false) in
		let (len, light_mask) =  
		  match light with
		  | Lights s -> (String.length s, light_to_mask s)
		  | _ -> (0, 0)
		in

		let all_buttons = 
		  List.fold_right (fun item acc ->
				match item with
				| Button b -> (mask_from_button 0 len b) :: acc 
				| _ -> acc            
			) line []

		in
		let find_presses buttons =
		  let rec try_size k =
			let combos = combinations k buttons
			in match List.find_opt
					   (fun combo ->
						   (List.fold_left
							  (lxor) 0 combo) = light_mask
					   ) combos
			   with
			   | Some combo -> List.length combo
			   | None ->
				   if k > List.length buttons then
					failwith "No combination found"
				   else
					 try_size (k+1)
		  in try_size 1
		in acc + find_presses all_buttons) 0 parsed


let solve_machine buttons target =
  let ctx = mk_context [] in
  let opt = Optimize.mk_opt ctx in

  let m = Array.length buttons in     
  let n = Array.length target in

  let xs =
    Array.init m (fun j ->
		  (* Make a variable x_{i} such that x_{i} is >= 0 for every button *)
		  let value = Arithmetic.Integer.mk_const_s ctx ("x" ^ string_of_int j) in
		  let zero = Arithmetic.Integer.mk_numeral_i ctx 0 in
		  Optimize.add opt [Arithmetic.mk_ge ctx value zero];
		  value
    )
  in

  for i = 0 to n - 1 do
	(* Find all buttons affecting this joltage level *)
    let affecting =
      List.filter_map (fun j ->
        if List.mem i buttons.(j) then Some xs.(j) else None
      ) (List.init m (fun j -> j))
    in

    let sum =
      match affecting with
      | [] -> Arithmetic.Integer.mk_numeral_i ctx 0
      | _  -> Arithmetic.mk_add ctx affecting
    in
	(* Sum of all affecting button presses = desired joltage level *)
    let rhs = Arithmetic.Integer.mk_numeral_i ctx target.(i) in
    Optimize.add opt [Boolean.mk_eq ctx sum rhs];
  done;

  (* Minimize the sum of all variables (button presses) *)
  let sum = Arithmetic.mk_add ctx (Array.to_list xs) in
  ignore (Optimize.minimize opt sum);

  match Optimize.check opt with
  | Solver.SATISFIABLE ->
	  (let model = match Optimize.get_model opt with
			| Some a -> a
			| None -> failwith "wtf"
	   in match (Model.eval model sum true) with
		  | Some v -> int_of_string (Expr.to_string v)
		  | _ -> 0)

  | _ ->
      failwith "No solution"


let part2 lines = 
  let parsed = List.map parse_line lines in

  List.fold_left (fun acc line ->
		let joltage_item = line |> List.find (function Joltage _ -> true | _ -> false) in

		let joltage = match joltage_item with
			| Joltage a -> a
			|_ -> failwith "wtf"
		in

		let all_buttons = 
		  List.fold_right (fun item acc ->
				match item with
				| Button b -> b :: acc 
				| _ -> acc            
			) line []

		in acc + solve_machine (Array.of_list all_buttons) (Array.of_list joltage)) 0 parsed

let () =
  let lines = read_all_lines Sys.argv.(1) in
  let result1 = part1 lines in
  let result2 = part2 lines in
  Printf.printf "1: %d\n" result1;
  Printf.printf "2: %d\n" result2;
