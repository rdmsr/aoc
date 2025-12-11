let read_all_lines f : string list =
  In_channel.with_open_text f In_channel.input_lines

let parse_line line =
  let line = String.trim line in
  let split = String.split_on_char ':' line in
  let node = List.hd split in
  let edges =
    match List.tl split with
    | [ x ] -> String.split_on_char ' ' (String.trim x)
    | _ -> []
  in
  (node, edges)

let part1 lines =
  let graph_list = List.map parse_line lines in
  let graph = Hashtbl.create (List.length graph_list) in

  List.iter (fun (node, edges) -> Hashtbl.add graph node edges) graph_list;

  let memo = Hashtbl.create (Hashtbl.length graph) in

  let rec dfs node visited =
    match Hashtbl.find_opt memo node with
    | Some result -> result
    | None ->
        let result =
          if node = "out" then 1
          else
            Hashtbl.find graph node
            |> List.fold_left (fun acc nxt -> acc + dfs nxt visited) 0
        in
        Hashtbl.add memo node result;
        result
  in
  let visited = Hashtbl.create (Hashtbl.length graph) in
  dfs "you" visited

let part2 lines =
  let graph_list = List.map parse_line lines in
  let graph = Hashtbl.create (List.length graph_list) in

  List.iter (fun (node, edges) -> Hashtbl.add graph node edges) graph_list;

  let memo = Hashtbl.create (Hashtbl.length graph) in

  let rec dfs node has_fft has_dac =
    match Hashtbl.find_opt memo (node, has_fft, has_dac) with
    | Some result -> result
    | None ->
        let has_fft = has_fft || node = "fft" in
        let has_dac = has_dac || node = "dac" in
        let result =
          if node = "out" then if has_fft && has_dac then 1 else 0
          else
            List.fold_left
              (fun acc nxt -> acc + dfs nxt has_fft has_dac)
              0 (Hashtbl.find graph node)
        in
        Hashtbl.add memo (node, has_fft, has_dac) result;
        result
  in
  dfs "svr" false false

let () =
  Printexc.record_backtrace true;
  let lines = read_all_lines Sys.argv.(1) in
  let result1 = part1 lines in
  let result2 = part2 lines in
  Printf.printf "1: %d\n" result1;
  Printf.printf "2: %d\n" result2
