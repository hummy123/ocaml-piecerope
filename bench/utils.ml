(* Times any function and returns the output of that function. *)
let time_func title f =
  let title = "\nStarting " ^ title ^ "...\n" in
  let _ = Printf.printf "%s" title in
  let t = Sys.time () in
  let x = f () in
  let endTime = (Sys.time () -. t) *. 1000.0 in
  let _ = Printf.printf "Execution time: %f ms\n" endTime in
  x

(* Runs an empty rope throught the edit trace, resulting rope when done. *)
let run_txns_result_generic initial (arr : (int * int * string) array) f_ins
    f_del =
  Array.fold_left
    (fun rope (pos, del_num, ins_str) ->
      let rope = if del_num > 0 then f_del pos del_num rope else rope in
      if ins_str <> String.empty then f_ins pos ins_str rope else rope)
    initial arr

let run_txns_time title arr initial f_ins f_del =
  time_func title (fun _ -> run_txns_result_generic initial arr f_ins f_del)

let run_txns_piece_rope title (arr : (int * int * string) array) =
  run_txns_time title arr Piece_rope.empty
    (fun pos ins_str rope -> Piece_rope.insert pos ins_str rope)
    (fun pos del_num rope -> Piece_rope.delete pos del_num rope)

let run_substring_result rope =
  let stats = Piece_rope.stats rope in
  let half_length = stats.utf32_length / 2 in
  let quarter_length = half_length / 2 in
  Piece_rope.substring quarter_length half_length rope

let run_substring title rope =
  time_func title (fun _ -> run_substring_result rope)
