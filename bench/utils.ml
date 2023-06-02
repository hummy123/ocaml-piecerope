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
    (fun (rope, num_chars) (pos, del_num, ins_str) ->
      let rope = if del_num > 0 then f_del pos del_num rope else rope in
      let rope =
        if ins_str <> String.empty then f_ins pos ins_str rope else rope
      in
      let num_chars = num_chars - del_num + String.length ins_str in
      (rope, num_chars))
    (initial, 0) arr

let run_txns_time title arr initial f_ins f_del =
  time_func title (fun _ -> run_txns_result_generic initial arr f_ins f_del)

let run_substring_result rope num_chars f =
  let half_length = num_chars / 2 in
  let start = half_length / 2 in
  f start half_length rope

let run_substring_time title rope num_chars f =
  time_func title (fun _ -> run_substring_result rope num_chars f)
