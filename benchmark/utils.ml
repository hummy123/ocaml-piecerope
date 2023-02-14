open Piecerope

(* Times any function and returns the output of that function. *)
let time_func title f =
  let title = "\nStarting " ^ title ^ "...\n" in
  let _ = Printf.printf "%s" title in
  let t = Sys.time() in
  let x = f() in
  let endTime = (Sys.time() -. t) *. 1000.0 in
  let _ = (Printf.printf "Execution time: %f ms\n" endTime) in
  x

(* Runs an empty ropr throught the edit trace, resulting rope when done. *)
let run_txns_internal (arr: (int * int * string) array) =
  Array.fold_left (fun acc (pos, delNum, insStr) ->
    let rope = 
      if delNum > 0 then
        Piece_rope.delete pos delNum acc
      else
        acc
    in
    let rope =
      if insStr <> String.empty then
        Piece_rope.insert pos insStr rope
      else
        rope
    in
    rope) Piece_rope.empty arr 

let run_txns title arr =
  time_func title (fun _ -> run_txns_internal arr)

let run_substring_internal rope =
  let half_length = Piece_rope.total_length rope / 2 in
  let quarter_length = half_length / 2 in
  Piece_rope.substring quarter_length half_length rope

let run_substring title rope =
  time_func title (fun _ -> run_substring_internal rope)
