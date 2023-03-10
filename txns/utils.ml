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
let run_txns_result (arr: (int * int * string) array) =
  let (rope, count) = 
    Array.fold_left (fun (rope, count) (pos, delNum, insStr) ->
      let rope = 
        if delNum > 0 then
          Piece_rope.delete pos delNum rope
        else
          rope
      in
      let rope =
        if insStr <> String.empty then
          Piece_rope.insert pos insStr rope
        else
          rope
      in
      let count = count + String.length insStr - delNum in
      rope, count
    ) (Piece_rope.empty, 0) arr in
  (rope, count)

let run_txns title arr =
  time_func title (fun _ -> run_txns_result arr)

let run_substring_result rope =
  let half_length = (Piece_rope.metadata rope).utf32_subtree / 2 in
  let quarter_length = half_length / 2 in
  let str = Piece_rope.substring quarter_length half_length rope in
  str

let run_substring title rope =
  time_func title (fun _ -> run_substring_result rope)
