open Piecerope

(* Runs an empty ropr throught the edit trace, printing time taken and returning resulting rope when done. *)
let run_txns (title: string) (arr: (int * int * string) array) =
  let title = "\nStarting " ^ title ^ "...\n" in
  let _ = Printf.printf "%s" title in
  let t = Sys.time() in
  let rope = Array.fold_left (fun acc (pos, delNum, insStr) ->
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
    rope) Piece_rope.empty arr in
  let endTime = (Sys.time() -. t) *. 1000.0 in
  let _ = (Printf.printf "Execution time: %f ms\n" endTime) in
  rope

let () = 
  let _ = run_txns "Svelete" Sveltecomponent.data in
  let _ = run_txns "Rustcode" Rustcode.data in
  let _ = run_txns "Sephblog" Sephblog.data in
  let _ = run_txns "Automerge" Automerge.data in
  ()
