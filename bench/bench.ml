(*
    Function to insert into a structure, returning the structure modified.
    First parameter: index to insert at.
    Second: string to insert.
    Third: the structure (it acts as an accumulator value).
*)
let piece_rope_insert pos ins_str rope = Piece_rope.insert pos ins_str rope in
(*
    Function to delete from a structure, returning the structure modified.
    First parameter: index to start deleting from.
    Second: length to delete.
    Third: the structure (it acts as an accumulator value).
*)
let piece_rope_delete pos del_num rope = Piece_rope.delete pos del_num rope in

(* Zed_rope insertion and deletion functions. *)
let zed_insert pos (ins_str : string) rope =
  ins_str |> Zed_string.of_utf8 |> Zed_rope.of_string
  |> Zed_rope.insert rope pos
in

let zed_delete pos del_num rope = Zed_rope.remove rope pos del_num in

let () =
  (* Below functions run the edit traces with each dataset. *)
  (* Structure:
     let _ =
       Utils.run_txns_time
       (* Name of data set with structure name (displayed in terminal). *)
       (* Dataset to run. *)
       (* The initial/empty structure to start with. *)
       (* The insert function we previously defined. *)
       (* The delete function we previously defined. *)
     in
  *)
  let _ =
    Utils.run_txns_time "Svelete Piece_rope" Sveltecomponent.data
      Piece_rope.empty piece_rope_insert piece_rope_delete
  in
  let _ =
    Utils.run_txns_time "Rustcode Piece_rope" Rustcode.data Piece_rope.empty
      piece_rope_insert piece_rope_delete
  in
  let _ =
    Utils.run_txns_time "Sephblog Piece_rope" Sephblog.data Piece_rope.empty
      piece_rope_insert piece_rope_delete
  in
  let _ =
    Utils.run_txns_time "Automerge Piece_rope" Automerge.data Piece_rope.empty
      piece_rope_insert piece_rope_delete
  in

  (* Running the datasets on Zed. *)
  let _ =
    Utils.run_txns_time "Svelte Zed" Sveltecomponent.data (Zed_rope.empty ())
      zed_insert zed_delete
  in
  let _ =
    Utils.run_txns_time "Rustcode Zed" Rustcode.data (Zed_rope.empty ())
      zed_insert zed_delete
  in
  let _ =
    Utils.run_txns_time "Sephblog Zed" Sephblog.data (Zed_rope.empty ())
      zed_insert zed_delete
  in
  let _ =
    Utils.run_txns_time "Automerge Zed" Automerge.data (Zed_rope.empty ())
      zed_insert zed_delete
  in
  ()
in
()
