let () =
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

  (* Below functions run the edit traces with each dataset. *)
  let _ =
    Utils.run_txns_time 
    "Svelete Piece_rope" (* Name of data set with structure name (displayed in terminal). *)
    Sveltecomponent.data (* Dataset to run. *)
      Piece_rope.empty (* The initial/empty structure to start with. *)
      piece_rope_insert (* The insert function we previously defined. *)
      piece_rope_delete (* The delete function we previously defined. *)
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
  let zed_insert pos (ins_str: string) rope = 
    let ins_str = Zed_string.of_utf8 ins_str in
    Zed_rope.insert rope pos (Zed_rope.of_string ins_str)
  in
  let zed_delete pos del_num rope =
    Zed_rope.remove rope pos del_num
  in

  let _ =
    Utils.run_txns_time "Svelte Zed" Sveltecomponent.data (Zed_rope.empty())
      zed_insert zed_delete
  in
  let _ =
    Utils.run_txns_time "Rustcode Zed" Rustcode.data (Zed_rope.empty())
      zed_insert zed_delete
  in
  let _ =
    Utils.run_txns_time "Sephblog Zed" Sephblog.data (Zed_rope.empty())
      zed_insert zed_delete
  in
  let _ =
    Utils.run_txns_time "Automerge Zed" Automerge.data (Zed_rope.empty())
      zed_insert zed_delete
  in
  ()
