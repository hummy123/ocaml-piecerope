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

let piece_rope_substring start length rope =
  Piece_rope.substring start length rope
in

(* Zed_rope insertion and deletion functions. *)
let zed_insert pos (ins_str : string) rope =
  ins_str |> Zed_string.of_utf8 |> Zed_rope.of_string
  |> Zed_rope.insert rope pos
in

let zed_delete pos del_num rope = Zed_rope.remove rope pos del_num in

let zed_substring start length rope = Zed_rope.sub rope start length in

(* OCaml-bazaar rope insertion and deletion functoins. *)
let bazaar_insert pos (ins_str : string) rope =
  Bazaar_rope.S.insert rope pos (Bazaar_rope.S.of_string ins_str)
in

(* The OCaml-bazaar rope doesn't provide a function to delete a range of text as far as I can see,
   but just a function to delete one character. *)
let bazaar_delete pos del_num rope =
  let rec del rope length_left =
    if length_left = 0 then rope
    else
      let rope = Bazaar_rope.S.delete rope pos in
      del rope (length_left - 1)
  in
  del rope del_num
in

let bazaar_substring start length rope = Bazaar_rope.S.sub rope start length in

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
  Printf.printf "\n-\t piece_rope edit traces \t-";
  let piece_svelte, svelte_length =
    Utils.run_txns_time "Svelete Piece_rope" Sveltecomponent.data
      Piece_rope.empty piece_rope_insert piece_rope_delete
  in
  let piece_rust, rust_length =
    Utils.run_txns_time "Rustcode Piece_rope" Rustcode.data Piece_rope.empty
      piece_rope_insert piece_rope_delete
  in
  let piece_seph, seph_length =
    Utils.run_txns_time "Sephblog Piece_rope" Sephblog.data Piece_rope.empty
      piece_rope_insert piece_rope_delete
  in
  let piece_merge, merge_length =
    Utils.run_txns_time "Automerge Piece_rope" Automerge.data Piece_rope.empty
      piece_rope_insert piece_rope_delete
  in

  Printf.printf "\n-\t piece_rope substring \t-";
  let _ =
    Utils.run_substring_time "Svelte Piece_rope sub" piece_svelte svelte_length
      piece_rope_substring
  in
  let _ =
    Utils.run_substring_time "Rustcode Piece_rope sub" piece_rust rust_length
      piece_rope_substring
  in
  let _ =
    Utils.run_substring_time "Sephblog Piece_rope sub" piece_seph seph_length
      piece_rope_substring
  in
  let _ =
    Utils.run_substring_time "Automerge Piece_rope sub" piece_merge merge_length
      piece_rope_substring
  in

  (* Running the datasets on Zed. *)
  Printf.printf "\n-\t zed edit traces \t-";
  let zed_svelte, svelte_length =
    Utils.run_txns_time "Svelte Zed" Sveltecomponent.data (Zed_rope.empty ())
      zed_insert zed_delete
  in
  let zed_rust, rust_length =
    Utils.run_txns_time "Rustcode Zed" Rustcode.data (Zed_rope.empty ())
      zed_insert zed_delete
  in
  let zed_seph, seph_length =
    Utils.run_txns_time "Sephblog Zed" Sephblog.data (Zed_rope.empty ())
      zed_insert zed_delete
  in
  let zed_merge, merge_length =
    Utils.run_txns_time "Automerge Zed" Automerge.data (Zed_rope.empty ())
      zed_insert zed_delete
  in
  Printf.printf "\n-\t zed substring \t-";
  let _ =
    Utils.run_substring_time "Svelte Zed sub" zed_svelte svelte_length
      zed_substring
  in
  let _ =
    Utils.run_substring_time "Rustcode Zed sub" zed_rust rust_length
      zed_substring
  in
  let _ =
    Utils.run_substring_time "Sephblog Zed sub" zed_seph seph_length
      zed_substring
  in
  let _ =
    Utils.run_substring_time "Automerge Zed sub" zed_merge merge_length
      zed_substring
  in

  (* Running the datasets on Bazaar_rope. *)
  Printf.printf "\n-\t bazaar edit traces \t-";
  let bazaar_svelete, svelte_length =
    Utils.run_txns_time "Svelte Bazaar" Sveltecomponent.data Bazaar_rope.S.empty
      bazaar_insert bazaar_delete
  in
  let bazaar_rust, rust_length =
    Utils.run_txns_time "Rustcode Bazaar_rope" Rustcode.data Bazaar_rope.S.empty
      bazaar_insert bazaar_delete
  in
  let bazaar_seph, seph_length =
    Utils.run_txns_time "Sephblog Bazaar_rope" Sephblog.data Bazaar_rope.S.empty
      bazaar_insert bazaar_delete
  in
  let bazaar_merge, merge_length =
    Utils.run_txns_time "Automerge Bazaar_rope" Automerge.data
      Bazaar_rope.S.empty bazaar_insert bazaar_delete
  in
  Printf.printf "\n-\t bazaar substring \t-";
  let _ =
    Utils.run_substring_time "Svelte bazaar sub" bazaar_svelete svelte_length
      bazaar_substring
  in
  let _ =
    Utils.run_substring_time "Rustcode bazaar sub" bazaar_rust rust_length
      bazaar_substring
  in
  let _ =
    Utils.run_substring_time "Sephblog bazaar sub" bazaar_seph seph_length
      bazaar_substring
  in
  let _ =
    Utils.run_substring_time "Automerge bazaar sub" bazaar_merge merge_length
      bazaar_substring
  in
  ()
in
()
