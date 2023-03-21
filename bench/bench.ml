let () =
  let piece_rope_insert pos ins_str rope = Piece_rope.insert pos ins_str rope in
  let piece_rope_delete pos del_num rope = Piece_rope.delete pos del_num rope in
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
  ()
