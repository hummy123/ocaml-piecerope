let () =
  let svelte = Utils.run_txns_piece_rope "Svelete Piece_rope" Sveltecomponent.data in
  let rust = Utils.run_txns_piece_rope "Rustcode Piece_rope" Rustcode.data in
  let seph = Utils.run_txns_piece_rope "Sephblog Piece_rope" Sephblog.data in
  let merge = Utils.run_txns_piece_rope "Automerge Piece_rope" Automerge.data in

  let _ = Utils.run_substring "Svelte Substring" svelte in
  let _ = Utils.run_substring "Rust Substring" rust in
  let _ = Utils.run_substring "Seph Substring" seph in
  let _ = Utils.run_substring "Automerge Substring" merge in
  ()
