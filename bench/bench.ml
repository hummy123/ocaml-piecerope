let () =
  let svelte, _ = Utils.run_txns "Svelete" Sveltecomponent.data in
  let rust, _ = Utils.run_txns "Rustcode" Rustcode.data in
  let seph, _ = Utils.run_txns "Sephblog" Sephblog.data in
  let merge, _ = Utils.run_txns "Automerge" Automerge.data in

  let _ = Utils.run_substring "Svelte Substring" svelte in
  let _ = Utils.run_substring "Rust Substring" rust in
  let _ = Utils.run_substring "Seph Substring" seph in
  let _ = Utils.run_substring "Automerge Substring" merge in
  ()
