let () = 
  let _ = Utils.run_txns "Svelete" Sveltecomponent.data in
  let _ = Utils.run_txns "Rustcode" Rustcode.data in
  let _ = Utils.run_txns "Sephblog" Sephblog.data in
  let _ = Utils.run_txns "Automerge" Automerge.data in
  ()
