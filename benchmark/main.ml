let ig _ = ()

let () = 
  let _ = Svelte.run() in
  let _ = Rustcode.run() in
  let _ = Sephblog.run() in
  let _ = Automerge.run() in
  ig ()
