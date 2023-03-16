# PieceRope OCaml - TUI Example

This folder contains a toy text editor you can try out with `dune exec example`.

You are better off reading the main.ml file (easy to read and commented) to understand how to use this library, so this file will contain a user guide on how to use it.

- Type an ASCII character to input it.
- Backspace to delete the previous character.
- Arrow keys to move cursor.
- Press escape or Ctrl-C to exit.
- Press Ctrl-Q to serialise the state to `current.json`, located at the path you ran this program from.
- Press Ctrl-W to deserialise `current.json`, located again at the path this program was run from. 
- Ctrl-Z to undo, Ctrl-Y to redo (try this after deserialising to see persistent undo/redo in action!)
- Ctrl-S to save normally, to `content.txt`.
- Ctrl-L to load normally from `content.txt`.
- Ctrl-R to rebuild the structure from the current state while preserving undo and redo, optimising it for speed and memory.
