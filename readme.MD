# PieceRope-ocaml - ⚠️ WIP

## Introduction

This is a text buffer based on [Ropes](https://en.wikipedia.org/wiki/Rope_(data_structure)) and [Piece Trees](http://e98cuenc.free.fr/wordprocessor/piecetable.html).

It uses a Piece Tree to store pointers to the current text in the same way a [Piece Table](https://en.wikipedia.org/wiki/Piece_table) would. 

The strcture eliminates the unnecessary (when all text is stored in RAM) distinction between the original and add buffer and uses a Rope to represent the buffer (with the Rope's distinction between "leaf" and "concat" nodes eliminated so we have a shorter tree).

The hope is that this strcture will use less memory for undo/redo operations. The latest buffer can be consulted for all versions of a PieceTree before that point so there is never any need to store multiple slightly-different-from-the-previous versions of a string.

The [F# version](https://github.com/hummy123/PieceRope) of this library (which is currently buggy in its core operations) provides a nice JSON serialisation format too for enabling persistent undo and redo which I suspect is possible with Ropes but more memory-inefficient. I hope to implement this in the OCaml version (which is bug-free as far as my 67 tests can tell).

```
  	// Array of strings (instead of a single contiguous string) because too large an object can cause runtime issues.
    "Buffer": [
        "<string placeholder>",
        "<string placeholder>"
      	...
    ],
    // Array of all pieces used in any PieceTree.
    "Pieces": [ 
      { "Start": <int placeholder>, 
       "Length": <int placeholder>, 
       "LineBreaks": [<array of ints representing line breaks in piece>] },
		...
    ],
	// Array of ints representing pointers to pieces. This represents the current PieceTree.
    "Current": [
        <int placeholder>,
      	...
    ],
	/// Undo and redo stacks are represented as a nested array of ints (inner array represents a PieceTree just like the "Current" field and outer array represents all the PieceTrees in that stack).
    "UndoStack": [],
    "RedoStack": []
}
```

## To do

- Handle Unicode code points and grapheme clusters.
- Add docs on data strcture design (declutter the readme) and docs on usage.
- Undo/redo stacks and serialise/deserialise for persistent undo/redo.
- Add credits section to readme (bring license files of projects that helped me, etc.).

