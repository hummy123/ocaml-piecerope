# PieceRope-ocaml

## Introduction

This is a text buffer based on [Ropes](https://en.wikipedia.org/wiki/Rope_(data_structure)) and [Piece Trees](http://e98cuenc.free.fr/wordprocessor/piecetable.html), exploiting several nice properties of the latter.

Here is the list of available features:
- Insert, prepend, append
- Delete
- Substring, get whole text, get a specific line
- Undo, redo
- Serialise and deserialise to/from file for persistent undo and redo
- Find, find and replace
- Rebuilding - the base structure is fast but (try `dune exec bench`!) but this can make the structure as fast as it was when it was first opened.
- Translate between Unicode offsets - UTF-8, UTF-16, UTF-32 (also known as code point offsets).
- Uses UTF-32 offsets for functions as validation so an invalid Unicode string is never created.

You can see the `/example/` directory for usage instructions and a simple example program.

## To do

- Add credits section to readme (bring license files of projects that helped me, etc.).

