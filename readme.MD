# PieceRope-ocaml

## Introduction

This is a text buffer based on [Ropes](https://en.wikipedia.org/wiki/Rope_(data_structure)) and [Piece Trees](http://e98cuenc.free.fr/wordprocessor/piecetable.html), exploiting several nice properties of the latter.

Documentation is available by running `dune build @doc` or reading through the piece_rope.mli file.

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

## Credits

The following list enumerates some of the projects helped me in one way or another.
- [atdgen](https://github.com/ahrefs/atd) and [yojson](https://github.com/ocaml-community/yojson) for help reading and writing JSON.
- [Notty](https://github.com/pqwy/notty) for providing a good and simple framework to display example usage in.
- [crdt-benchmarks](https://github.com/josephg/crdt-benchmarks) which helped provide real world edit traces to profile and improve real-world performance.
- [The F# Standard Library](https://github.com/dotnet/fsharp) which I pulled a function out of and rewrote to work with arrays (`tryFindIndex`/`try_find_index`).
- [A C# Piece Table implementation](https://github.com/veler/Csharp-Piece-Table-Implementation) which I pulled some tests out of.

The licenses for each of these can be found in the third-party folder.

There are also some ideas from various places I found help in, although I can't possibly list them all. Some of them are:
- [Ropey](https://github.com/cessen/ropey) for the idea to index the different Unicode encodings, helpful to translate across programming languages.
- [Proofs from Functional Data Structures](https://isabelle.in.tum.de/library/HOL/HOL-Data_Structures/document.pdf) by Tobias Nipkow for a catalogue to try out different balanced trees and select the best-performing for my case.
- Efficient Sets - A Balancing Act by Stephen Adams for introducing me to Weight Balanced Trees, which are used for the serialisation logic to transform the structure to a form more compact for secondary storage (through the rank function that assigns a unique index to each value in the tree).
- Various internet comments by others such as [this one by Raph Levien about how Piece Tables depend on files not changing through a Git checkout for example](https://news.ycombinator.com/item?id=15383193) - that specific comment helped me commit to implementing an in-memory text buffer and not read from files.
- Of course the articles by developers on [AbiWord](http://e98cuenc.free.fr/wordprocessor/piecetable.html) and [VS Code](https://code.visualstudio.com/blogs/2018/03/23/text-buffer-reimplementation) about Piece Trees which this structure just slightly modifies.

## Contributing

Issues and pull requests are welcome. I have faith in the solidity of the library as it has passed my personal tests (both the unit tests and manual testing through `dune exec example`), but I would like to fix any bugs if anyone encounters them.
