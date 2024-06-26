# PieceRope-ocaml ⚠️
 
## DEPRECATED

This repository is deprecated and exists only for archival purposes now.

There is a less featured but much more performant text editing data structure (here)[https://github.com/hummy123/brolib]. Alternatively, zed is probably OCaml's most popular rope implementation, used by utop and lambda-term.

That data structure is built on the idea of plain, simple Ropes rather than Piece Tables or Piece Trees.

The implementation in this repository has embarrassingly slow query/substring times because it is built on the idea of a Piece Tree.

Briefly, a Piece Table/Tree suffers from fragmentation problems because pieces can be extended only in the case of consecutive insertions. This means the tree contains many piece fragments, in contrast to ropes which suffer no such limitation in extending strings because the whole string itself is available. 

This fragmentation is bad because it means there are more nodes in the tree, so that `n` in the expected `O(log n)` time is substituted for a larger number.

Additionally, a Piece Tree as implemented here (and also as implemented in VS Code and AbiWord) contains longer substring/deletion times, because the tree contains data at internal nodes rather than at leaves like a rope would.

To illustrate this, consider an Piece Tree containing the words "The quick brown fox jumps" where each space represents a different piece, and the root piece represents the string "brown".

The user wants to highlight the "quick" and "fox" pieces. To do this, you get the rightmost piece at the root's left subtree (an O(log n) query), the leftmost piece at the root's right subtree (another O(log n) query) and the string represented by the root piece.

This relatively common operation represents two O(log n) queries from the root node and that's pretty slow.

In contrast, a rope which stores data at the leaves will find the smallest subtree that the whole query can be reached from. It may perform more than one O(log n) queries from that smallest subtree, but that's faster than two O(log n) queries from the root.

The embarrasingly slow query times are the reason for this repository being deprecated and existing only as an archive.

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

## Installation

The easiest way to install this package is `opam install piece_rope`.

## Example usage

You can see the `/example/` directory for usage instructions and a simple example program. 

The example program can be run with `dune exec example/example.exe` and requires you to have installed notty with `opam install notty`.

## Benchmarks

Benchmarks can be run with `dune exec bench/bench.exe`.

From the benchmarks (using the same dataset as [Jumprope-rs](https://github.com/josephg/jumprope-rs#benchmarks)), it seems to be faster than XiRope but slower than the fastest mutable Ropes in Rust.

It seems to be much faster than the Rope in [Zed](https://github.com/ocaml-community/zed) and competitive with the Rope in [Ocaml-bazaar](https://github.com/backtracking/ocaml-bazaar) (half the time this library is faster and half the time it is slower).

|              | Svelte | Rustcode | Sephblog | Automerge |
|--------------|--------|----------|----------|-----------|
| piece_rope   | 16 ms  | 31 ms    | 94 ms    | 218 ms    |
| zed_rope     | 267 ms | 1287 ms  | 665 ms   | 1558 ms   |
| ocaml_bazaar | 13 ms  | 86 ms    | 198 ms   | 122 ms    |

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
