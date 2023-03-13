open Piece_types

val empty : piece_tree
(** The empty Piece_tree. *)

val stats : piece_tree -> tree_stats
(**
    This function returns the tree_stats instance for the given tree.

    Accepts:
    The Piece_tree to retrieve information about.

    Returns:
    The tree_stats for the given tree.
  *)

val create_node : int -> int -> int -> int -> int array -> node
(**
    This function creates a node that can be inserted into the Piece_tree.

    Accepts:
    The length of the Piece_buffer before the corresponding string was appended.
    The length of the string in UTF-8.
    The length of the string in UTF-16.
    The length of the string in UTF-32.
    The int array describing line break offsets in UTF-32.

    Returns:
    A node that can be inserted into the Piece_tree.
  *)

val offsets : int -> piece_rope -> encoding -> index_offsets
(**
    This function, given an index, returns an index_offsets record describing the offset in UTF-8, UTF-16 and UTF-32.
    If the given index is in the middle of a valid UTF-32 offset, the return value clips to the previous valid UTF-32 offset.

    Accepts:
    The offset to find in. Can be UTF-8, UTF-16 or UTF-32.
    The piece_rope to find the offset in.

    Returns:
    The index_offsets record for the given offset and piece_rope.
  *)

val prepend : node -> piece_tree -> piece_tree
(**
    This function inserts the given node to the very start of the Piece_tree.

    Accepts:
    The node to insert.
    The Piece_tree to insert into.

    Returns:
    The Piece_tree with the node prepended.
  *)

val append : node -> piece_tree -> piece_tree
(**
    This function inserts the given node to the very end of the Piece_tree.

    Accepts:
    The node to insert.
    The Piece_tree to insert into.

    Returns:
    The Piece_tree with the node appended.
  *)

val ins_max : node -> piece_tree -> piece_tree
(**
    This function inserts the given node to the very end of the Piece_tree.
    The difference between this and the append function is that it skips an optimisation attempt which is known to be invalid by this function's callers.

    Accepts:
    The node to insert.
    The Piece_tree to insert into.

    Returns:
    The Piece_tree with the node inserted at the end.
  *)

val insert_tree : int -> node -> piece_tree -> piece_buffer -> piece_tree
(*An function inserts the given node to the given Piece_tree at the given UTF-32 index.

    Accepts:
    The index to insert into, in UTF-32.
    The node to insert.
    The Piece_tree to insert into.
    The Piece_buffer corresponding to the Piece_tree.

    Returns:
    The Piece_tree with the node inserted at the given index.
*)

val delete_tree : int -> int -> piece_tree -> piece_buffer -> piece_tree
(**
    This function deletes a range of text from the given Piece_tree, assuming start and length are UTF-32.

    Accepts:
    The start index to delete from, in UTF-32.
    The length to delete, in UTF-32.
    The Piece_tree to delete from.
    The Piece_buffer corresponding to the Piece_tree.

    Returns:
    The Piece_tree with the given range deleted.
  *)

val substring : int -> int -> piece_rope -> string
(**
    This function extracts a substring from the given tree, assuming start and length are UTF-32.

    Accepts:
    The start index to start extracting a substring from, in UTF-32.
    The length to extract, in UTF-32.
    The piece_rope to extract a substring from.

    Returns:
    The extracted substring.
  *)

val get_line : int -> piece_rope -> line_offset
(**
    This function retrieves a given line from a Piece_tree.

    Accepts:
    The line to retrieve.
    The piece_rope to retrieve the line from

    Returns:
    A line_offset instance containing the retrieved line and the offset the line starts at.
  *)

val get_text : piece_rope -> string
(**
    This function returns all the text for the given piece_rope..

    Accepts:
    The piece_rope to retrieve text from.

    Returns:
    String containing all text in the Piece_tree.
  *)

val find_matches : string -> piece_rope -> int array
(**
    This function finds all matches of a given string in the given Piece_tree and returns an int array with the UTF-32 indices the string was found at.
    The array contains the matches are in order, where array.[0] is the first match.
    If no matches are found, returns an empty array.

    Accepts:
    The string to find.
    The piece_rope to find within.

    Returns:
    An int array representing the indices in terms of UTF-32 offsets.
  *)

val find_and_replace : string -> node -> piece_rope -> piece_tree
(**
    This function finds all matches of a given string and replaces them with the given node.

    Accepts:
    The string to find.
    The node to replace the given string with.
    The piece_rope to replace within.

    Returns:
    A piece_tree with all instances of the string replaced.
  *)

val fold_text : piece_rope -> 'a -> ('a -> string -> 'a) -> 'a
(**
    This function executes a function for each string in the Piece_tree in order, threading an accumulator argument between each stage,

    Accepts:
    The Piece_tree to fold over.
    The Piece_buffer corresponding to the Piece_tree.
    The initial state the accumulator starts at.
    The folder function to apply on each string.

    Returns:
    The result of the accumulator after applying the folder on each string.
  *)

val fold_lines : piece_rope -> 'a -> ('a -> line_offset -> 'a) -> 'a
(**
    This function executes a function for each line in the Piece_tree in order, threading an accumulator argument between each stage,

    Accepts:
    The Piece_tree to fold over.
    The Piece_buffer corresponding to the Piece_tree.
    The initial state the accumulator starts at.
    The folder function to apply on each line.

    Returns:
    The result of the accumulator after applying the folder on each line.
  *)

val fold_match_indices : string -> piece_rope -> 'a -> ('a -> int -> 'a) -> 'a
(**
    This function executes a folder function on the UTF-32 indices found to match a given string.
    This is used internally for the find_matches and find_and_replace functions and it may be useful to library consumers for other purposes as well.

    Accepts:
    The Piece_tree to fold over.
    The Piece_buffer corresponding to the Piece_tree.
    The initial state the accumulator starts at.
    The folder function to apply on each matched index.

    Returns:
    The result of the accumulator after applying the folder on each index.
  *)
