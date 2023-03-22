open Piece_types

type t = piece_rope
(**
   The Piece_rope.t type implements an efficient data structure for inserting, deleting and retrieving text.
 *)

exception Out_of_bounds of string 
(**
   The Out_of_bounds exception is raised to indicate out-of-bounds access.
   The string arguments indicates the function that triggered the exception and what the problem was.
 *)

val empty : piece_rope
(** The empty Piece_rope.t. *)

val insert : int -> string -> piece_rope -> piece_rope
(**
    This function inserts a string into a Piece_rope.t at the specified index which is assumed to be in UTF-32.
    If you want to insert using UTF-8 or UTF-16 indices, use the Piece_rope.offsets function to convert the indices to UTF-32 and then use this function.

    Accepts:
    The index to insert into, in UTF-32.
    The string to insert.
    The Piece_rope.t to insert into.

    Returns:
    The Piece_rope.t with the node inserted at the given index.

    Raises Out_of_bounds exception if:
    - Attempting to insert before index 0.
    - Attempting to insert after the UTF-32 length queried by Piece_rope.stats.
  *)

val prepend : string -> piece_rope -> piece_rope
(**
    This function inserts the given node at the very start of the Piece_rope.t.

    Accepts:
    The string to insert.
    The Piece_rope.t to insert into.

    Returns:
    The Piece_rope.t with the string prepended.
  *)

val append : string -> piece_rope -> piece_rope
(**
    This function inserts the given string to the very end of the Piece_rope.t.

    Accepts:
    The string to insert.
    The Piece_rope.t to insert into.

    Returns:
    The Piece_rope.t with the node appended.
  *)

val delete : int -> int -> piece_rope -> piece_rope
(**
    This function deletes a range, specified in UTF-32 indices, of text from the given Piece_tree.
    If you want to delete using UTF-8 or UTF-16 indices, use the Piece_rope.offsets function to convert the indices to UTF-32 and then use this function.

    Accepts:
    The start index to delete from, in UTF-32.
    The length to delete, in UTF-32.
    The Piece_rope.t to delete from.

    Returns:
    The Piece_rope.t with the given range deleted.

    Raises Out_of_bounds exception if:
    - Provided a negative number for the start or length values.
    - Attempting to delete after the UTF-32 length queried by Piece_rope.stats.
  *)

val substring : int -> int -> piece_rope -> string
(**
    This function extracts a substring from the given tree, assuming start and length are UTF-32.
    If you would rather use UTF-8 or UTF-32 indices, use the Piece_rope.offsets function to convert the indices to UTF-32 and then use this function.

    Accepts:
    The start index to start extracting a substring from, in UTF-32.
    The length to extract, in UTF-32.
    The Piece_rope.t to extract a substring from.

    Returns:
    The extracted substring.

    Raises Out_of_bounds exception if:
    - Provided a negative number for the start or length values.
    - Attempting to get a subsring after the UTF-32 length queried by Piece_rope.stats.
  *)

val get_line : int -> piece_rope -> line_offset
(**
    This function retrieves a given line from a Piece_tree.

    Accepts:
    The line to retrieve. This is zero-indexed where line 1 can be accessed with index 0, just like string indices are.
    The Piece_rope.t to retrieve the line from

    Returns:
    A line_offset instance containing the retrieved line and the offset the line starts at.

    Notes:
    This library counts \r, \n and \r\n as distinct line breaks. 

    Raises Out_of_bounds exception if:
    - Attempting to get a line before index 0.
    - Attempting to get a line after the total number of lines queried by Piece_rope.stats.
  *)

val get_text : piece_rope -> string
(**
    This function returns all the text for the given Piece_rope.t.

    Accepts:
    The Piece_rope.t to retrieve text from.

    Returns:
    String containing all text in the Piece_tree.
  *)

val of_string : string -> piece_rope
(**
    This function creates a new Piece_rope.t from a string.

    Accepts:
    The string to create the Piece_rope.t with.

    Returns:
    The newly created Piece_rope.t.
  *)

val stats : piece_rope -> tree_stats
(**
    This function returns statistics for the given Piece_rope.t.
    The statistics returned are:
    - The number of lines in the Piece_rope.t.
    - The length of the Piece_rope.t in UTF-8, UTF-16 and UTF-32.

    Accepts:
    The Piece_tree to retrieve statistics about.

    Returns:
    The tree_stats for the given tree.
  *)

val offsets : int -> piece_rope -> encoding -> index_offsets
(**
    This function, given an index, returns an index_offsets record describing the offset in UTF-8, UTF-16 and UTF-32.
    If the given index is in the middle of a valid UTF-32 offset, the return value clips to the previous valid UTF-32 offset.

    Accepts:
    The offset to find in. Can be UTF-8, UTF-16 or UTF-32.
    The Piece_rope.t to find the offset in.

    Returns:
    The index_offsets record for the given offset and Piece_rope.t.
  *)

val find_matches : string -> piece_rope -> int array
(**
    This function finds all matches of a given string in the given Piece_tree and returns an int array with the UTF-32 indices the string was found at.
    The array contains the matches are in order, where array.[0] is the first match.
    If no matches are found, returns an empty array.

    Accepts:
    The string to find.
    The Piece_rope.t to find within.

    Returns:
    An int array representing the indices in terms of UTF-32 offsets.
  *)

val find_and_replace : string -> string -> piece_rope -> piece_rope
(**
    This function finds all matches of one string and replaces them with another.

    Accepts:
    The string to find.
    The string to replace with.
    The Piece_rope.t to replace within.

    Returns:
    A Piece_rope.t with all instances of the string replaced.
  *)

val can_undo : piece_rope -> bool
(** This function returns true if there is anything in the undo stack of the given piece_rope. 
    It is not necessary as calling undo when the undo stack is empty will result in a no-op. *)

val can_redo : piece_rope -> bool
(** This function returns true if there is anything in the redo stack of the given piece_rope. 
    It is not necessary as calling redo when the redo stack is empty will result in a no-op. *)

val undo : piece_rope -> piece_rope
(** This function adds the current state of the piece_rope to the redo stack and pops the most recent state in the undo stack and makes it current.
    If the undo stack is empty, this will just result in a no-op. *)

val redo : piece_rope -> piece_rope
(** This function adds the current state of the piece_rope to the undo stack and pops the most recent state in the redo stack and makes it current.
    If the redo stack is empty, this will just result in a no-op. *)

val serialise : string -> piece_rope -> bool
(** This function serialises the given piece_rope to a file at the given string.
    This is useful for persisting the undo and redo stacks across sessions of your application. 
    To achieve this functionality, all you need to do is to serialise a piece_rope with this function and then call deserialise with the created file path. *)

val deserialise : string -> piece_rope
(** This function deserialises the file at the file path contained in the given string and returns the corresponding piece_rope, with the undo and redo stacks restored.
    It is paired with the serialise function as they must both be used together for persistent undo and redo. *)

val add_to_history : piece_rope -> piece_rope
(** This function sets a flag for the current state of the piece_rope to be added to the undo stack the next time one of these functions is called@
    - Insert
    - Append
    - Prepend
    - Delete
    - Find and replace
 *)

val count_string_stats : string -> string_stats
(** This function returns a record containing the given string's length in UTF-8, UTF-16 and UTF-32 and the number of line breaks where one line break can be \r \n or \r\n.
 *)

val rebuild : piece_rope -> piece_rope
(** This function rebuilds the given piece_rope at the current state so that it performs as well as it did when it was first opened.
    The undo and redo stacks are preserved and rebuilt as well.
    Note that this function is unnecessary when your only operation is append as that is the optimal case for this data structure. It will spend time executing but give no benefit.
    Recommended to run this method asynchronously as it can be computationally expensive.
 *)

val save : string -> piece_rope -> bool
(** This function just saves the given piece_rope to the file path in the provided string. It does not do any special processing, unlike serialise. *)

val load : string -> piece_rope
(** This function loads the file at the given file path and returns a piece_rope. There is no special processing. *)

val fold_text : piece_rope -> 'a -> ('a -> string -> 'a) -> 'a
(**
    This function executes a given function on every string in the Piece_rope.t..
    It is an extensibility point you can use to implement your own functions, such as writing a file.

    Accepts:
    The Piece_rope.t to fold in.
    The initial state to fold with.
    The function to apply at each stage.

    Returns:
    The result of applying the given function on each string.
  *)

val fold_lines : piece_rope -> 'a -> ('a -> line_offset -> 'a) -> 'a
(**
    This function executes a given function on every line in the Piece_rope.t.
    It is an extensibility point you can use to implement your own functions.

    Accepts:
    The Piece_rope.t to fold in.
    The initial state to fold with.
    The function to apply at each stage.

    Returns:
    The result of applying the given function on each line.
  *)

val fold_match_indices : string -> piece_rope -> 'a -> ('a -> int -> 'a) -> 'a
(**
    This function executes a given function on every UTF-32 index matched with the given string in the Piece_rope.t.
    It is an extensibility point you can use to implement your own functions.
    Internally, the find_matches and find_and_replace functions rely on this.

    Accepts:
    The Piece_rope.t to fold in.
    The initial state to fold with.
    The function to apply at each stage.

    Returns:
    The result of applying the given function on each matched index.
  *)
