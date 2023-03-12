(* Core structure types. Not commented, except for those intended to be exposed to users. *)

type piece_buffer =
  | BE
  | BT of int * piece_buffer * int * string * int * piece_buffer * int

type node = {
  start : int;
  utf8_length : int;
  utf16_length : int;
  utf32_length : int;
  lines : int array;
}

type metadata = {
  utf8_subtree : int;
  utf16_subtree : int;
  utf32_subtree : int;
  subtree_lines : int;
}

type piece_tree =
  | PE
  | PT of int * piece_tree * metadata * node * metadata * piece_tree

type piece_rope = { buffer : piece_buffer; pieces : piece_tree }
(**
   The piece_rope type implements an efficient data structure for inserting, deleting and retrieving text.
 *)

(* Types exposed externally, explaining why they are being provided. *)

type encoding =
  | Utf8
  | Utf16
  | Utf32
      (**
   The encoding type is used by some functions to let you choose how you want to query an offset.
 *)

type index_offsets = { utf8_pos : int; utf16_pos : int; utf32_pos : int }
(**
   The index_offsets type is returned by some functions to help you query text in different Unicode encodings.
 *)

type line_offset = {
  line : string;
  utf32_offset : int;
  utf16_offset : int;
  utf8_offset : int;
}
(**
   The line_offset type is returned when you query for a line.
   It contains the line string itself and the line's start index in different Unicode encodings.
 *)

type tree_stats = {
  lines : int;
  utf32_length : int;
  utf16_length : int;
  utf8_length : int;
}
(**
   The tree_stats type contains the number of lines and the text length in different encodings for a piece_rope you provide.
 *)
