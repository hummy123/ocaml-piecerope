(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
(*                                                         *)
(*  Core Piece Tree structure types below.                 *)
(*  They are commented with their implementation details,  *)
(*  explaining how the structure works.                    *)
(*                                                         *)
(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

type piece_buffer =
  | BE
  | BT of int * piece_buffer * int * string * int * piece_buffer * int
(*
    Implementation of the Buffer type in a Piece Table by a Rope,
    except it doesn't distinguish between Concat and Leaf nodes.
    The tree used is an AVL Tree.
*)

type node = {
  start : int;
  utf8_length : int;
  utf16_length : int;
  utf32_length : int;
  lines : int array;
}
(*
    The node type represents the Piece in a Piece Table. 
    Stores lengths in different Unicode encodings to support indexing by different encodings.
 *)

type metadata = {
  utf8_subtree : int;
  utf16_subtree : int;
  utf32_subtree : int;
  subtree_lines : int;
}
(*
    A Piece Tree works by storing metadata information about each node in a Rope-like fashion.
    Each node has text length metadata about the left subtree and the right subtree.
    This Implementation contains subtree line metadata to support line search,
    and UTF-encoding data in place of text length to support indexing by different encodings.
 *)

type piece_tree =
  | PE
  | PT of int * piece_tree * metadata * node * metadata * piece_tree
(*
    The piece_tree type contains pieces (pointers) to a buffer 
    which itself contains string contents.
    The tree is ordered in a way where performing an in-order traversal 
    and reading the string each piece points to reconstructs the expected string.
 *)

type piece_rope = {
  buffer : piece_buffer;
  pieces : piece_tree;
  undo : piece_tree list;
  redo : piece_tree list;
  add_to_history : bool;
}
(*
    The piece_rope type is just a wrapper around a Piece Tree and a Buffer,
    as they must be used together to provide any string contents at all.
 *)

(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
(*                                                                     *)
(* Other types used for retrieving tree stats and passing to functions.*)
(*                                                                     *)
(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

type encoding = Utf8 | Utf16 | Utf32
type index_offsets = { utf8_pos : int; utf16_pos : int; utf32_pos : int }

type line_offset = {
  line : string;
  utf32_offset : int;
  utf16_offset : int;
  utf8_offset : int;
}

type string_stats = {
  utf8_length : int;
  utf16_length : int;
  utf32_length : int;
  line_breaks : int;
}

type tree_stats = {
  lines : int;
  utf32_length : int;
  utf16_length : int;
  utf8_length : int;
}
