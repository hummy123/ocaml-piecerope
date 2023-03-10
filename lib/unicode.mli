(**
    The index_offsets type describes the same offset in UTF-8, UTF-16 and UTF-32 encodings.
  *)
type index_offsets = { utf8_pos : int; utf16_pos : int; utf32_pos : int; }

(**
    The encoding type specifies whether something should be UTF-8, UTF-16 or UTF-32. 
    It is used in some internal functions for changing behaviour slightly depending on encoding.
  *)
type encoding = Utf8 | Utf16 | Utf32

(** 
    This function analyses an input string for metadata necessary for tracking Unicode encodings and line break information in the data structure.
   
    Arguments:
    String to be processed.
    Integer representing current length of the Piece_buffer before appending this string.
   
    Returns a tuple containing, in order:
    String length in UTF-16.
    String length in UTF-32 (Unicode codepoints).
    Array of UTF-32 indices representing line breaks.
  *)
val count_string_stats : string -> int -> int * int * int array

(**
    This function extracts a substring from a string using UTF-32 indices, useful as OCaml natively uses UTF-8.
   
    Arguments:
    String to extract from.
    Substring start index in UTF-32.
    Substring length where start + length is a UTF-32 index.
   
    Returns:
    Extracted substring.
  *)
val utf32_sub : string -> int -> int -> string

(**
    This function creates an instance of the index_offsets type.
   
    Arguments:
    Offset in UTF-8.
    Offset in UTF-16.
    Offset in UTF-32.
   
    Returns:
    Instance of index_offsets type.
  *)
val create_offsets : int -> int -> int -> index_offsets

(**
    This function counts to a specified index in the string.
    If a UTF-8 or UTF-16 encoding is provided and the index refers in between a UTF-32 code point so that inserting here would produce invalid Unicode, the return value clips to the start of that code point.

    Arguments:
    String to count in.
    Index to count to.
    Encoding of the index to count to.

    Returns:
    Instance of index_offsets type, representing the indices in UTF-8, UTF-16 and UTF-32.
  *)
val count_to : string -> int -> encoding -> index_offsets

