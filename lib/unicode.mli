type index_offsets = { utf8_pos : int; utf16_pos : int; utf32_pos : int }
(**
    The index_offsets type describes the same offset in UTF-8, UTF-16 and UTF-32 encodings.
  *)

type encoding =
  | Utf8
  | Utf16
  | Utf32
      (**
    The encoding type specifies whether something should be UTF-8, UTF-16 or UTF-32. 
    It is used in some internal functions for changing behaviour slightly depending on encoding.
  *)

val utf8_length : char -> int
(**
    This function returns the length of a code point in UTF-8 bytes.
   
    Accepts:
    The first byte in the code point.

    Returns:
    The length of the code point in bytes.
  *)

val count_string_stats : string -> int -> int * int * int array
(** 
    This function analyses an input string for metadata necessary for tracking Unicode encodings and line break information in the data structure.
   
    Accepts:
    String to be processed.
    Integer representing current length of the Piece_buffer before appending this string.
   
    Returns a tuple containing, in order:
    String length in UTF-16.
    String length in UTF-32 (Unicode codepoints).
    Array of UTF-32 indices representing line breaks.
  *)

val utf32_sub : string -> int -> int -> string
(**
    This function extracts a substring from a string using UTF-32 indices, useful as OCaml natively uses UTF-8.
   
    Accepts:
    String to extract from.
    Substring start index in UTF-32.
    Substring length where start + length is a UTF-32 index.
   
    Returns:
    Extracted substring.
  *)

val create_offsets : int -> int -> int -> index_offsets
(**
    This function creates an instance of the index_offsets type.
   
    Accepts:
    Offset in UTF-8.
    Offset in UTF-16.
    Offset in UTF-32.
   
    Returns:
    Instance of index_offsets type.
  *)

val count_to : string -> int -> encoding -> index_offsets
(**
    This function counts to a specified index in the string.
    If a UTF-8 or UTF-16 encoding is provided and the index refers in between a UTF-32 code point so that inserting here would produce invalid Unicode, the return value clips to the start of that code point.

    Accepts:
    String to count in.
    Index to count to.
    Encoding of the index to count to.

    Returns:
    Instance of index_offsets type, representing the indices in UTF-8, UTF-16 and UTF-32.
  *)
