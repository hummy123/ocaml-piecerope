(** Returns a tuple containing two int arrays, where the first represents char breaks in UAX 29 and the second represents line breaks. *)
val count_string_stats :  string -> int -> int * int * int array

(** Given an index and a string, clips to the start of the code point at/before the index. *)
val utf32_sub: string -> int -> int -> string

type index_offsets = { utf8_pos: int; utf16_pos: int; utf32_pos: int; }

val create_offsets : int -> int -> int -> index_offsets

type encoding = Utf8 | Utf16 | Utf32

val count_to : string -> int -> encoding -> index_offsets
