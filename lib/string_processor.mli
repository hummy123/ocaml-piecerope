(** Returns a tuple containing two int arrays, where the first represents char breaks in UAX 29 and the second represents line breaks. *)
val char_length_and_line_breaks :  string -> int -> int * int array

(** Given an index and a string, clips to the start of the code point at/before the index. *)
val codepointSub: string -> int -> int -> string
