(** Returns a tuple containing two int arrays, where the first represents char breaks in UAX 29 and the second represents line breaks. *)
val find_char_and_line_breaks :  string -> int -> int array * int array
