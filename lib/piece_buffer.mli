type t
(**
    The Piece_buffer.t type represents the buffer in the Piece Table data structure.
  *)

val size : t -> int
(**
    This function returns the length of the provided Piece_buffer in UTF-32 code points.

    Accepts:
    The Piece_buffer to retrieve the length of.

    Returns:
    int indicating length of the Piece_buffer in UTF-32.
  *)

val empty : t
(**
    The empty Piece_buffer.
  *)

val append : string -> int -> t -> t
(**
    Appends a string to a Piece_buffer.

    Accepts:
    The string to append.
    The Piece_buffer to append to.

    Returns:
    The Piece_buffer with the string appended.
  *)

val substring : int -> int -> t -> string
(**
    Returns a substring from the given Piece_buffer.

    Accepts:
    An integer representing the start index of the substring range in UTF-32.
    An integer representing the length of the substring range in UTF-32.

    Returns:
    The specified substring.
  *)
