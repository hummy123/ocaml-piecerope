(** A tree for a Pieeerope. *)
type t

(** An empty Piece_tree *)
val empty : t

(** The total number of lines in a Piece_tree. Is zero-indexed where first line = 0. *)
val total_lines: t -> int

(** The total number of characters in a Piece_Tree. Is zero-indexed where first character = 0. *)
val total_length: t -> int

(** Returns a substring using a Piece_tree and a Piece_buffer. Is zero-indexed where first character = 0. *)
val substring: int -> int -> t -> Piece_buffer.t -> string

(** Returns a line using a Piece_tree and a Piece_buffer. Is zero-indexed. *)
val get_line: int -> t -> Piece_buffer.t -> string

(** Returns all text using a Piece_tree and a Piece_buffer. May cause errors if the resulting string is too large for OCaml to handle. *)
val get_text: t -> Piece_buffer.t -> string

(** Inserts pointers to text in the Piece_buffer into a Piece_tree. *)
val insert_tree: int -> int -> int -> int array -> t -> t

(** Deletes or truncates pointers to text in the Piece_buffer from a Piece_tree. *)
val delete_tree: int -> int -> t -> t

