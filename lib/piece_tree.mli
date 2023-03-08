(** A tree for a Pieeerope. *)
type t

(** A node in the Piece_tree. *)
type node

(** Creates a new node that can be inserted into a Piece_tree. *)
val create_node : int -> int -> int -> int -> int array -> node

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

(** Returns a line and the index where that line starts. *)
val get_line_and_line_start_index: int -> t -> Piece_buffer.t -> string * int

(** Returns all text using a Piece_tree and a Piece_buffer. May cause errors if the resulting string is too large for OCaml to handle. *)
val get_text: t -> Piece_buffer.t -> string

(** Deletes or truncates pointers to text in the Piece_buffer from a Piece_tree. *)
val delete_tree: int -> int -> t -> t

(** Inserts pointers to text from the Piece_buffer into a Piece_tree. *)
val insert_tree: int -> int -> int -> int array -> t -> t

(** Inserts pointers to text from the Piece_buffer into the start of a Piece_tree. *)
val prepend: int -> int -> int array -> t -> t

(** Inserts points to text from the Piece_buffer into the end of a Piece_tree. *)
val append: int -> int -> int array -> t -> t

(** Folds over text in order (from start to last). Useful for saving to a file and possibly other situations. *)
val fold_text: t -> Piece_buffer.t -> 'a -> (string -> 'a) -> 'a
