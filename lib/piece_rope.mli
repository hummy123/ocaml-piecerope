(** A Piecerope. *)
type t

(** An empty Piecerope. *)
val empty: t

(** Returns the Piecerope's total character length. *)
val total_length: t -> int

(** Returns the total number of lines in a Piecerope. *)
val total_lines: t -> int

(** Retrieves a substring from a Piecerope. *)
val substring: int -> int -> t -> string

(** Retrieves a specific line from a Piecerope. *)
val get_line: int -> t -> string

(** Retrieves all the text in the Piecerope. *)
val get_text: t -> string

(** Inserts a string into a Piecerope. *)
val insert: int -> string -> t -> t

(** Deletes a range of text from a Piecerope. *)
val delete: int -> int -> t -> t

(** Creates a new Piecerope with the specified string. *)
val create: string -> t
