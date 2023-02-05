(** A buffer for a PieceRope. *)
type t

(** An empty PieceRope buffer. *)
val empty: t

(** Returns the buffer's total character length. *)
val size: t -> int

(** Appends a string to a buffer. *)
val append: string -> t -> t 

(** Retrieves a substring from a buffer. *)
val substring: int -> int -> t -> string
