(** A buffer for a PieceRope. *)
type t

(** An empty PieceRope buffer. *)
val empty: t

(** Appends a string to a buffer. *)
val append: string -> t -> t 

(** Retrieves a substring from a buffer. *)
val substring: int -> int -> t -> string
