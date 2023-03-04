type t

val empty: t

val add: int -> Codepoint_types.t -> t -> t

val find: int -> t -> Codepoint_types.t

