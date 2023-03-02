(** A codepoint category. *)
type t = | CR | Control | Extend | L | LF | LV | LVT | Prepend | Regional_Indicator | SpacingMark | T | V | ZWJ | Any

(** Convert an integet to a Unicode codepoint category. *)
val intToCategory : int -> t

