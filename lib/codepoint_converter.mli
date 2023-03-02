type t = | CR | Control | Extend | Extended_Pictographic | L | LF | LV | LVT | Prepend | Regional_Indicator | SpacingMark | T | V | ZWJ | Any
(** Convert an integer to a Unicode codepoint category. *)
val intToCategory : int -> t