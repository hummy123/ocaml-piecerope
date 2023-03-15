(* Auto-generated from "json_types.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type json_piece = Json_types_t.json_piece = { start : int; length : int }

type json_doc = Json_types_t.json_doc = {
  buffer : string list;
  pieces : json_piece list;
  current : int list;
  undo : int list list;
  redo : int list list;
}

val write_json_piece : Buffer.t -> json_piece -> unit
(** Output a JSON value of type {!type:json_piece}. *)

val string_of_json_piece : ?len:int -> json_piece -> string
(** Serialize a value of type {!type:json_piece}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_json_piece : Yojson.Safe.lexer_state -> Lexing.lexbuf -> json_piece
(** Input JSON data of type {!type:json_piece}. *)

val json_piece_of_string : string -> json_piece
(** Deserialize JSON data of type {!type:json_piece}. *)

val write_json_doc : Buffer.t -> json_doc -> unit
(** Output a JSON value of type {!type:json_doc}. *)

val string_of_json_doc : ?len:int -> json_doc -> string
(** Serialize a value of type {!type:json_doc}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_json_doc : Yojson.Safe.lexer_state -> Lexing.lexbuf -> json_doc
(** Input JSON data of type {!type:json_doc}. *)

val json_doc_of_string : string -> json_doc
(** Deserialize JSON data of type {!type:json_doc}. *)
