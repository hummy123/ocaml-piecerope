(* Auto-generated from "json_types.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type json_piece = { start : int; length : int }

type json_doc = {
  buffer : string list;
  pieces : json_piece list;
  current : int list;
  undo : int list list;
  redo : int list list;
}
