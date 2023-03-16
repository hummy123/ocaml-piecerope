open Piece_types

type t = piece_rope

let empty =
  {
    buffer = Piece_buffer.empty;
    pieces = Piece_tree.empty;
    undo = [];
    redo = [];
    add_to_history = true;
  }

(* Repetitive logic to manage undo/redo stack when inserting/deleting. *)
let update_piecerope new_tree new_buffer piecerope =
  let undo =
    if piecerope.add_to_history then piecerope.pieces :: piecerope.undo
    else piecerope.undo
  in
  let redo = [] in
  { buffer = new_buffer; pieces = new_tree; undo; redo; add_to_history = false }

let insert index (str : string) piecerope =
  if str <> "" then
    let buffer_length = Piece_buffer.size piecerope.buffer in
    let utf16length, utf32length, pcLines =
      Unicode.count_string_stats str buffer_length
    in
    let utf8length = String.length str in
    let buffer = Piece_buffer.append str utf32length piecerope.buffer in
    let node =
      Piece_tree.create_node buffer_length utf8length utf16length utf32length
        pcLines
    in
    let pieces =
      Piece_tree.insert_tree index node piecerope.pieces piecerope.buffer
    in
    update_piecerope pieces buffer piecerope
  else piecerope

let prepend (str : string) piecerope =
  if str <> "" then
    let pcStart = Piece_buffer.size piecerope.buffer in
    let utf16length, utf32length, pcLines =
      Unicode.count_string_stats str pcStart
    in
    let utf8length = String.length str in
    let buffer = Piece_buffer.append str utf32length piecerope.buffer in
    let node =
      Piece_tree.create_node pcStart utf8length utf16length utf32length pcLines
    in
    let pieces = Piece_tree.prepend node piecerope.pieces in
    update_piecerope pieces buffer piecerope
  else piecerope

let append (str : string) piecerope =
  if str <> "" then
    let pcStart = Piece_buffer.size piecerope.buffer in
    let utf16length, utf32length, pcLines =
      Unicode.count_string_stats str pcStart
    in
    let utf8length = String.length str in
    let buffer = Piece_buffer.append str utf32length piecerope.buffer in
    let node =
      Piece_tree.create_node pcStart utf8length utf16length utf32length pcLines
    in
    let pieces = Piece_tree.append node piecerope.pieces in
    update_piecerope pieces buffer piecerope
  else piecerope

let delete start length piecerope =
  let pieces =
    Piece_tree.delete_tree start length piecerope.pieces piecerope.buffer
  in
  update_piecerope pieces piecerope.buffer piecerope

let substring start length piecerope =
  Piece_tree.substring start length piecerope

let get_line line piecerope = Piece_tree.get_line line piecerope
let get_text piecerope = Piece_tree.get_text piecerope
let of_string str = append str empty
let stats piecerope = Piece_tree.stats piecerope.pieces

let find_matches find_string piecerope =
  Piece_tree.find_matches find_string piecerope

let find_and_replace find_string replace_string piecerope =
  let buffer_length = Piece_buffer.size piecerope.buffer in
  let utf8_length = String.length replace_string in
  let utf16_length, utf32_length, line_breaks =
    Unicode.count_string_stats replace_string buffer_length
  in
  let buffer =
    Piece_buffer.append replace_string utf32_length piecerope.buffer
  in
  let ins_node =
    Piece_tree.create_node buffer_length utf8_length utf16_length utf32_length
      line_breaks
  in
  let pieces = Piece_tree.find_and_replace find_string ins_node piecerope in
  update_piecerope pieces buffer piecerope

let fold_text = Piece_tree.fold_text
let fold_lines = Piece_tree.fold_lines
let fold_match_indices = Piece_tree.fold_match_indices
let offsets = Piece_tree.offsets
let can_undo piecerope = match piecerope.undo with [] -> false | _ -> true
let can_redo piecerope = match piecerope.redo with [] -> false | _ -> true
let add_to_history piecerope = { piecerope with add_to_history = true }
let rebuild = Piece_builder.rebuild

let count_string_stats str =
  let utf16_length, utf32_length, line_breaks =
    Unicode.count_string_stats str 0
  in
  let utf8_length = String.length str in
  let line_breaks = Array.length line_breaks in
  { utf8_length; utf16_length; utf32_length; line_breaks }

let undo piecerope =
  match piecerope.undo with
  | head :: tail ->
      let pieces = head in
      let undo = tail in
      let redo = piecerope.pieces :: piecerope.redo in
      { piecerope with pieces; undo; redo; add_to_history = true }
  | [] -> piecerope

let redo piecerope =
  match piecerope.redo with
  | head :: tail ->
      let pieces = head in
      let redo = tail in
      let undo = piecerope.pieces :: piecerope.undo in
      { piecerope with pieces; undo; redo; add_to_history = true }
  | [] -> piecerope

let serialise = Piece_serialiser.serialise
let deserialise = Piece_serialiser.deserialise

let save file_path piecerope =
  let tree_stats = Piece_tree.stats piecerope.pieces in
  let out_buffer = Buffer.create tree_stats.utf8_length in
  let _ =
    Piece_tree.fold_text piecerope () (fun _ txt ->
        Buffer.add_string out_buffer txt)
  in
  let oc = open_out file_path in
  let _ = Buffer.output_buffer oc out_buffer in
  let _ = close_out oc in
  true

let load file_path =
  let ch = open_in file_path in
  let str = really_input_string ch (in_channel_length ch) in
  let _ = close_in ch in
  append str empty
