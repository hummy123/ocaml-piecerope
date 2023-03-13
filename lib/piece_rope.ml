open Piece_types

let empty = { buffer = Piece_buffer.empty; pieces = Piece_tree.empty }

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
    { buffer; pieces }
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
    { buffer; pieces }
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
    { buffer; pieces }
  else piecerope

let delete start length piecerope =
  let pieces =
    Piece_tree.delete_tree start length piecerope.pieces piecerope.buffer
  in
  { piecerope with pieces }

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
    Unicode.count_string_stats find_string buffer_length
  in
  let buffer =
    Piece_buffer.append replace_string utf32_length piecerope.buffer
  in
  let ins_node =
    Piece_tree.create_node buffer_length utf8_length utf16_length utf32_length
      line_breaks
  in
  let pieces =
    Piece_tree.find_and_replace find_string utf32_length ins_node piecerope
  in
  { buffer; pieces }

let fold_text = Piece_tree.fold_text
let fold_lines = Piece_tree.fold_lines
let fold_match_indices = Piece_tree.fold_match_indices
let offsets = Piece_tree.offsets
