type t = {
  buffer: Piece_buffer.t;
  pieces: Piece_tree.t;
  lookup: (int, Codepoint_types.t) Hashtbl.t;
}

let empty = { 
  buffer = Piece_buffer.empty; 
  pieces = Piece_tree.empty; 
  lookup = Codepoint_values.hashTableGen()
}

let insert index (str: string) piecerope =
  let pcStart = Piece_buffer.size piecerope.buffer in
  let open String_processor in
  let (_, pcLines) = find_char_and_line_breaks str pcStart piecerope.lookup in
  let buffer = Piece_buffer.append str piecerope.buffer in
  let pieces = Piece_tree.insert_tree index pcStart (String.length str) pcLines piecerope.pieces in
  { piecerope with buffer; pieces }

let prepend (str: string) piecerope =
  let pcStart = Piece_buffer.size piecerope.buffer in
  let open String_processor in
  let (_, pcLines) = find_char_and_line_breaks str pcStart piecerope.lookup in
  let buffer = Piece_buffer.append str piecerope.buffer in
  let pieces = Piece_tree.prepend pcStart (String.length str) pcLines piecerope.pieces in
  { piecerope with buffer; pieces }

let append (str: string) piecerope =
  let pcStart = Piece_buffer.size piecerope.buffer in
  let open String_processor in
  let (_, pcLines) = find_char_and_line_breaks str pcStart piecerope.lookup in
  let buffer = Piece_buffer.append str piecerope.buffer in
  let pieces = Piece_tree.append pcStart (String.length str) pcLines piecerope.pieces in
  { piecerope with buffer; pieces }

let delete start length piecerope =
  let pieces = Piece_tree.delete_tree start length piecerope.pieces in
  { piecerope with pieces = pieces; }

let substring start length piecerope =
  Piece_tree.substring start length piecerope.pieces piecerope.buffer

let get_line line piecerope =
  Piece_tree.get_line line piecerope.pieces piecerope.buffer

let get_line_and_line_start_index  line piecerope =
  Piece_tree.get_line_and_line_start_index  line piecerope.pieces piecerope.buffer


let get_text piecerope = Piece_tree.get_text piecerope.pieces piecerope.buffer

let create str = insert 0 str empty

let total_length piecerope = Piece_tree.total_length piecerope.pieces
 
let total_lines piecerope = Piece_tree.total_lines piecerope.pieces

