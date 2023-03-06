type t = {
  buffer: Piece_buffer.t;
  pieces: Piece_tree.t;
}

let empty = { 
  buffer = Piece_buffer.empty; 
  pieces = Piece_tree.empty; 
}

let insert index (str: string) piecerope =
  let pcStart = Piece_buffer.size piecerope.buffer in
  let open String_processor in
  let (pcLength, pcLines) = char_length_and_line_breaks str pcStart in
  let buffer = Piece_buffer.append str pcLength piecerope.buffer in
  let pieces = Piece_tree.insert_tree index pcStart pcLength pcLines piecerope.pieces in
  { buffer; pieces }

let prepend (str: string) piecerope =
  let pcStart = Piece_buffer.size piecerope.buffer in
  let open String_processor in
  let (pcLength, pcLines) = char_length_and_line_breaks str pcStart in
  let buffer = Piece_buffer.append str pcLength piecerope.buffer in
  let pieces = Piece_tree.prepend pcStart pcLength pcLines piecerope.pieces in
  { buffer; pieces }

let append (str: string) piecerope =
  let pcStart = Piece_buffer.size piecerope.buffer in
  let open String_processor in
  let (pcLength, pcLines) = char_length_and_line_breaks str pcStart in
  let buffer = Piece_buffer.append str pcLength piecerope.buffer in
  let pieces = Piece_tree.append pcStart pcLength pcLines piecerope.pieces in
  { buffer; pieces }

let delete start length piecerope =
  let pieces = Piece_tree.delete_tree start length piecerope.pieces in
  { piecerope with pieces; }

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

