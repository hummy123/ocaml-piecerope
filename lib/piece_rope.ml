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
  let (utf16length, utf32length, pcLines) = Unicode.count_string_stats str pcStart in
  let utf8length = String.length str in
  let buffer = Piece_buffer.append str utf32length piecerope.buffer in
  let node = Piece_tree.create_node pcStart utf8length utf16length utf32length pcLines in
  let pieces = Piece_tree.insert_tree index node piecerope.pieces piecerope.buffer in
  { buffer; pieces }

let prepend (str: string) piecerope =
  let pcStart = Piece_buffer.size piecerope.buffer in
  let (utf16length, utf32length, pcLines) = Unicode.count_string_stats str pcStart in
  let utf8length = String.length str in
  let buffer = Piece_buffer.append str utf32length piecerope.buffer in
  let node = Piece_tree.create_node pcStart utf8length utf16length utf32length pcLines in
  let pieces = Piece_tree.prepend node piecerope.pieces in
  { buffer; pieces }

let append (str: string) piecerope =
  let pcStart = Piece_buffer.size piecerope.buffer in
  let (utf16length, utf32length, pcLines) = Unicode.count_string_stats str pcStart in
  let utf8length = String.length str in
  let buffer = Piece_buffer.append str utf32length piecerope.buffer in
  let node = Piece_tree.create_node pcStart utf8length utf16length utf32length pcLines in
  let pieces = Piece_tree.append node piecerope.pieces in
  { buffer; pieces }

let delete start length piecerope =
  let pieces = Piece_tree.delete_tree start length piecerope.pieces piecerope.buffer in
  { piecerope with pieces; }

let substring start length piecerope =
  Piece_tree.substring start length piecerope.pieces piecerope.buffer

let get_line line piecerope =
  Piece_tree.get_line line piecerope.pieces piecerope.buffer

let get_line_and_line_start_index  line piecerope =
  Piece_tree.get_line_and_line_start_index  line piecerope.pieces piecerope.buffer

let get_text piecerope = Piece_tree.get_text piecerope.pieces piecerope.buffer

let create str = insert 0 str empty

let metadata piecerope = Piece_tree.tree_size piecerope.pieces

let find_matches find_string piecerope = 
  Piece_tree.find_matches find_string piecerope.pieces piecerope.buffer

let find_and_replace find_string replace_string piecerope =
  let (utf16_length, utf32_length, line_breaks) = Unicode.count_string_stats find_string 0 in
  let buffer_length = Piece_buffer.size piecerope.buffer in
  let buffer = Piece_buffer.append replace_string utf32_length piecerope.buffer in
  let ins_node = Piece_tree.create_node buffer_length (String.length replace_string) utf16_length utf32_length line_breaks in
  let pieces = Piece_tree.find_and_replace find_string utf32_length ins_node piecerope.pieces buffer in
  { buffer; pieces; }
