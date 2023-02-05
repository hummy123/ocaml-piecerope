type t = {
  buffer: Piece_buffer.t;
  pieces: Piece_tree.t;
}
let rec find_line_breaks (str: string) strLengthMinus1 pcStart pos acc =
  if pos > strLengthMinus1 then
    List.rev acc |> Array.of_list
  else
    let cur = str.[pos] in
    if cur = '\n' then
      find_line_breaks str strLengthMinus1 pcStart (pos + 1) ((pos + pcStart)::acc)
    else if cur = '\r' then
      let acc = (pos + pcStart)::acc in
      if pos = strLengthMinus1 then
        List.rev acc |> Array.of_list
      else
        let next = str.[pos + 1] in
        if next = '\n' then
          find_line_breaks str strLengthMinus1 pcStart (pos + 2) acc
        else
          find_line_breaks str strLengthMinus1 pcStart (pos + 1) acc
    else
      find_line_breaks str strLengthMinus1 pcStart (pos + 1) acc

let insert index (str: string) piecerope =
  let pcStart = Piece_buffer.size piecerope.buffer in
  let pcLines = find_line_breaks str (String.length str - 1) pcStart 0 [] in
  let buffer = Piece_buffer.append str piecerope.buffer in
  let pieces = Piece_tree.insert_tree index pcStart (String.length str) pcLines piecerope.pieces in
  { buffer; pieces }

let prepend (str: string) piecerope =
  let pcStart = Piece_buffer.size piecerope.buffer in
  let pcLines = find_line_breaks str (String.length str - 1) pcStart 0 [] in
  let buffer = Piece_buffer.append str piecerope.buffer in
  let pieces = Piece_tree.prepend pcStart (String.length str) pcLines piecerope.pieces in
  { buffer; pieces }

let append (str: string) piecerope =
  let pcStart = Piece_buffer.size piecerope.buffer in
  let pcLines = find_line_breaks str (String.length str - 1) pcStart 0 [] in
  let buffer = Piece_buffer.append str piecerope.buffer in
  let pieces = Piece_tree.append pcStart (String.length str) pcLines piecerope.pieces in
  { buffer; pieces }

let delete start length piecerope =
  let pieces = Piece_tree.delete_tree start length piecerope.pieces in
  { piecerope with pieces = pieces; }

let substring start length piecerope =
  Piece_tree.substring start length piecerope.pieces piecerope.buffer

let get_line line piecerope =
  Piece_tree.get_line line piecerope.pieces piecerope.buffer

let empty = { buffer = Piece_buffer.empty; pieces = Piece_tree.empty }

let get_text piecerope = Piece_tree.get_text piecerope.pieces piecerope.buffer

let create str = insert 0 str empty

let total_length piecerope = Piece_tree.total_length piecerope.pieces
 
let total_lines piecerope = Piece_tree.total_lines piecerope.pieces

