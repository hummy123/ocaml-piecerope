open Notty
open Notty_unix

type model = {
  col_num : int;
  line_num : int;
  offset : int;
  text : Piece_rope.t;
}

let initial_model =
  { col_num = 0; line_num = 0; offset = 0; text = Piece_rope.empty }

type actions =
  | CaretUp
  | CaretDown
  | CaretLeft
  | CaretRight
  | TypeChar of char
  | Backspace
  | Enter
  | Serialise

let dispatch model = function
  | CaretUp ->
      if model.line_num = 0 then model
      else
        let prev_line = model.line_num - 1 in
        let line_offset = Piece_rope.get_line prev_line model.text in
        let max_col_num = String.length line_offset.line - 1 in
        let col_num =
          if model.col_num <= max_col_num && model.col_num >= 0 then
            model.col_num
          else if model.col_num > max_col_num then max_col_num
          else 0
        in
        let offset = line_offset.utf32_offset + col_num in
        { model with line_num = prev_line; offset; col_num }
  | CaretDown ->
      let next_line = model.line_num + 1 in
      let stats = Piece_rope.stats model.text in
      let line_offset = Piece_rope.get_line next_line model.text in
      if next_line <= stats.lines then
        let max_col_num = String.length line_offset.line - 1 in
        let col_num =
          if model.col_num <= max_col_num && model.col_num >= 0 then
            model.col_num
          else if model.col_num > max_col_num then max_col_num
          else 0
        in
        let offset = line_offset.utf32_offset + col_num in
        { model with line_num = next_line; offset; col_num }
      else model
  | CaretLeft ->
      if model.offset = 0 then model
      else if model.col_num = 0 then
        let prev_line = Piece_rope.get_line (model.line_num - 1) model.text in
        let offset =
          prev_line.utf32_offset + String.length prev_line.line - 1
        in
        let col_num = String.length prev_line.line - 1 in
        { model with col_num; offset; line_num = model.line_num - 1 }
      else { model with col_num = model.col_num - 1; offset = model.offset - 1 }
  | CaretRight ->
      let stats = Piece_rope.stats model.text in
      let line_offset = Piece_rope.get_line model.line_num model.text in
      let line_length = String.length line_offset.line - 1 in
      if model.offset = stats.utf32_length then model
      else if model.col_num = line_length && model.line_num < stats.lines then
        let next_line_offset =
          Piece_rope.get_line (model.line_num + 1) model.text
        in
        let offset = next_line_offset.utf32_offset in
        { model with line_num = model.line_num + 1; offset; col_num = 0 }
      else { model with col_num = model.col_num + 1; offset = model.offset + 1 }
  | TypeChar chr ->
      let rope =
        Piece_rope.insert model.offset (String.make 1 chr) model.text
      in
      {
        model with
        col_num = model.col_num + 1;
        text = rope;
        offset = model.offset + 1;
      }
  | Backspace ->
      if model.offset = 0 then model
      else if model.col_num > 0 then
        let rope = Piece_rope.delete (model.offset - 1) 1 model.text in
        {
          model with
          col_num = model.col_num - 1;
          text = rope;
          offset = model.offset - 1;
        }
      else
        let offset = model.offset - 1 in
        let rope = Piece_rope.delete offset 1 model.text in
        let line_offset = Piece_rope.get_line (model.line_num - 1) rope in
        let col_num = String.length line_offset.line in
        let line_num = model.line_num - 1 in
        { col_num; line_num; offset; text = rope }
  | Enter ->
      let rope = Piece_rope.insert model.offset "\n" model.text in
      {
        col_num = 0;
        line_num = model.line_num + 1;
        text = rope;
        offset = model.offset + 1;
      }
  | Serialise ->
      let file_path = "current.json" in
      let result = Piece_rope.serialise file_path model.text in
      if result = true then
        model
      else
        failwith "unexepected serialise error"

let get_stats model =
  let stats = Piece_rope.stats model.text in
  let line_offset = Piece_rope.get_line model.line_num model.text in
  let line_length = String.length line_offset.line in

  let offset_str =
    Format.sprintf "offset %i of %i" model.offset stats.utf32_length
  in
  let col_str =
    Format.sprintf "col %i of %i cols for current line" model.col_num
      line_length
  in
  let line_str = Format.sprintf "line %i of %i" model.line_num stats.lines in
  let line_offset_str =
    Format.sprintf "current line start idx: %i" line_offset.utf32_offset
  in

  let newline_i = I.string A.empty "" in
  let offset_i = I.string A.empty offset_str in
  let col_i = I.string A.empty col_str in
  let line_i = I.string A.empty line_str in
  let line_o_i = I.string A.empty line_offset_str in

  I.( <-> ) offset_i col_i |> I.( <-> ) line_i |> I.( <-> ) line_o_i
  |> I.( <-> ) newline_i

let rec main t model =
  let str = Piece_rope.get_text model.text in
  let strList = String.split_on_char '\n' str in
  let text =
    List.fold_left
      (fun acc str ->
        let str = I.string A.empty str in
        I.( <-> ) acc str)
      I.empty strList
  in
  let text = I.( <-> ) text (get_stats model) in
  Term.image t text;
  Term.cursor t (Some (model.col_num, model.line_num));
  match Term.event t with
  (* Below cases terminate program. *)
  | `Key (`Escape, []) | `Key (`ASCII 'C', [ `Ctrl ]) -> ()
  (* Serialise to file. *)
  | `Key (`ASCII 'Q', [ `Ctrl ]) -> 
      let _ = dispatch model Serialise in
      main t model
  (* Cursor movements. *)
  | `Key (`Arrow d, _) ->
      let model =
        match d with
        | `Up -> dispatch model CaretUp
        | `Down -> dispatch model CaretDown
        | `Left -> dispatch model CaretLeft
        | `Right -> dispatch model CaretRight
      in
      main t model
  (* Text input. *)
  | `Key (`ASCII chr, _) ->
      let model = dispatch model (TypeChar chr) in
      main t model
  | `Key (`Enter, _) ->
      let model = dispatch model Enter in
      main t model
  | `Key (`Backspace, _) ->
      let model = dispatch model Backspace in
      main t model
  | _ -> main t model

let () = main (Term.create ()) initial_model
