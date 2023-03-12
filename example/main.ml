open Notty
open Notty_unix

type model = {
  col_num : int;
  line_num : int;
  text : Piece_rope__.Piece_types.piece_rope;
}

let initial_model = { col_num = 0; line_num = 0; text = Piece_rope.empty }

type actions = CaretUp | CaretDown | CaretLeft | CaretRight | TypeChar of char

let dispatch model = function
  | CaretUp -> { model with line_num = model.line_num - 1 }
  | CaretDown -> { model with line_num = model.line_num + 1 }
  | CaretLeft -> { model with col_num = model.col_num - 1 }
  | CaretRight -> { model with col_num = model.col_num + 1 }
  | TypeChar chr ->
      let line_offset = Piece_rope.get_line model.line_num model.text in
      let index = line_offset.utf32_offset + model.col_num in
      let rope = Piece_rope.insert index (String.make 1 chr) model.text in
      { model with col_num = model.col_num + 1; text = rope }

let rec main t model =
  let str = Piece_rope.get_text model.text in
  let text = I.string A.(bg lightred ++ fg black) str in
  Term.image t text;
  Term.cursor t (Some (model.col_num, model.line_num));
  match Term.event t with
  (* Below cases terminate program. *)
  | `Key (`Escape, []) | `Key (`ASCII 'C', [ `Ctrl ]) -> ()
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
  | _ -> main t model

let () = main (Term.create ()) initial_model
