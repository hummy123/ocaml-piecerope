open Notty
open Notty_unix

(* This example follows the MVU/Redux pattern which is helpful for showing the Piece_rope API
   without the view portion getting in the way of explanation. *)

type model = {
  col_num : int;
      (* This is just a UI value to track which column we are at on the screen. *)
  line_num : int;
      (* Another UI value to track the line we're at on the screen. *)
  offset : int;
      (* The index the cursor is at, in UTF-32. However, note that this example only supports ASCII characters due to difficulties with OCaml's Uchar type. *)
  text : Piece_rope.t; (* The piece_rope. *)
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
  | Deserialise
  | Undo
  | Redo
  | Rebuild
  | Save
  | Load

(* The Piece_rope.add_to_history function must be called to add to the undo stack.
   In a real application, you would usually try to detect when the program/user is idle and then call this function.
   In the example, every insert/delete calls this so undo and redo are easier to see.
*)
let add_to_history model =
  let rope = Piece_rope.add_to_history model.text in
  { model with text = rope }

(* This is the main function calling the Piece_rope API. *)
let dispatch model = function
  | CaretUp ->
      (* If the user has pressed up and is already at line number 0, do nothing as this makes no sense. *)
      if model.line_num = 0 then model
      else
        (* Retrieve the previous line, count its UTF-32 length,
           get the maximum columns the line can have and clip the column to one of the edges if it is outside the allowable range,
           and update model with line, column and offset. *)
        let prev_line = model.line_num - 1 in
        let line_offset = Piece_rope.get_line prev_line model.text in
        let line_stats = Piece_rope.count_string_stats line_offset.line in
        let max_col_num = line_stats.utf32_length - 1 in
        let col_num =
          if model.col_num <= max_col_num && model.col_num >= 0 then
            model.col_num
          else if model.col_num > max_col_num then max_col_num
          else 0
        in
        let offset = line_offset.utf32_offset + col_num in
        { model with line_num = prev_line; offset; col_num }
  | CaretDown ->
      (* Check if there is a line below the current one, and if there is,
         get offset, line and column number just as done for CaretUp case.
      *)
      let next_line = model.line_num + 1 in
      let stats = Piece_rope.stats model.text in
      if next_line <= stats.lines then
        let line_offset = Piece_rope.get_line next_line model.text in
        let line_stats = Piece_rope.count_string_stats line_offset.line in
        let max_col_num = line_stats.utf32_length in
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
      (* Doesn't make sense to move left if offset is at zero and we're at start,
         and in the normal case we can just decrement the offset and column number by 1.
         However, there is a not-uncommon case where we're at the start of a line and press left,
         in which case we need to calculate the new line and column numbers as we did for CaretUp.
      *)
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
      (* Same as CaretLeft, except we increment offset and column number.
         The special case is when we're at the end of a line and we want to move to the next one, just like CaretDown. *)
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
      (* This types a character into the piece_rope, incrementing the offset and column number by one.
         Notty (and maybe all terminals since I am not familiar) processes characters one-by-one, even if you paste.
         However, the Piece_rope API used here is for inserting strings.

         The way that you would inserting a string is by calling Piece_rope.insert and then Piece_rope.count_string_stats.
         The string_stats object will give you the UTF-32 length which you will add to the current offset, and
         you will add the number of lines in the string_stats object with the current line number you are on.
         Then you can call Piece_rope.get_line on the new line number and clip the column as done in the above examples.
      *)
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
      (* This erases a character from the piece_rope. It is a no-op if our offset is at the start,
         and in the normal case it called the Piece_rope.delete function and decrements the column number and offset by one.
         There is a special case similar to CaretLeft where we delete a line break and want to move the cursor to the start of the previous one.

         The Piece_rope.delete API is deisgned to work with lengths greater than 1, although Notty only subtracts by 1.
         To delete more than one character, you can consider first getting the substring of the range you want to delete and getting its stats.
         Then you subtract the number of lines from your model's lines and perform a similar computation to the comment in TypeChar.
      *)
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
      (* Inserts a new line into the piece_rope and increments the offset and line number by one. *)
      let rope = Piece_rope.insert model.offset "\n" model.text in
      if model.offset = 0 then model
      else
        {
          col_num = 0;
          line_num = model.line_num + 1;
          text = rope;
          offset = model.offset + 1;
        }
  | Serialise ->
      (* Serialises the current state of the piece_rope to the file path where this example was executed from.
         Together with the deserialise function, this provides persistent undo and redo. *)
      let file_path = "current.json" in
      let result = Piece_rope.serialise file_path model.text in
      if result = true then model else failwith "unexepected serialise error"
  | Deserialise ->
      (* Deserialises the file the string points to and returns it with the model.
         Provides persistent undo and redo together with serialise. *)
      let file_path = "current.json" in
      let rope = Piece_rope.deserialise file_path in
      { text = rope; line_num = 0; col_num = 0; offset = 0 }
  | Undo ->
      (* Undoes the current state and pushes the current state to the redo stack, if the undo stack is not empty.
           The column number and line number are set to 0 for this example, which is usually not what you want.
           Users would expect both of these to go to their previous position, but they are set to 0 as that is always a valid offset
         and we are not keeping track of column/line history in this example.
      *)
      if Piece_rope.can_undo model.text then
        let rope = Piece_rope.undo model.text in
        let col_num = 0 in
        let line_num = 0 in
        { model with text = rope; col_num; line_num }
      else model
  | Redo ->
      (* Pushes the current state to the undo stack and pops and makes current the first item on the redo stack if possible.
         Column and line are both set to 0 for the same reason mentioned in the Undo comment. *)
      if Piece_rope.can_redo model.text then
        let rope = Piece_rope.redo model.text in
        let col_num = 0 in
        let line_num = 0 in
        { model with text = rope; col_num; line_num }
      else model
  | Rebuild ->
      (* The rebuild function recreates the piece_rope from its current state, optimising the structure for speed and memory usage
          and also optimising the states in the undo and redo stacks similarly.
          Can be computationally expensive and recommended to run asynchronously. *)
      let rope = Piece_rope.rebuild model.text in
      { model with text = rope }
  | Save ->
      (* Just saves to a plain filed. *)
      let file_path = "content.txt" in
      let _ = Piece_rope.save file_path model.text in
      model
  | Load ->
      (* Loads from a file. No special processing. *)
      let file_path = "content.txt" in
      let rope = Piece_rope.load file_path in
      { text = rope; offset = 0; line_num = 0; col_num = 0 }

(* The below function just prints various statistics. *)
let get_stats model =
  let stats = Piece_rope.stats model.text in
  let line_offset = Piece_rope.get_line model.line_num model.text in
  let line_stats = Piece_rope.count_string_stats line_offset.line in

  let offset_str =
    Format.sprintf "offset %i of %i" model.offset stats.utf32_length
  in
  let col_str =
    Format.sprintf "col %i of %i cols for current line" model.col_num
      line_stats.utf32_length
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
  let model = add_to_history model in
  (* We have to split the strings on \n and other controls characters
     because Notty, the TUI framework, errors when we try to print those. *)
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
  (* Deserialise. *)
  | `Key (`ASCII 'W', [ `Ctrl ]) ->
      let model = dispatch model Deserialise in
      main t model
  (* Undo/redo. *)
  | `Key (`ASCII 'Z', [ `Ctrl ]) ->
      let model = dispatch model Undo in
      main t model
  | `Key (`ASCII 'Y', [ `Ctrl ]) ->
      let model = dispatch model Redo in
      main t model
  (* Rebuild. *)
  | `Key (`ASCII 'R', [ `Ctrl ]) ->
      let model = dispatch model Rebuild in
      main t model
  (* Load, save. *)
  | `Key (`ASCII 'S', [ `Ctrl ]) ->
      let model = dispatch model Save in
      main t model
  | `Key (`ASCII 'L', [ `Ctrl ]) ->
      let model = dispatch model Load in
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
