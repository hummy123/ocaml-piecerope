open Piece_types

let rebuild piecerope =
  (* Rebuild current. *)
  let current_text = Piece_tree.get_text piecerope in
  let utf8_length = String.length current_text in
  let utf16_length, utf32_length, line_breaks =
    Unicode.count_string_stats current_text 0
  in
  let buffer =
    ref (Piece_buffer.empty |> Piece_buffer.append current_text utf32_length)
  in
  let current_node =
    Piece_tree.create_node 0 utf8_length utf16_length utf32_length line_breaks
  in
  let current_tree = Piece_tree.empty |> Piece_tree.append current_node in

  let rebuild_stack stack =
    List.map
      (fun input_tree ->
        Piece_tree.fold
          (fun build_tree node ->
            let text =
              Piece_buffer.substring node.start node.utf32_length
                piecerope.buffer
            in
            let utf8_length = String.length text in
            let is_found = Piece_buffer.find_match text !buffer in
            match is_found with
            | Some x ->
                let utf16_length, utf32_length, line_breaks =
                  Unicode.count_string_stats text x
                in
                let node =
                  Piece_tree.create_node x utf8_length utf16_length utf32_length
                    line_breaks
                in
                let build_tree = Piece_tree.append node build_tree in
                build_tree
            | None ->
                let buffer_length = Piece_buffer.size !buffer in
                let _ =
                  buffer := Piece_buffer.append text utf32_length !buffer
                in
                let utf16_length, utf32_length, line_breaks =
                  Unicode.count_string_stats current_text buffer_length
                in
                let node =
                  Piece_tree.create_node buffer_length utf8_length utf16_length
                    utf32_length line_breaks
                in
                let build_tree = Piece_tree.append node build_tree in
                build_tree)
          Piece_tree.empty input_tree)
      stack
  in

  let undo = rebuild_stack piecerope.undo in
  let redo = rebuild_stack piecerope.redo in
  { piecerope with buffer = !buffer; pieces = current_tree; undo; redo }
