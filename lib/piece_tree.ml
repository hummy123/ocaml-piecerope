open Piece_types

let top_level_cont x = x

let create_metadata utf8 utf16 utf32 lines : metadata =
  {
    utf8_subtree = utf8;
    utf16_subtree = utf16;
    utf32_subtree = utf32;
    subtree_lines = lines;
  }

let emptyMetadata =
  { utf8_subtree = 0; utf16_subtree = 0; utf32_subtree = 0; subtree_lines = 0 }

let ht = function PE -> 0 | PT (h, _, _, _, _, _) -> h

let fold f x t =
  let rec fld x t cont =
    match t with
    | PE -> cont x
    | PT (_, l, _, v, _, r) ->
        fld x l (fun x ->
            let x = f x v in
            fld x r (fun x -> cont x))
  in
  fld x t top_level_cont

let fold_back f x t =
  let rec fld x t cont =
    match t with
    | PE -> cont x
    | PT (_, l, _, v, _, r) ->
        fld x r (fun x ->
            let x = f x v in
            fld x l (fun x -> cont x))
  in
  fld x t top_level_cont

(* Getting narious node data. *)
let utf8_length node =
  match node with PE -> 0 | PT (_, _, _, v, _, _) -> v.utf8_length

let utf16_length node =
  match node with PE -> 0 | PT (_, _, _, v, _, _) -> v.utf16_length

let utf32_length node =
  match node with PE -> 0 | PT (_, _, _, v, _, _) -> v.utf32_length

let n_lines node =
  match node with PE -> 0 | PT (_, _, _, v, _, _) -> Array.length v.lines

let tree_size node =
  match node with
  | PE -> emptyMetadata
  | PT (_, _, lm, v, rm, _) ->
      let utf8 = lm.utf8_subtree + rm.utf8_subtree + v.utf8_length in
      let utf16 = lm.utf16_subtree + rm.utf16_subtree + v.utf16_length in
      let utf32 = lm.utf32_subtree + rm.utf32_subtree + v.utf32_length in
      let lines = lm.subtree_lines + rm.subtree_lines + Array.length v.lines in
      create_metadata utf8 utf16 utf32 lines

let utf8_size_left node =
  match node with PE -> 0 | PT (_, _, lm, _, _, _) -> lm.utf8_subtree

let utf16_size_left node =
  match node with PE -> 0 | PT (_, _, lm, _, _, _) -> lm.utf16_subtree

let utf32_size_left node =
  match node with PE -> 0 | PT (_, _, lm, _, _, _) -> lm.utf32_subtree

let utf8_size_right node =
  match node with PE -> 0 | PT (_, _, _, _, rm, _) -> rm.utf8_subtree

let utf16_size_right node =
  match node with PE -> 0 | PT (_, _, _, _, rm, _) -> rm.utf16_subtree

let utf32_size_right node =
  match node with PE -> 0 | PT (_, _, _, _, rm, _) -> rm.utf32_subtree

let lines_left node =
  match node with PE -> 0 | PT (_, _, lm, _, _, _) -> lm.subtree_lines

let lines_right node =
  match node with PE -> 0 | PT (_, _, _, _, rm, _) -> rm.subtree_lines

let stats tree =
  let metadata = tree_size tree in
  {
    lines = metadata.subtree_lines + 1;
    utf32_length = metadata.utf32_subtree;
    utf16_length = metadata.utf16_subtree;
    utf8_length = metadata.utf8_subtree;
  }

(* Creating and editing node data. *)
let create_node start utf8length utf16length utf32length lines =
  {
    start;
    utf8_length = utf8length;
    utf16_length = utf16length;
    utf32_length = utf32length;
    lines;
  }

(* AVL tree balance functions. *)
let mk l a r =
  let h = (if ht l > ht r then ht l else ht r) + 1 in
  PT (h, l, tree_size l, a, tree_size r, r)

let balL ab x c =
  if ht ab = ht c + 2 then
    match ab with
    | PT (_, a, _, y, _, b) -> (
        if ht a >= ht b then mk a y (mk b x c)
        else
          match b with
          | PT (_, b1, _, bx, _, b2) -> mk (mk a y b1) bx (mk b2 x c)
          | _ -> mk ab x c)
    | _ -> mk ab x c
  else mk ab x c

let balR a x bc =
  if ht bc = ht a + 2 then
    match bc with
    | PT (_, b, _, y, _, c) -> (
        if ht b <= ht c then mk (mk a x b) y c
        else
          match b with
          | PT (_, b1, _, bx, _, b2) -> mk (mk a x b1) bx (mk b2 y c)
          | _ -> mk a x bc)
    | _ -> mk a x bc
  else mk a x bc

let split_max tree =
  let rec split node cont =
    match node with
    | PT (_, l, _, a, _, r) ->
        if r = PE then (l, a) |> cont
        else split r (fun (r', a') -> (balL l a r', a') |> cont)
    | PE -> failwith "unexpected split_max case"
  in
  split tree top_level_cont

(* Logic for handling piece nodes; not using a separate module because more work due to defining interface. *)

(* tryFindIndex function ported from F#: https://github.com/dotnet/fsharp/blob/main/src/FSharp.Core/array.fs#L1558 *)
let try_find_index (predicate : int -> bool) (lines : int array) =
  let len = Array.length lines in
  let rec find n =
    if n >= len then None
    else if predicate (Array.unsafe_get lines n) then Some n
    else find (n + 1)
  in
  find 0

let take_while predicate lines =
  let len = Array.length lines in
  let rec take n =
    if n >= len then
      lines
    else if predicate (Array.unsafe_get lines n) then
      take (n + 1)
    else
      Array.sub lines 0 n
  in
  take 0

let skip_while predicate lines =
  let len = Array.length lines in
  let rec skip n =
    if n >= len then
      [||]
    else if predicate (Array.unsafe_get lines n) then
      skip (n + 1)
    else
      Array.sub lines n (len - n)
  in
  skip 0

let count_lines pc_start offset_difference lines =
  let find_pos = pc_start + offset_difference in
  let lines = skip_while (fun x -> x <= find_pos) lines in
  Array.length lines
 
let split_lines rStart (lines : int array) =
  match try_find_index (fun x -> x >= rStart) lines with
  | Some splitPoint ->
      let arrLeft = Array.sub lines 0 splitPoint in
      let arrRight =
        Array.sub lines splitPoint (Array.length lines - splitPoint)
      in
      (arrLeft, arrRight)
  | None -> (lines, Array.make 0 0)

let text piece buffer =
  Piece_buffer.substring piece.start piece.utf32_length buffer

let text_in_range curIndex start finish piece buffer =
  let textStart = start - curIndex + piece.start in
  let textLength = finish - curIndex + piece.start - textStart in
  Piece_buffer.substring textStart textLength buffer

let text_at_start curIndex finish piece buffer =
  let textLength = finish - curIndex in
  Piece_buffer.substring piece.start textLength buffer

let text_at_end curIndex start piece buffer =
  let textStart = start - curIndex + piece.start in
  let textLength = piece.start + piece.utf32_length - textStart in
  Piece_buffer.substring textStart textLength buffer

let at_start_and_length start length buffer =
  Piece_buffer.substring start length buffer

let is_at_line_break piece_pos_prev piece_pos_next lines =
  let len = Array.length lines in
  let rec find n =
    if n >= len then false
    else
      let cur = Array.unsafe_get lines n in
      if cur = piece_pos_prev || cur = piece_pos_next then true
      else find (n + 1)
  in
  find 0

let delete_at_start curIndex finish piece buffer =
  let difference = finish - curIndex in
  let new_start = piece.start + difference in
  let newLength = piece.utf32_length - difference in
  let newLines = skip_while (fun x -> x < new_start) piece.lines in
  if is_at_line_break new_start piece.lines then
    let txt = at_start_and_length (new_start - 1) 2 buffer in
    if txt = "\r\n" then
      (new_start - 1, newLength - 1, newLines)
    else
      (new_start, newLength, newLines)
  else
    (new_start, newLength, newLines)

let delete_at_end curIndex start piece buffer =
  let length = start - curIndex in
  let length_offset = length + piece.start in
  let lines = skip_while (fun x -> x <= length_offset) piece.lines in
  if is_at_line_break length_offset piece.lines then
    let txt = at_start_and_length (length_offset - 1) 2 buffer in 
    if txt = "\r\n" then
      length - 1, lines
    else
      length, lines
  else
    (length, lines)

let delete_in_range curIndex start finish piece buffer =
  let finish_difference = finish - curIndex in
  let p1_length = start - curIndex in
  let p2_start = finish_difference + piece.start in
  let p1_length_offset = p1_length + piece.start in
  let p1Lines = take_while (fun x -> x < p1_length_offset) piece.lines in
  let p2Lines = skip_while (fun x -> x < p2_start) piece.lines in
  let p2Length = piece.utf32_length - finish_difference in
  let p1_length =
    if is_at_line_break p1_length_offset piece.lines then
      let txt = at_start_and_length (p1_length_offset - 1) 2 buffer in
      if txt = "\r\n" then
        p1_length - 1
      else
        p1_length
    else
      p1_length
  in
  let p2_start =
    if is_at_line_break p2_start piece.lines then
      let txt = at_start_and_length (p2_start - 1) 2 buffer in
      if txt = "\r\n" then
        p2_start + 1
      else
        p2_start
    else p2_start
  in
  (p1_length, p1Lines, p2_start, p2Length, p2Lines)

(* Core PieceTree logic. *)
let is_consecutive v pcStart = v.start + v.utf32_length = pcStart

(* Indexing operations for finding offsets in other encodings. *)
let offsets_from_ut32 find_offset rope : index_offsets =
  let rec off cur_u8 cur_u16 cur_u32 (cur_line : int) (node : piece_tree) =
    match node with
    | PT (_, l, _, v, _, r) ->
        let u32_node_end = cur_u32 + v.utf32_length in
        if find_offset < cur_u32 then
          let next_u8 = cur_u8 - utf8_length l - utf8_size_right l in
          let next_u16 = cur_u16 - utf16_length l - utf16_size_right l in
          let next_u32 = cur_u32 - utf32_length l - utf32_size_right l in
          let next_line = cur_line - n_lines l - lines_right l in
          off next_u8 next_u16 next_u32 next_line l
        else if find_offset > u32_node_end then
          let next_u8 = cur_u8 + v.utf8_length + utf8_size_left r in
          let next_u16 = cur_u16 + v.utf16_length + utf16_size_left r in
          let next_u32 = u32_node_end + utf32_size_left r in
          let next_line = cur_line + Array.length v.lines + lines_left r in
          off next_u8 next_u16 next_u32 next_line r
          (* Trying to find start of this node. *)
        else if find_offset = cur_u32 then
          Unicode.create_offsets cur_u8 cur_u16 cur_u32 cur_line
          (* Trying to find end of this node. *)
        else if find_offset = u32_node_end then
          Unicode.create_offsets (cur_u8 + v.utf8_length)
            (cur_u16 + v.utf16_length) u32_node_end
            (cur_line + Array.length v.lines)
          (* Trying to find middle of this node
             while neither this node or any node we ttavelled to before contain any non-ASCII text. *)
        else if cur_u8 = cur_u32 && v.utf8_length = v.utf32_length then
          let line_num = count_lines v.start (find_offset - cur_u32) v.lines in
          Unicode.create_offsets find_offset find_offset find_offset line_num
          (* Trying to find middle of this node, and this node contains no non-ASCII chars. *)
        else if v.utf8_length = v.utf32_length then
          let u32_difference = find_offset - cur_u32 in
          let u8_offset = cur_u8 + u32_difference in
          let u16_offset = cur_u16 + u32_difference in
          let line_num = count_lines v.start u32_difference v.lines in
          Unicode.create_offsets u8_offset u16_offset find_offset line_num
          (* Trying to find middle of this node. We must get this node's text and count offsets from the string. *)
        else
          let u32_difference = find_offset - cur_u32 in
          let nodeText = text v rope.buffer in
          let textOffsets = Unicode.count_to nodeText u32_difference Utf32 in
          let line_num = count_lines v.start u32_difference v.lines in
          Unicode.create_offsets
            (textOffsets.utf8_pos + cur_u8)
            (textOffsets.utf16_pos + cur_u16)
            find_offset line_num
    | PE ->
        if rope.pieces = PE then Unicode.create_offsets 0 0 0 0
        else failwith "impossible offsets_from_ut32 case"
  in
  off
    (utf8_size_left rope.pieces)
    (utf16_size_left rope.pieces)
    (utf32_size_left rope.pieces)
    (lines_left rope.pieces) rope.pieces

let offsets_from_ut16 find_offset rope =
  let rec off cur_u8 cur_u16 cur_u32 (cur_line : int) node =
    match node with
    | PT (_, l, _, v, _, r) ->
        let u16_node_end = cur_u16 + v.utf16_length in
        if find_offset < cur_u16 then
          let next_u8 = cur_u8 - utf8_length l - utf8_size_right l in
          let next_u16 = cur_u16 - utf16_length l - utf16_size_right l in
          let next_u32 = cur_u32 - utf32_length l - utf32_size_right l in
          let next_line = cur_line - n_lines l - lines_right l in
          off next_u8 next_u16 next_u32 next_line l
        else if find_offset > u16_node_end then
          let next_u8 = cur_u8 + v.utf8_length + utf8_size_left r in
          let next_u16 = u16_node_end + utf16_size_left r in
          let next_u32 = cur_u32 + v.utf32_length + utf32_size_left r in
          let next_line = cur_line + Array.length v.lines + lines_left r in
          off next_u8 next_u16 next_u32 next_line r
          (* Trying to find start of this node. *)
        else if find_offset = cur_u16 then
          Unicode.create_offsets cur_u8 cur_u16 cur_u32 cur_line
          (* Trying to find end of this node. *)
        else if find_offset = u16_node_end then
          Unicode.create_offsets (cur_u8 + v.utf8_length) u16_node_end
            (cur_u32 + v.utf32_length)
            (cur_line + Array.length v.lines)
          (* Trying to find middle of this node
             while neither this node or any node we ttavelled to before contain any non-ASCII text. *)
        else if cur_u8 = cur_u32 && v.utf8_length = v.utf32_length then
          let line_num = count_lines v.start (find_offset - cur_u32) v.lines in
          Unicode.create_offsets find_offset find_offset find_offset line_num
          (* Trying to find middle of this node, and this node contains no non-ASCII chars. *)
        else if v.utf8_length = v.utf32_length then
          let u16_difference = find_offset - cur_u16 in
          let u8_offset = cur_u8 + u16_difference in
          let u32_offset = cur_u32 + u16_difference in
          let line_num = count_lines v.start u32_offset v.lines in
          Unicode.create_offsets u8_offset find_offset u32_offset line_num
          (* Trying to find middle of this node. We must get this node's text and count offsets from the string. *)
        else
          let u16_difference = find_offset - cur_u16 in
          let nodeText = text v rope.buffer in
          let textOffsets = Unicode.count_to nodeText u16_difference Utf16 in
          Unicode.create_offsets
            (textOffsets.utf8_pos + cur_u8)
            (textOffsets.utf16_pos + cur_u16)
            (textOffsets.utf32_pos + cur_u32)
            (cur_line + textOffsets.line_num)
    | PE ->
        if rope.pieces = PE then Unicode.create_offsets 0 0 0 0
        else failwith "impossible offsets_from_ut32 case"
  in
  off
    (utf8_size_left rope.pieces)
    (utf16_size_left rope.pieces)
    (utf32_size_left rope.pieces)
    (lines_left rope.pieces) rope.pieces

let offsets_from_ut8 find_offset rope =
  let rec off cur_u8 cur_u16 cur_u32 (cur_line : int) (node : piece_tree) =
    match node with
    | PT (_, l, _, v, _, r) ->
        let u8_node_end = cur_u8 + v.utf8_length in
        if find_offset < cur_u8 then
          let next_u8 = cur_u8 - utf8_length l - utf8_size_right l in
          let next_u16 = cur_u16 - utf16_length l - utf16_size_right l in
          let next_u32 = cur_u32 - utf32_length l - utf32_size_right l in
          let next_line = cur_line - n_lines l - lines_right l in
          off next_u8 next_u16 next_u32 next_line l
        else if find_offset > u8_node_end then
          let next_u8 = u8_node_end + utf8_size_left r in
          let next_u16 = cur_u16 + v.utf16_length + utf16_size_left r in
          let next_u32 = cur_u32 + v.utf32_length + utf32_size_left r in
          let next_line = cur_line + Array.length v.lines + lines_left r in
          off next_u8 next_u16 next_u32 next_line r
          (* Trying to find start of this node. *)
        else if find_offset = cur_u8 then
          Unicode.create_offsets cur_u8 cur_u16 cur_u32 cur_line
          (* Trying to find end of this node. *)
        else if find_offset = u8_node_end then
          Unicode.create_offsets u8_node_end (cur_u16 + v.utf16_length)
            (cur_u32 + v.utf32_length)
            (cur_line + Array.length v.lines)
          (* Trying to find middle of this node
             while neither this node or any node we ttavelled to before contain any non-ASCII text. *)
        else if cur_u8 = cur_u32 && v.utf8_length = v.utf32_length then
          let line_num = count_lines v.start (find_offset - cur_u32) v.lines in
          Unicode.create_offsets find_offset find_offset find_offset line_num
          (* Trying to find middle of this node, while this node contains no non-ASCII chars. *)
        else if v.utf8_length = v.utf32_length then
          let u8_difference = find_offset - cur_u8 in
          let u16_offset = cur_u16 + u8_difference in
          let u32_offset = cur_u32 + u8_difference in
          let line_num = count_lines v.start u32_offset v.lines in
          Unicode.create_offsets find_offset u16_offset u32_offset line_num
          (* Trying to find middle of this node. We must get this node's text and count offsets from the string. *)
        else
          let u16_difference = find_offset - cur_u16 in
          let nodeText = text v rope.buffer in
          let textOffsets = Unicode.count_to nodeText u16_difference Utf8 in
          Unicode.create_offsets
            (textOffsets.utf8_pos + cur_u8)
            (textOffsets.utf16_pos + cur_u16)
            (textOffsets.utf32_pos + cur_u32)
            (textOffsets.line_num + cur_line)
    | PE ->
        if rope.pieces = PE then Unicode.create_offsets 0 0 0 0
        else failwith "impossible offsets_from_ut32 case"
  in
  off
    (utf8_size_left rope.pieces)
    (utf16_size_left rope.pieces)
    (utf32_size_left rope.pieces)
    (lines_left rope.pieces) rope.pieces

let offsets find_offset rope enc : index_offsets =
  match enc with
  | Utf8 -> offsets_from_ut8 find_offset rope
  | Utf16 -> offsets_from_ut16 find_offset rope
  | Utf32 -> offsets_from_ut32 find_offset rope

(** Adds the given piece to the start of the given tree. *)
let prepend insNode tree =
  let rec pre node cont =
    match node with
    | PE -> mk PE insNode PE |> cont
    | PT (_, l, _, v, _, r) -> pre l (fun l' -> balL l' v r |> cont)
  in
  pre tree top_level_cont

(** Adds the given piece to the end of the given tree. 
    If the previous insert on the tree was done at the very end,
    we merge with the last piece rather than creating a new node to keep the tree more shallow. *)
let append insNode tree =
  let rec app node cont =
    match node with
    | PE -> mk PE insNode PE |> cont
    | PT (_, l, _, v, _, PE) when is_consecutive v insNode.start ->
        let v'Lines = Array.append v.lines insNode.lines in
        let v' =
          {
            v with
            lines = v'Lines;
            utf8_length = v.utf8_length + insNode.utf8_length;
            utf16_length = v.utf16_length + insNode.utf16_length;
            utf32_length = v.utf32_length + insNode.utf32_length;
          }
        in
        mk l v' PE |> cont
    | PT (_, l, _, v, _, r) -> app r (fun r' -> balR l v r' |> cont)
  in
  app tree top_level_cont

(** Same as above append functino but omits the is_consecutive check. 
    Used when we split a piece and is not meant to be a public function. *)
let ins_max insNode tree =
  let rec max node cont =
    match node with
    | PE -> mk PE insNode PE |> cont
    | PT (_, l, _, v, _, r) -> max r (fun r' -> balR l v r' |> cont)
  in
  max tree top_level_cont

let insert_tree insIndex insNode tree buffer =
  let rec ins curIndex node cont =
    match node with
    | PE -> mk PE insNode PE |> cont
    | PT (_, l, _, v, _, r) when insIndex < curIndex ->
        let nextIndex = curIndex - utf32_length l - utf32_size_right l in
        ins nextIndex l (fun l' -> balL l' v r |> cont)
    | PT (_, l, _, v, _, r) when insIndex > curIndex + v.utf32_length ->
        let nextIndex = curIndex + v.utf32_length + utf32_size_left r in
        ins nextIndex r (fun r' -> balR l v r' |> cont)
    | PT (_, l, _, v, _, r) when insIndex = curIndex ->
        let l' = ins_max insNode l in
        balL l' v r |> cont
    | PT (_, l, _, v, _, r)
      when insIndex = curIndex + v.utf32_length
           && is_consecutive v insNode.start ->
        let v'Lines = Array.append v.lines insNode.lines in
        let v' =
          {
            v with
            lines = v'Lines;
            utf8_length = v.utf8_length + insNode.utf8_length;
            utf16_length = v.utf16_length + insNode.utf16_length;
            utf32_length = v.utf32_length + insNode.utf32_length;
          }
        in
        mk l v' r |> cont
    | PT (_, l, _, v, _, r) when insIndex = curIndex + v.utf32_length ->
        let r' = prepend insNode r in
        balR l v r' |> cont
    | PT (_, l, _, v, _, r) ->
        (* UTF-32 offset calculations which are same in both if/else cases. *)
        let difference_u32 = insIndex - curIndex in
        let rStart = v.start + difference_u32 in
        let rLength_u32 = v.utf32_length - difference_u32 in
        let leftLines, rightLines = split_lines rStart v.lines in

        (* Requires no special handling as this node only contains ASCII. *)
        if v.utf32_length = v.utf8_length then
          let l'node =
            create_node v.start difference_u32 difference_u32 difference_u32
              leftLines
          in
          let l' = ins_max l'node l in
          let r'node =
            create_node rStart rLength_u32 rLength_u32 rLength_u32 rightLines
          in
          let r' = prepend r'node r in
          mk l' insNode r' |> cont
          (* Get string so we can translate utf-32 offset to utf-8/16. *)
        else
          let nodeText = text v buffer in
          let offsets = Unicode.count_to nodeText difference_u32 Utf32 in
          let difference_u8 = offsets.utf8_pos in
          let difference_u16 = offsets.utf16_pos in
          let rLength_u8 = v.start + difference_u8 in
          let rLength_u16 = v.start + difference_u16 in
          let l'node =
            create_node v.start difference_u8 difference_u16 difference_u32
              leftLines
          in
          let l' = ins_max l'node l in
          let r'node =
            create_node rStart rLength_u8 rLength_u16 rLength_u32 rightLines
          in
          let r' = prepend r'node r in
          mk l' insNode r' |> cont
  in
  ins (utf32_size_left tree) tree top_level_cont

(* Repeated if-statements used in both delete and substring. *)
let in_range start curIndex finish nodeEndIndex =
  start <= curIndex && finish >= nodeEndIndex

let start_is_in_range start curIndex finish nodeEndIndex =
  start <= curIndex && finish < nodeEndIndex && curIndex < finish

let end_is_in_range start curIndex finish nodeEndIndex =
  start > curIndex && finish >= nodeEndIndex && start <= nodeEndIndex

let middle_is_in_range start curIndex finish nodeEndIndex =
  start >= curIndex && finish <= nodeEndIndex

let delete_tree start length tree buffer =
  let finish = start + length in
  let rec del curIndex node cont =
    match node with
    | PE -> PE |> cont
    | PT (_, l, _, v, _, r)
      when in_range start curIndex finish (curIndex + v.utf32_length) ->
        let recurseLeftIndex = curIndex - utf32_length l - utf32_size_right l in
        let recurseRightIndex = curIndex + v.utf32_length + utf32_size_left r in

        del recurseLeftIndex l (fun l' ->
            del recurseRightIndex r (fun r' ->
                if l' = PE then r' |> cont
                else
                  let newLeft, newVal = split_max l' in
                  balR newLeft newVal r' |> cont))
    | PT (_, l, _, v, _, r)
      when start_is_in_range start curIndex finish (curIndex + v.utf32_length)
      ->
        let recurseLeftIndex = curIndex - utf32_length l - utf32_size_right l in
        del recurseLeftIndex l (fun l' ->
            let newStart, newLength, newLines =
              delete_at_start curIndex finish v buffer
            in
           if v.utf8_length = v.utf32_length then
              let v' =
                create_node newStart newLength newLength newLength newLines
              in
              balR l' v' r |> cont
            else
              let nodeText = at_start_and_length newStart newLength buffer in
              let offsets = Unicode.count_to nodeText newLength Utf32 in
              let v' =
                create_node newStart offsets.utf8_pos offsets.utf16_pos
                  newLength newLines
              in
              balR l' v' r)
    | PT (_, l, _, v, _, r)
      when end_is_in_range start curIndex finish (curIndex + v.utf32_length) ->
        let recurseRightIndex = curIndex + v.utf32_length + utf32_size_left r in
        del recurseRightIndex r (fun r' ->
            let length, lines = delete_at_end curIndex start v buffer in
            if v.utf32_length = v.utf8_length then
              let v' = create_node v.start length length length lines in
              balL l v' r' |> cont
            else
              let nodeText = at_start_and_length v.start length buffer in
              let offsets = Unicode.count_to nodeText length Utf32 in
              let v' =
                create_node v.start offsets.utf8_pos offsets.utf16_pos length
                  lines
              in
              balL l v' r' |> cont)
    | PT (_, l, _, v, _, r)
      when middle_is_in_range start curIndex finish (curIndex + v.utf32_length)
      ->
        let p1Length, p1Lines, p2Start, p2Length, p2Lines =
          delete_in_range curIndex start finish v buffer
        in
        if v.utf32_length = v.utf8_length then
          let r'node = create_node p2Start p2Length p2Length p2Length p2Lines in
          let r' = prepend r'node r in
          let v' = create_node v.start p1Length p1Length p1Length p1Lines in
          balR l v' r' |> cont
        else
          let p1_text = at_start_and_length v.start p1Length buffer in
          let p2_text = at_start_and_length p2Start p2Length buffer in
          let p1_offset = Unicode.count_to p1_text p1Length Utf32 in
          let p2_offset = Unicode.count_to p2_text p2Length Utf32 in
          let r'node =
            create_node p2Start p2_offset.utf8_pos p2_offset.utf16_pos p2Length
              p2Lines
          in
          let v' =
            create_node v.start p1_offset.utf8_pos p1_offset.utf16_pos p1Length
              p1Lines
          in
          let r' = prepend r'node r in
          balR l v' r' |> cont
    | PT (_, l, _, v, _, r) when start < curIndex ->
        let recurseLeftIndex = curIndex - utf32_length l - utf32_size_right l in
        del recurseLeftIndex l (fun l' -> balR l' v r |> cont)
    | PT (_, l, _, v, _, r) when finish > curIndex + v.utf32_length ->
        let recurseRightIndex = curIndex + v.utf32_length + utf32_size_left r in
        del recurseRightIndex r (fun r' -> balL l v r' |> cont)
    | PT (_, _, _, _, _, _) -> failwith "impossible PieceTree.delete case"
  in
  del (utf32_size_left tree) tree top_level_cont

let substring start length rope =
  let finish = start + length in
  let rec sub curIndex node acc cont =
    match node with
    | PE -> acc |> cont
    (* Cases when the current node is at least partially in the substring range. *)
    (* The end of the current substring range is in this node, |whi|ch means the start of this node. *)
    | PT (_, _, _, v, _, r)
      when end_is_in_range start curIndex finish (curIndex + v.utf32_length) ->
        let nodeText = text_at_end curIndex start v rope.buffer in
        let recurseRightIndex = curIndex + v.utf32_length + utf32_size_left r in
        sub recurseRightIndex r acc (fun x -> nodeText :: x |> cont)
    (* The currennt node is |fully| within the substring range. *)
    | PT (_, l, _, v, _, r)
      when in_range start curIndex finish (curIndex + v.utf32_length) ->
        let nodeEndIndex = curIndex + v.utf32_length in
        let nodeText = text v rope.buffer in

        let recurseRightIndex = nodeEndIndex + utf32_size_left r in
        let recurseLeftIndex = curIndex - utf32_length l - utf32_size_right l in

        sub recurseRightIndex r acc (fun right ->
            sub recurseLeftIndex l (nodeText :: right) (fun x -> x |> cont))
    (* The start of the substring range is in the node, whi|ch| means the end of this node. *)
    | PT (_, l, _, v, _, _)
      when start_is_in_range start curIndex finish (curIndex + v.utf32_length)
      ->
        let nodeText = text_at_start curIndex finish v rope.buffer in
        let recurseLeftIndex = curIndex - utf32_length l - utf32_size_right l in
        sub recurseLeftIndex l (nodeText :: acc) (fun x -> x |> cont)
    (* The mi|d|dle of the current node is in the substring range. *)
    | PT (_, _, _, v, _, _)
      when middle_is_in_range start curIndex finish (curIndex + v.utf32_length)
      ->
        [ text_in_range curIndex start finish v rope.buffer ] |> cont
    (* Below two cases navigate to the next node when the substring range is outside the current node. *)
    (* When the current node is after the substring's end range. *)
    | PT (_, l, _, _, _, _) when start < curIndex ->
        let recurseLeftIndex = curIndex - utf32_length l - utf32_size_right l in
        sub recurseLeftIndex l acc (fun x -> x |> cont)
    (* When the current node is before the substring's start range. *)
    | PT (_, _, _, v, _, r) when finish > curIndex + v.utf32_length ->
        let recurseRightIndex = curIndex + v.utf32_length + utf32_size_left r in
        sub recurseRightIndex r acc (fun x -> x |> cont)
    | PT (_, _, _, _, _, _) -> failwith "unreachable Buffer.substring case"
  in
  String.concat ""
    (sub (utf32_size_left rope.pieces) rope.pieces [] top_level_cont)

(* Delete/substring if-statements adapted to work with lines. *)
let node_is_in_line nodeStartLine searchLine nodeEndLine =
  nodeStartLine = searchLine && searchLine = nodeEndLine

let end_of_line_is_in_node nodeStartLine searchLine nodeEndLine =
  nodeStartLine = searchLine && searchLine < nodeEndLine

let start_of_line_in_node nodeStartLine searchLine nodeEndLine =
  nodeStartLine < searchLine && searchLine = nodeEndLine

let line_is_in_node nodeStartLine searchLine nodeEndLine =
  nodeStartLine < searchLine && nodeEndLine > searchLine

(*
    We retrieve an additional character when getting a line to see if it ends with \r\n
    and return it unmodified if it does.
    However, we need to substring (chop off the last character) if the line break is \r or \n
    to account for the additional character.
*)
let chop_last_char_if_not_crln str =
  if String.length str = 1 then str
  else
    let last_char = String.unsafe_get str (String.length str - 1) in
    let second_last_char = String.unsafe_get str (String.length str - 2) in
    if second_last_char = '\r' && last_char = '\n' then str
    else String.sub str 0 (String.length str - 1)

let chop_first_char_if_ln str =
  if String.length str = 1 then str
  else
    let first_char = String.unsafe_get str 0 in
    if first_char = '\n' then String.sub str 1 (String.length str - 1) else str

let get_line line rope =
  let rec get cur_line cur_u32 node acc cont =
    match node with
    | PE -> (acc, None) |> cont
    | PT (_, _, _, v, _, r)
      when start_of_line_in_node cur_line line (cur_line + Array.length v.lines)
      ->
        (* Start of line in terms of piece offset. *)
        let lineStart =
          Array.unsafe_get v.lines (Array.length v.lines - 1) + 1
        in
        let length = v.utf32_length - lineStart + v.start in
        let nodeText = at_start_and_length lineStart length rope.buffer in

        (* Index where line starts in terms of piece tree (not buffer). *)
        let lineStartIndex = Some (cur_u32 + v.utf32_length - length) in

        let recurseRightLine = cur_line + Array.length v.lines + lines_left r in
        let recurseRightIndex = cur_u32 + v.utf32_length + utf32_size_left r in

        get recurseRightLine recurseRightIndex r acc (fun (acc, _) ->
            (nodeText :: acc, lineStartIndex) |> cont)
    | PT (_, l, _, v, _, r)
      when node_is_in_line cur_line line (cur_line + Array.length v.lines) ->
        let nodeEndLine = cur_line + Array.length v.lines in
        let nodeText = text v rope.buffer in

        let recurseRightLine = nodeEndLine + lines_left r in
        let recurseLeftLine = cur_line - n_lines l - lines_right l in

        let recurseRightIndex = cur_u32 + v.utf32_length + utf32_size_left r in
        let recurseLeftIndex = cur_u32 - utf32_length l - utf32_size_right l in

        get recurseRightLine recurseRightIndex r acc (fun (acc, _) ->
            get recurseLeftLine recurseLeftIndex l (nodeText :: acc)
              (fun (acc, lidx) ->
                match lidx with
                | Some _ -> (acc, lidx) |> cont
                | None -> (acc, Some cur_u32) |> cont))
    | PT (_, l, _, v, _, _)
      when end_of_line_is_in_node cur_line line (cur_line + Array.length v.lines)
      ->
        (* + 2 in length gives us \r\n in string and - v.Start takes us to piece offset *)
        let length : int = Array.unsafe_get v.lines 0 + 2 - v.start in
        let nodeText =
          at_start_and_length v.start length rope.buffer
          |> chop_last_char_if_not_crln
        in

        let recurseLeftLine = cur_line - n_lines l - lines_right l in
        let recurseLeftIndex = cur_u32 - utf32_length l - utf32_size_right l in

        get recurseLeftLine recurseLeftIndex l (nodeText :: acc)
          (fun (acc, lidx) ->
            match lidx with
            | Some _ -> (acc, lidx) |> cont
            | None -> (acc, Some cur_u32) |> cont)
    | PT (_, _, _, v, _, _)
      when line_is_in_node cur_line line (cur_line + Array.length v.lines) ->
        let lineDifference = line - cur_line in
        let lineStart = Array.unsafe_get v.lines (lineDifference - 1) + 1 in
        (* + 2 in length gives us \r\n in string and - v.Start takes us to piece offset *)
        let lineLength =
          Array.unsafe_get v.lines lineDifference - lineStart + 2
        in
        let text =
          at_start_and_length lineStart lineLength rope.buffer
          |> chop_last_char_if_not_crln
        in

        let lineStartIndex = Some (cur_u32 + lineStart - v.start) in
        ([ text ], lineStartIndex) |> cont
    | PT (_, l, _, _, _, _) when line < cur_line ->
        let recurseLeftLine = cur_line - n_lines l - lines_right l in
        let recurseLeftIndex = cur_u32 - utf32_length l - utf32_size_right l in
        get recurseLeftLine recurseLeftIndex l acc (fun x -> x |> cont)
    | PT (_, _, _, v, _, r) when line > cur_line + Array.length v.lines ->
        let right_line = cur_line + Array.length v.lines + lines_left r in
        let right_u32 = cur_u32 + v.utf32_length + utf32_size_left r in
        get right_line right_u32 r acc (fun x -> x |> cont)
    | PT (_, _, _, _, _, _) -> failwith "unreachable Piece_tree.get_line case"
  in
  let strList, lineStartIndex =
    get (lines_left rope.pieces)
      (utf32_size_left rope.pieces)
      rope.pieces [] top_level_cont
  in
  let str = String.concat "" strList in

  match lineStartIndex with
  | Some idx ->
      let offsets = offsets_from_ut32 idx rope in
      {
        line = str |> chop_first_char_if_ln;
        utf32_offset = idx;
        utf16_offset = offsets.utf16_pos;
        utf8_offset = offsets.utf8_pos;
      }
  | None -> { line = str; utf32_offset = 0; utf16_offset = 0; utf8_offset = 0 }

let empty = PE

let fold_text rope state folder =
  fold (fun x pc -> folder x (text pc rope.buffer)) state rope.pieces

let get_text rope =
  let lst = fold_text rope [] (fun acc str -> str :: acc) in
  List.rev lst |> String.concat ""

let fold_lines rope state folder =
  let metadata = tree_size rope.pieces in
  let total_lines = metadata.subtree_lines in
  let rec fld lineNum state =
    if lineNum > total_lines then state
    else
      let line = get_line lineNum rope in
      let state = folder state line in
      fld (lineNum + 1) state
  in
  fld 0 state

let fold_match_indices str rope initial_state folder =
  let chr = String.unsafe_get str 0 in
  let _, length, _ = Unicode.count_string_stats str 0 in

  let rec fnd str_idx utf32_pos text acc tree_pos =
    if str_idx = String.length text then acc
    else
      let cur_chr = String.unsafe_get text str_idx in
      let char_length = Unicode.utf8_length cur_chr in
      if cur_chr = chr then
        let substr = substring utf32_pos length rope in
        let acc =
          if substr = str then folder acc (utf32_pos + tree_pos) else acc
        in
        fnd (str_idx + char_length) (utf32_pos + 1) text acc tree_pos
      else fnd (str_idx + char_length) (utf32_pos + 1) text acc tree_pos
  in

  let result, _ =
    fold
      (fun (acc, pos) piece ->
        let text =
          Piece_buffer.substring piece.start piece.utf32_length rope.buffer
        in
        let acc = fnd 0 0 text acc pos in
        (acc, pos + piece.utf32_length))
      (initial_state, 0) rope.pieces
  in
  result

let find_matches find_string rope =
  let lst =
    fold_match_indices find_string rope [] (fun acc idx -> idx :: acc)
  in
  List.rev lst |> Array.of_list

let find_and_replace find_string (replace_node : node) rope =
  let _, find_string_length, _ = Unicode.count_string_stats find_string 0 in
  let length_difference = replace_node.utf32_length - find_string_length in
  let folder (acc_tree, acc_diff) idx =
    let idx = idx + acc_diff in
    let acc_tree = delete_tree idx find_string_length acc_tree rope.buffer in
    ( insert_tree idx replace_node acc_tree rope.buffer,
      acc_diff + length_difference )
  in
  let tree, _ = fold_match_indices find_string rope (rope.pieces, 0) folder in
  tree
