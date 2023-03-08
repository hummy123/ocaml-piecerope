type node = {
  start:            int;
  utf8_length:      int;
  utf16_length:     int;
  utf32_length:     int;
  lines:            int array;
}

type metadata = {
  utf8_subtree:   int;
  utf16_subtree:  int;
  utf32_subtree:  int;
  subtree_lines:  int;
}

let create_metadata utf8 utf16 utf32 lines = {
  utf8_subtree  = utf8;
  utf16_subtree = utf16;
  utf32_subtree = utf32;
  subtree_lines = lines;
}

let emptyMetadata = {
  utf8_subtree  = 0;
  utf16_subtree = 0;
  utf32_subtree = 0;
  subtree_lines = 0;
}

type t = 
  | PE
  | PT of int * t * metadata * node * metadata * t

let ht = function
  | PE -> 0
  | PT(h, _, _, _, _, _) -> h

(* Getting narious node data. *)
let utf8_length node = 
  match node with
  | PE -> 0
  | PT(_, _, _, v, _, _) -> v.utf8_length

let utf16_length node = 
  match node with
  | PE -> 0
  | PT(_, _, _, v, _, _) -> v.utf16_length

let utf32_length node = 
  match node with
  | PE -> 0
  | PT(_, _, _, v, _, _) -> v.utf32_length

let n_lines node =
  match node with
  | PE -> 0
  | PT(_, _, _, v, _, _) -> Array.length v.lines

let tree_size node =
  match node with
  | PE -> emptyMetadata
  | PT(_, _, lm, v, rm, _) -> 
      let utf8 = lm.utf8_subtree + rm.utf8_subtree + v.utf8_length in
      let utf16 = lm.utf16_subtree + rm.utf16_subtree + v.utf16_length in
      let utf32 = lm.utf32_subtree + rm.utf32_subtree + v.utf32_length in
      let lines = lm.subtree_lines + rm.subtree_lines + Array.length v.lines in
      create_metadata utf8 utf16 utf32 lines

let tree_lines node =
  match node with
  | PE -> 0
  | PT(_, _, lm, v, rm, _) -> 
      lm.subtree_lines + rm.subtree_lines + Array.length v.lines

let utf8_size_left node = 
  match node with 
  | PE -> 0
  | PT(_, _, lm, _, _, _) -> 
      lm.utf8_subtree

let utf16_size_left node = 
  match node with 
  | PE -> 0
  | PT(_, _, lm, _, _, _) -> 
      lm.utf16_subtree

let utf32_size_left node = 
  match node with 
  | PE -> 0
  | PT(_, _, lm, _, _, _) -> 
      lm.utf32_subtree

let utf8_size_right node = 
  match node with 
  | PE -> 0
  | PT(_, _, _, _, rm, _) -> 
      rm.utf8_subtree

let utf16_size_right node = 
  match node with 
  | PE -> 0
  | PT(_, _, _, _, rm, _) -> 
      rm.utf16_subtree

let utf32_size_right node = 
  match node with 
  | PE -> 0
  | PT(_, _, _, _, rm, _) -> 
      rm.utf32_subtree

let lines_left node =
  match node with
  | PE -> 0
  | PT(_, _, lm, _, _, _) -> lm.subtree_lines

let lines_right node =
  match node with
  | PE -> 0
  | PT(_, _, _, _, rm, _) -> rm.subtree_lines

let top_level_cont x = x

(* Creating and editing node data. *)
let create start utf8length utf16length utf32length lines = { 
  start = start; 
  utf8_length = utf8length; 
  utf16_length = utf16length;
  utf32_length = utf32length;
  lines = lines; 
}

(* AVL tree balance functions. *)
let mk l a r = 
  let h = (if ht l > ht r then ht l else ht r) + 1 in
  PT(h, l, tree_size l, a, tree_size r, r)

let balL ab x c =
  if ht ab = ht c + 2 then
    match ab with
    | PT(_, a, _, y, _, b) ->
        if ht a >= ht b then
          mk a y (mk b x c)
        else
          (match b with
           | PT(_, b1, _, bx, _, b2) -> 
               mk (mk a y b1) bx (mk b2 x c)
           | _ -> mk ab x c)
    | _ -> mk ab x c
  else
    mk ab x c

let balR a x bc =
  if ht bc = ht a + 2 then
    match bc with
    | PT(_, b, _, y, _, c) -> 
        if ht b <= ht c then
          mk (mk a x b) y c
        else
          (match b with
          | PT(_, b1, _, bx, _, b2) -> mk (mk a x b1) bx (mk b2 y c)
          | _ -> mk a x bc)
    | _ -> mk a x bc
  else
    mk a x bc

let split_max tree = 
  let rec split node cont = 
    match node with
    | PT(_, l, _, a, _, r) ->
        if r = PE then
          (l, a) |> cont
        else
          split r (fun (r', a') -> (balL l a r', a') |> cont)
    | PE -> failwith "unexpected split_max case"
  in
  split tree top_level_cont

(* Logic for handling piece nodes; not using a separate module because more work due to defining interface. *)

(* tryFindIndex function ported from F#: https://github.com/dotnet/fsharp/blob/main/src/FSharp.Core/array.fs#L1558 *)
let try_find_index (predicate: int -> bool) (lines: int array) =
  let len = Array.length lines in
  let rec find n =
    if n >= len then None
    else if predicate (Array.unsafe_get lines n) then Some n
    else find (n + 1)
  in
  find 0

let split_lines rStart (lines: int array) =
  match try_find_index (fun x -> x >= rStart) lines with
  | Some splitPoint ->
      let arrLeft = Array.sub lines 0 splitPoint in
      let arrRight = Array.sub lines splitPoint (Array.length lines - splitPoint) in
      arrLeft, arrRight
  | None ->
      lines, Array.make 0 0

let delete_lines_in_range p1Length p2Start lines =
  let p1Lines =
    match try_find_index (fun x -> x >= p1Length) lines with
    | Some x -> Array.sub lines 0 x
    | None -> lines
  in
  let p2Lines =
    match try_find_index (fun x -> x >= p2Start) lines with
    | Some x -> Array.sub lines x (Array.length lines - x)
    | None -> Array.make 0 0
  in
  p1Lines, p2Lines

let delete_in_range curIndex start finish piece =
  let finishDifference = finish - curIndex in
  let p1Length = start - curIndex in
  let p2Start = finishDifference + piece.start in
  let (p1Lines, p2Lines) = delete_lines_in_range (p1Length + piece.start) p2Start piece.lines in
  let p2Length = piece.length - finishDifference in
  (p1Length, p1Lines, p2Start, p2Length, p2Lines)

let delete_at_start curIndex finish piece =
  let difference = finish - curIndex in
  let newStart = piece.start + difference in
  let newLength = piece.length - difference in
  let newLines =
    match try_find_index (fun x -> x >= difference + piece.start) piece.lines with
    | Some x -> 
        Array.sub piece.lines x (Array.length piece.lines - 1 - x)
    | None -> 
        Array.make 0 0
  in
  (newStart, newLength, newLines)

let delete_at_end curIndex start piece =
  let length = start - curIndex in
  let lines = 
    match try_find_index (fun x -> x <= length + piece.start) piece.lines with
    | Some x ->
        Array.sub piece.lines x (Array.length piece.lines - 1 - x)
    | None -> 
        Array.make 0 0
  in
  (length, lines)

let text piece buffer = Piece_buffer.substring piece.start piece.length buffer

let text_in_range curIndex start finish piece buffer =
  let textStart = start - curIndex + piece.start in
  let textLength = finish - curIndex + piece.start - textStart in
  Piece_buffer.substring textStart textLength buffer

let text_at_start curIndex finish piece buffer =
  let textLength = finish - curIndex in
  Piece_buffer.substring piece.start textLength buffer

let text_at_end curIndex start piece buffer =
  let textStart = start - curIndex + piece.start in
  let textLength = piece.start + piece.length - textStart in
  Piece_buffer.substring textStart textLength buffer

let at_start_and_length start length buffer =
  Piece_buffer.substring start length buffer

(* AA Tree balancing functions. *)
let rec fold f x t =
  match t with
  | PE -> x
  | PT(_, l, v, r) ->
      let x = fold f x l in
      let x = f x v in
      fold f x r

(* Core PieceTree logic. *)
let is_consecutive v pcStart = v.start + v.length = pcStart

(** Adds the given piece to the start of the given tree. *)
let prepend pcStart pcLength pcLines tree =
  let rec pre node cont =
    match node with
    | PE -> PT(1, PE, create pcStart pcLength pcLines, PE) |> cont
    | PT(_, l, v, r) ->
        pre l (fun l' ->
          let v' = plus_left pcLength (Array.length pcLines) v in
          balL l' v' r |> cont
        )
  in
  pre tree top_level_cont

(** Adds the given piece to the end of the given tree. 
    If the previous insert on the tree was done at the very end,
    we merge with the last piece rather than creating a new node to keep the tree more shallow. *)
let append pcStart pcLength pcLines tree =
  let rec app node cont =
    match node with
    | PE -> PT(1, PE, create pcStart pcLength pcLines, PE) |> cont
    | PT(h, l, v, PE) when is_consecutive v pcStart ->
        let v'Lines = Array.append v.lines pcLines in
        let v' = { v with length = v.length + pcLength; lines = v'Lines } in
        PT(h, l, v', PE) |> cont
    | PT(_, l, v, r) ->
        app r (fun r' ->
          let v' = plus_right pcLength (Array.length pcLines) v in
          balR l v' r' |> cont
        )
  in
  app tree top_level_cont

(** Same as above append functino but omits the is_consecutive check. 
    Used when we split a piece and is not meant to be a public function. *)
let ins_max pcStart pcLength (pcLines: int array) tree = 
  let rec max node cont =
    match node with
    | PE -> PT(1, PE, create pcStart pcLength pcLines, PE) |> cont
    | PT(_, l, v, r) ->
        max r (fun r' ->
          let v' = plus_right pcLength (Array.length pcLines) v in
          balR l v' r' |> cont
        )
  in
  max tree top_level_cont

let insert_tree insIndex pcStart pcLength pcLines tree =
  let rec ins curIndex node cont =
    match node with
    | PE -> PT(1, PE, create pcStart pcLength pcLines, PE) |> cont
    | PT(_, l, v, r) when insIndex < curIndex ->
        let nextIndex = curIndex - utf32_length l - utf32_size_right l in
        let v' = plus_left pcLength (Array.length pcLines) v in
        ins nextIndex l (fun l' -> 
          balL l' v' r |> cont
        )
    | PT(_, l, v, r) when insIndex > curIndex + v.length ->
        let nextIndex = curIndex + v.length + utf32_size_left r in
        let v' = plus_right pcLength (Array.length pcLines) v in
        ins nextIndex r (fun r' ->
          balR l v' r' |> cont
        )
    | PT(_, l, v, r) when insIndex = curIndex ->
        let v' = plus_left pcLength (Array.length pcLines) v in
        let l' = ins_max pcStart pcLength pcLines l in
        balL l' v' r |> cont
    | PT(h, l, v, r) when insIndex = curIndex + v.length && is_consecutive v pcStart ->
        let v'Lines = Array.append v.lines pcLines in
        let v' = { v with length = v.length + pcLength; lines = v'Lines } in
        PT(h, l, v', r) |> cont
    | PT(_, l, v, r) when insIndex = curIndex + v.length ->
        let v' = plus_right pcLength (Array.length pcLines) v in
        let r' = prepend pcStart pcLength pcLines r in
        balR l v' r' |> cont
    | PT(_, l, v, r) ->
        let difference = insIndex - curIndex in
        let rStart = v.start + difference in
        let rLength = v.length - difference in
        
        let (leftLines, rightLines) = split_lines rStart v.lines in
        
        let l' = ins_max v.start difference leftLines l in
        let r' = prepend rStart rLength rightLines r in
        let v' =  { 
                    start = pcStart;
                    length = pcLength;
                    lines = pcLines;
                    left_idx = v.left_idx + difference;
                    left_lns = v.left_lns + Array.length leftLines;
                    right_idx = v.right_idx + rLength;
                    right_lns = v.right_lns + Array.length rightLines;
                  } in
        (mk l' v' r') |> cont
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

let delete_tree start length tree =
  let finish = start + length in
  let rec del curIndex node cont =
    match node with
    | PE -> PE |> cont
    | PT(_, l, v, r) when in_range start curIndex finish (curIndex + v.length) ->
        let recurseLeftIndex = curIndex - utf32_length l - utf32_size_right l in
        let recurseRightIndex = curIndex + v.length + utf32_size_left r in

        del recurseLeftIndex l (fun l' ->
          del recurseRightIndex r (fun r' -> 
            if l' = PE then
              r' |> cont
            else
              let (newLeft, newVal) = split_max l' in
              balR newLeft newVal r' |> cont
          )
        )
    | PT(_, l, v, r) when start_is_in_range start curIndex finish (curIndex + v.length) ->
        let recurseLeftIndex = curIndex - utf32_length l - utf32_size_right l in
        del recurseLeftIndex l (fun l' -> 
          let (newStart, newLength, newLines) = delete_at_start curIndex finish v in
          let v' =  { v with
                      start = newStart;
                      length = newLength;
                      lines = newLines;
                      left_idx = tree_size l';
                      left_lns = tree_lines l';
                    } in
          balR l' v' r |> cont
        )
    | PT(_, l, v, r) when end_is_in_range start curIndex finish (curIndex + v.length) ->
        let recurseRightIndex = curIndex + v.length + utf32_size_left r in
        del recurseRightIndex r (fun r' ->
          let (length, lines) = delete_at_end curIndex start v in
          let v' =  {
                      v with
                      length = length;
                      lines = lines;
                      right_idx = tree_size r';
                      right_lns = tree_lines r';
                    } in
          balL l v' r' |> cont
        )
    | PT(_, l, v, r) when middle_is_in_range start curIndex finish (curIndex + v.length) ->
        let (p1Length, p1Lines, p2Start, p2Length, p2Lines) =
          delete_in_range curIndex start finish v in
        let newRight = prepend p2Start p2Length p2Lines r in
        let v' =  {
                    v with
                    length = p1Length;
                    lines = p1Lines;
                    right_idx = tree_size newRight;
                    right_lns = tree_lines newRight;
                  } in
        balR l v' newRight |> cont
    | PT(_, l, v, r) when start < curIndex ->
        let recurseLeftIndex = curIndex - utf32_length l - utf32_size_right l in
        del recurseLeftIndex l (fun l' -> 
          balR l' v r |> cont
        )
    | PT(_, l, v, r) when finish > curIndex + v.length ->
        let recurseRightIndex = curIndex + v.length + utf32_size_left r in
        del recurseRightIndex r (fun r' -> 
          balL l v r' |> cont
        )
    | PT(h, l, v, r) ->
        (* Unreachable case. *)
        PT(h, l, v, r) |> cont
  in
  del (utf32_size_left tree) tree top_level_cont

let substring start length tree buffer =
  let finish = start + length in
  let rec sub curIndex node acc cont =
    match node with
    | PE -> 
        acc |> cont
    (* Cases when the current node is at least partially in the substring range. *)
    (* The end of the current substring range is in this node, |whi|ch means the start of this node. *)
    | PT(_, _, v, r) when end_is_in_range start curIndex finish (curIndex + v.length) ->
        let nodeText = text_at_end curIndex start v buffer in
        let recurseRightIndex = curIndex + v.length + utf32_size_left r in
        sub recurseRightIndex r acc (fun x -> nodeText::x |> cont)

    (* The currennt node is |fully| within the substring range. *)
    | PT(_, l, v, r) when in_range start curIndex finish (curIndex + v.length) ->
        let nodeEndIndex = curIndex + v.length in
        let nodeText = text v buffer in

        let recurseRightIndex = nodeEndIndex + utf32_size_left r in
        let recurseLeftIndex = curIndex - utf32_length l - utf32_size_right l in

        sub recurseRightIndex r acc (fun right ->
          sub recurseLeftIndex l (nodeText::right) (fun x -> x |> cont)
        )

    (* The start of the substring range is in the node, whi|ch| means the end of this node. *)
    | PT(_, l, v, _) when start_is_in_range start curIndex finish (curIndex + v.length) ->
        let nodeText = text_at_start curIndex finish v buffer in
        let recurseLeftIndex = curIndex - utf32_length l - utf32_size_right l in
        sub recurseLeftIndex l (nodeText::acc) (fun x -> x |> cont)

    (* The mi|d|dle of the current node is in the substring range. *)
    | PT(_, _, v, _) when middle_is_in_range start curIndex finish (curIndex + v.length) ->
        [text_in_range curIndex start finish v buffer] |> cont

    (* Below two cases navigate to the next node when the substring range is outside the current node. *)
    (* When the current node is after the substring's end range. *)
    | PT(_, l, _, _) when start < curIndex ->
        let recurseLeftIndex = curIndex - utf32_length l - utf32_size_right l in
        sub recurseLeftIndex l acc (fun x -> x |> cont)
    (* When the current node is before the substring's start range. *)
    | PT(_, _, v, r) when finish > curIndex + v.length ->
        let recurseRightIndex = curIndex + v.length + utf32_size_left r in
        sub recurseRightIndex r acc (fun x -> x |> cont)
    | PT(_, _, _, _) -> 
        failwith "unreachable Buffer.substring case"
 in
 String.concat "" (sub (utf32_size_left tree) tree [] top_level_cont)

(* Delete/substring if-statements adapted to work with lines. *)
let node_is_in_line nodeStartLine searchLine nodeEndLine =
  nodeStartLine = searchLine && searchLine = nodeEndLine

let end_of_line_is_in_node nodeStartLine searchLine nodeEndLine =
  nodeStartLine = searchLine && searchLine < nodeEndLine

let start_of_line_in_node nodeStartLine searchLine nodeEndLine =
  nodeStartLine < searchLine && searchLine = nodeEndLine

let line_is_in_node nodeStartLine searchLine nodeEndLine =
  nodeStartLine < searchLine && nodeEndLine > searchLine

let get_line_and_line_start_index line tree buffer =
  let rec get curLine curIndex node acc cont =
    match node with
    | PE -> 
        (acc, None) |> cont

    | PT(_, _, v, r) when start_of_line_in_node curLine line (curLine + Array.length v.lines) ->
        (* Start of line in terms of piece offset. *)
        let lineStart = (Array.unsafe_get v.lines (Array.length v.lines - 1)) + 1 in
        let length = v.length - lineStart + v.start in
        let nodeText = at_start_and_length lineStart length buffer in

        (* Index where line starts in terms of piece tree (not buffer). *)
        let lineStartIndex = Some (curIndex + v.length - lineStart) in

        let recurseRightLine = curLine + Array.length v.lines + lines_left r in
        let recurseRightIndex = curIndex + v.length + utf32_size_left r in

        get recurseRightLine recurseRightIndex r acc (fun (acc, _) -> 
          (nodeText::acc, lineStartIndex) |> cont
        )

    | PT(_, l, v, r) when node_is_in_line curLine line (curLine + Array.length v.lines) ->
        let nodeEndLine = curLine + Array.length v.lines in
        let nodeText = text v buffer in

        let recurseRightLine = nodeEndLine + lines_left r in
        let recurseLeftLine = curLine - n_lines l - lines_right l in

        let recurseRightIndex = curIndex + v.length + utf32_size_left r in
        let recurseLeftIndex = curIndex - utf32_length l - utf32_size_right l in

        get recurseRightLine recurseRightIndex r acc (fun (acc, _) -> 
          get recurseLeftLine recurseLeftIndex l (nodeText::acc) (fun (acc, lidx) ->
            match lidx with
            | Some _ -> (acc, lidx) |> cont
            | None -> (acc, Some curIndex) |> cont
          )
        )

    | PT(_, l, v, _) when end_of_line_is_in_node curLine line (curLine + Array.length v.lines) ->
        (* + 1 gives us \n in string and - v.Start takes us to piece offset *)
        let length: int = (Array.unsafe_get v.lines 0) + 1 - v.start in
        let nodeText = at_start_and_length v.start length buffer in

        let recurseLeftLine = curLine - n_lines l - lines_right l in
        let recurseLeftIndex = curIndex - utf32_length l - utf32_size_right l in

        get recurseLeftLine recurseLeftIndex l (nodeText::acc) (fun (acc, lidx) -> 
          match lidx with
          | Some _ -> (acc, lidx) |> cont
          | None -> (acc, Some curIndex) |> cont
        )

    | PT(_, _, v, _) when line_is_in_node curLine line (curLine + Array.length v.lines) ->
        let lineDifference = line - curLine in
        let lineStart = (Array.unsafe_get v.lines (lineDifference - 1)) + 1 in
        let lineLength = (Array.unsafe_get v.lines lineDifference) - lineStart + 1 in

        let lineStartIndex = Some ((lineStart - v.start) - curIndex) in
        ([at_start_and_length lineStart lineLength buffer], lineStartIndex) |> cont

    | PT(_, l, _, _) when line < curLine ->
        let recurseLeftLine = curLine - n_lines l - lines_right l in
        let recurseLeftIndex = curIndex - utf32_length l - utf32_size_right l in
        get recurseLeftLine recurseLeftIndex l acc (fun x -> x |> cont)

    | PT(_, _, v, r) when line > curLine + Array.length v.lines ->
        let recurseRightLine = curLine + Array.length v.lines + lines_left r in
        let recurseRightIndex = curIndex + v.length + utf32_size_left r in
        get recurseRightLine recurseRightIndex r acc (fun x -> x |> cont)

    | PT(_, _, _, _) -> 
        failwith "unreachable Piece_tree.get_line case"
 in
 let (strList, lineStartIndex) = get (lines_left tree) (utf32_size_left tree) tree [] top_level_cont in
 let str = String.concat "" strList in

 match lineStartIndex with
 | Some idx -> str, idx
 | None -> str, 0

let get_line line tree buffer = 
  let (str, _) = get_line_and_line_start_index line tree buffer in
  str

let empty = PE

let get_text tree buffer = 
  let lst = fold (fun (acc: string list) pc ->
    let text = Piece_buffer.substring pc.start pc.length buffer in
    text::acc
  ) [] tree in
  List.rev lst |> String.concat ""

let fold_text tree buffer state folder =
  fold (fun _ pc ->
    folder (Piece_buffer.substring pc.start pc.length buffer)
  ) state tree

let total_length tree = tree_size tree

let total_lines tree = n_lines tree
 
