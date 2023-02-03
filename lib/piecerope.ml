type node = {
  start: int;
  length: int;
  left_idx: int;
  right_idx: int;
  left_lns: int;
  right_lns: int;
  lines: int array;
}

type tree = 
  | PE
  | PT of int * tree * node * tree

type t = {
  buffer: Buffer.t;
  pieces: tree;
}

let top_level_cont x = x

let create start length lines = { 
  start = start; 
  length = length; 
  left_idx = 0; 
  right_idx = 0;
  left_lns = 0;
  right_lns = 0; 
  lines = lines; 
}

let plus_left idxDelta lnDelta node =
  { node with left_idx = node.left_idx + idxDelta; left_lns = node.left_lns + lnDelta }

let plus_right idxDelta lnDelta node =
  { node with right_idx = node.right_idx + idxDelta; right_lns = node.right_lns + lnDelta }

let set_data (leftSize, leftLines) (rightSize, rightLines) node =
  { node with 
      left_idx = leftSize;
      left_lns = leftLines; 
      right_idx = rightSize;
      right_lns = rightLines; }
  
let n_length node = 
  match node with
  | PE -> 0
  | PT(_, _, v, _) -> v.length

let n_lines node =
  match node with
  | PE -> 0
  | PT(_, _, v, _) -> Array.length v.lines

let tree_size node =
  match node with
  | PE -> 0
  | PT(_, _, v, _) -> v.left_idx + v.right_idx + v.length

let idx_ln_size node =
  match node with
  | PE -> 0, 0
  | PT(_, _, v, _) -> v.left_idx + v.right_idx + v.length,
                      v.left_lns + v.right_lns + Array.length v.lines

let size_left node = 
  match node with 
  | PE -> 0
  | PT(_, _, v, _) -> v.left_idx

let size_right node = 
  match node with 
  | PE -> 0
  | PT(_, _, v, _) -> v.right_idx

let lines_left node =
  match node with
  | PE -> 0
  | PT(_, _, v, _) -> v.left_lns

let lines_right node =
  match node with
  | PE -> 0
  | PT(_, _, v, _) -> v.right_lns

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
      let arrLeft = Array.sub lines 0 (splitPoint - 1) in
      let arrRight = Array.sub lines splitPoint (Array.length lines - splitPoint - 1) in
      arrLeft, arrRight
  | None ->
      lines, Array.make 0 0

let delete_lines_in_range p1Length p2Start lines =
  let p1Lines =
    match try_find_index (fun x -> x >= p1Length) lines with
    | Some x -> Array.sub lines 0 (x - 1)
    | None -> Array.make 0 0
  in
  let p2Lines =
    match try_find_index (fun x -> x >= p2Start) lines with
    | Some x -> Array.sub lines x (Array.length lines - 1 - x)
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
    match try_find_index (fun x -> x >= difference) piece.lines with
    | Some x -> Array.sub piece.lines x (Array.length piece.lines - 1 - x)
    | None -> Array.make 0 0 
  in
  (newStart, newLength, newLines)

let delete_at_end curIndex start piece =
  let length = start - curIndex in
  let lines = 
    match try_find_index (fun x -> x <= length) piece.lines with
    | Some x -> Array.sub piece.lines x (Array.length piece.lines - 1 - x)
    | None -> Array.make 0 0
  in
  (length, lines)

let text piece rope = Buffer.substring piece.start piece.length rope.buffer

let text_in_range curIndex start finish piece table =
  let textStart = start - curIndex + piece.start in
  let textLength = finish - curIndex + piece.start - textStart in
  Buffer.substring textStart textLength table.buffer

let text_at_start curIndex finish piece table =
  let textLength = finish - curIndex in
  Buffer.substring piece.start textLength table.buffer

let text_at_end curIndex start piece table =
  let textStart = start - curIndex + piece.start in
  let textLength = piece.start + piece.length - textStart in
  Buffer.substring textStart textLength table.buffer

let at_start_and_length start length table =
  Buffer.substring start length table.buffer

(* AA Tree balancing functions. *)
let sngl = function
  | PE -> false
  | PT(_, _, _, PE) -> false
  | PT(lvx, _, _, PT(lvy, _, _, _)) -> lvx > lvy

let lvl = function
  | PE -> 0
  | PT(lvt, _, _, _) -> lvt

let skew = function
  | PT(lvx, PT(lvy, a, ky, b), kx, c) when lvx = lvy ->
      let kx = set_data (idx_ln_size b) (idx_ln_size c) kx in
      let innerNode = PT(lvx, b, kx, c) in
      let ky = set_data (idx_ln_size a) (idx_ln_size innerNode) ky in
      PT(lvx, a, ky, innerNode)
  | t -> t

let split = function
  | PT(lvx, a, kx, PT(lvy, b, ky, PT(lvz, c, kz, d))) when lvx = lvy && lvy = lvz ->
      let right = PT(lvx, c, kz, d) in
      let kx = set_data (idx_ln_size a) (idx_ln_size b) kx in
      let left = PT(lvx, a, kx, b) in
      let ky = set_data (idx_ln_size left) (idx_ln_size right) ky in
      PT(lvx + 1, left, ky, right)
  | t -> t

let nlvl = function
  | PT(lvt, _, _, _) as t -> if sngl t then lvt else lvt - 1
  | _ -> failwith "unexpected nlvl case"

let adjust = function
  | PT(lvt, lt, _, rt) as t when lvl lt >= lvt - 1 && lvl rt >= (lvt - 1) -> 
      t
  | PT(lvt, lt, kt, rt) when lvl rt < lvt - 1 && sngl lt -> 
      PT(lvt - 1, lt, kt, rt) |> skew
  | PT(lvt, PT(lv1, a, kl, PT(lvb, lb, kb, rb)), kt, rt) when lvl rt < lvt - 1 -> 
      let kl = set_data (idx_ln_size a) (idx_ln_size lb) kl in
      let leftNode = PT(lv1, a, kl, lb) in
      let kt = set_data (idx_ln_size rb) (idx_ln_size rt) kt in
      let rightNode = PT(lvt - 1, rb, kt, rt) in
      let kb = set_data (idx_ln_size leftNode) (idx_ln_size rightNode) kb in
      PT(lvb + 1, leftNode, kb, rightNode)
  | PT(lvt, lt, kt, rt) when lvl rt < lvt -> 
      PT(lvt - 1, lt, kt, rt) |> split
  | PT(lvt, lt, kt, PT(_, (PT(lva, c, ka, d) as a), kr, b)) ->
      let kt = set_data (idx_ln_size lt) (idx_ln_size c) kt in
      let leftNode = PT(lvt - 1, lt, kt, c) in
      let kr = set_data (idx_ln_size d) (idx_ln_size b) kr in
      let rightNode = PT(nlvl a, d, kr, b) |> split in
      let ka = set_data (idx_ln_size leftNode) (idx_ln_size rightNode) ka in
      PT(lva + 1, leftNode, ka, rightNode)
  | t -> t

let rec split_max = function
  | PT(_, l, v, PE) ->
      let v' = set_data (idx_ln_size l) (0, 0) v in
      l, v'
  | PT(h, l, v, r) -> 
      let (r', b) = split_max r in
      let (r'Size, r'Lines) = idx_ln_size r' in
      let v' = {v with right_idx = r'Size; right_lns = r'Lines} in
      let newLeft = PT(h, l, v', r') in
      let (leftSize, leftLines) = idx_ln_size newLeft in
      let b' = {b with left_idx = leftSize; left_lns = leftLines} in
      newLeft, b'
  | PE -> failwith "unexpected splitMax case"

let rec fold f x t =
  match t with
  | PE -> x
  | PT(_, l, v, r) ->
      let x = fold f x l in
      let x = f x v in
      fold f x r

(* Core PieceTree logic. *)
let ins_min pcStart pcLength pcLines tree =
  let rec min node cont =
    match node with
    | PE -> PT(1, PE, create pcStart pcLength pcLines, PE) |> cont
    | PT(h, l, v, r) ->
        min l (fun l' ->
          let v' = plus_left pcLength (Array.length pcLines) v in
          PT(h, l', v', r) |> skew |> split |> cont
        )
  in
  min tree top_level_cont

let ins_max pcStart pcLength (pcLines: int array) tree = 
  let rec max node cont =
    match node with
    | PE -> PT(1, PE, create pcStart pcLength pcLines, PE) |> cont
    | PT(h, l, v, r) ->
        max r (fun r' ->
          let v' = plus_right pcLength (Array.length pcLines) v in
          PT(h, l, v', r') |> skew |> split |> cont
        )
  in
  max tree top_level_cont

let is_consecutive v pcStart = v.start + v.length = pcStart

let insert_tree insIndex pcStart pcLength pcLines tree =
  let rec ins curIndex node cont =
    match node with
    | PE -> PT(1, PE, create pcStart pcLength pcLines, PE) |> cont
    | PT(h, l, v, r) when insIndex < curIndex ->
        let nextIndex = curIndex - n_length l - size_right l in
        let v' = plus_left pcLength (Array.length pcLines) v in
        ins nextIndex l (fun l' -> 
          PT(h, l', v', r) |> skew |> split |> cont
        )
    | PT(h, l, v, r) when insIndex > curIndex + v.length ->
        let nextIndex = curIndex + v.length + size_left r in
        let v' = plus_right pcLength (Array.length pcLines) v in
        ins nextIndex r (fun r' ->
          PT(h, l, v', r') |> skew |> split |> cont
        )
    | PT(h, l, v, r) when insIndex = curIndex ->
        let v' = plus_left pcLength (Array.length pcLines) v in
        let l' = ins_max pcStart pcLength pcLines l in
        PT(h, l', v', r) |> skew |> split |> cont
    | PT(h, l, v, r) when insIndex = curIndex + v.length && is_consecutive v pcStart ->
        let v'Lines = Array.append v.lines pcLines in
        let v' = { v with length = v.length + pcLength; lines = v'Lines } in
        PT(h, l, v', r) |> cont
    | PT(h, l, v, r) when insIndex = curIndex + v.length ->
        let v' = plus_right pcLength (Array.length pcLines) v in
        let r' = ins_min pcStart pcLength pcLines r in
        PT(h, l, v', r') |> skew |> split |> cont
    | PT(h, l, v, r) ->
        let difference = insIndex - curIndex in
        let rStart = v.start + difference in
        let rLength = v.length - difference in
        
        let (leftLines, rightLines) = split_lines rStart v.lines in
        
        let l' = ins_max v.start difference leftLines l in
        let r' = ins_min rStart rLength rightLines r in
        let v' =  { 
                    start = pcStart;
                    length = pcLength;
                    lines = pcLines;
                    left_idx = v.left_idx + difference;
                    left_lns = v.left_lns + Array.length leftLines;
                    right_idx = v.right_idx + rLength;
                    right_lns = v.right_lns + Array.length rightLines;
                  } in
        PT(h, l', v', r') |> skew |> split |> cont
    in
    ins (size_left tree) tree top_level_cont

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
  let rec del curIndex node =
    match node with
    | PE -> PE
    | PT(h, l, v, r) ->
        let left =
          if start < curIndex
          then del (curIndex - n_length l - size_right l) l
          else l in
        
        let nodeEndIndex = curIndex + v.length in
        
        let right =
          if finish > nodeEndIndex
          then del (nodeEndIndex + size_left r) r
          else r in

        if in_range start curIndex finish nodeEndIndex then
          if left = PE
          then right
          else
            let (newLeft, newVal) = split_max left in
            let v' = set_data (idx_ln_size newLeft) (idx_ln_size right) newVal in
            PT(h, newLeft, v', right) |> adjust
        else if start_is_in_range start curIndex finish nodeEndIndex then
          let (newStart, newLength, newLines) = delete_at_start curIndex finish v in
          let (leftidx, leftlns) = idx_ln_size left in
          let (rightidx, rightlns) = idx_ln_size right in
          let v' =  {
                      start = newStart;
                      length = newLength;
                      lines = newLines;
                      left_idx = leftidx;
                      left_lns = leftlns;
                      right_idx = rightidx;
                      right_lns = rightlns;
                    } in
          PT(h, left, v', right) |> skew |> split
        else if end_is_in_range start curIndex finish nodeEndIndex then
          let (length, lines) = delete_at_end curIndex start v in
          let (leftidx, leftlns) = idx_ln_size left in
          let (rightidx, rightlns) = idx_ln_size right in
          let v' =  {
                      v with
                      length = length;
                      lines = lines;
                      left_idx = leftidx;
                      left_lns = leftlns;
                      right_idx = rightidx;
                      right_lns = rightlns;
                    } in
          PT(h, left, v', right) |> adjust
        else if middle_is_in_range start curIndex finish nodeEndIndex then
          let (p1Length, p1Lines, p2Start, p2Length, p2Lines) =
            delete_in_range curIndex start finish v in
          let newRight = ins_min p2Start p2Length p2Lines right in
          let (leftidx, leftlns) = idx_ln_size left in
          let (rightidx, rightlns) = idx_ln_size newRight in
          let v' =  {
                      v with
                      length = p1Length;
                      lines = p1Lines;
                      left_idx = leftidx;
                      left_lns = leftlns;
                      right_idx = rightidx;
                      right_lns = rightlns;
                    } in
          PT(h, left, v', newRight) |> skew |> split
        else
          let v' = set_data (idx_ln_size left) (idx_ln_size right) v in
          PT(h, left, v', right) |> adjust
  in
  del (size_left tree) tree

let substring start length piecerope =
  let finish = start + length in
  let rec sub curIndex node acc =
    match node with
    | PE -> acc
    | PT(_, l, v, r) ->
        let left =
          if start < curIndex
          then sub (curIndex - n_length l - size_right l) l acc
          else acc
        in
        
        let nodeEndIndex = curIndex + v.length in
        let middle =
          if in_range start curIndex finish nodeEndIndex then
            left ^ text v piecerope
          else if start_is_in_range start curIndex finish nodeEndIndex then
            left ^ text_at_start curIndex finish v piecerope
          else if end_is_in_range start curIndex finish nodeEndIndex then
            left ^ text_at_end curIndex start v piecerope
          else if middle_is_in_range start curIndex finish nodeEndIndex then
            left ^ text_in_range curIndex start finish v piecerope
          else
            left

        in
        if finish > curIndex
        then sub (nodeEndIndex + size_left r) r middle
        else middle
  in
  sub (size_left piecerope.pieces) piecerope.pieces ""

(* Delete/substring if-statements adapted to work with lines. *)
let line_in_range nodeStartLine searchLine nodeEndLine =
  nodeStartLine = searchLine && searchLine = nodeEndLine

let start_is_in_line nodeStartLine searchLine nodeEndLine =
  nodeStartLine = searchLine && searchLine < nodeEndLine

let end_is_in_line nodeStartLine searchLine nodeEndLine =
  nodeStartLine < searchLine && searchLine = nodeEndLine

let middle_is_in_line nodeStartLine searchLine nodeEndLine =
  nodeStartLine < searchLine && nodeEndLine > searchLine

let get_line line table =
  let rec get curLine node acc =
      match node with
      | PE -> acc
      | PT(_, l, v, r) ->
          let left = 
              if line <= curLine
              then get (curLine - n_lines l - lines_right l) l acc
              else acc
          in

          let nodeEndLine = curLine + (Array.length v.lines) in
          let middle =
              if line_in_range curLine line nodeEndLine then
                  left ^ text v table
              else if start_is_in_line curLine line nodeEndLine then
                  (* + 1 gives us \n in string and - v.Start takes us to piece offset *)
                  let length: int = (Array.get v.lines 0) + 1 - v.start in
                  left ^ at_start_and_length v.start length table
              else if end_is_in_line curLine line nodeEndLine then
                  let start = (Array.get v.lines (Array.length v.lines - 1)) + 1 in
                  let length = v.length - start + v.start in
                  left ^ at_start_and_length start length table
              else if middle_is_in_line curLine line nodeEndLine then
                  let lineDifference = line - curLine in
                  let lineStart = (Array.get v.lines (lineDifference - 1)) + 1 in
                  let lineLength = (Array.get v.lines lineDifference) - lineStart + 1 in
                  left ^ at_start_and_length lineStart lineLength table
              else
                  left
          in

          if line >= nodeEndLine
          then get (nodeEndLine + lines_left r) r middle
          else middle
  in
  get (lines_left table.pieces) table.pieces ""

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

let empty = { buffer = Buffer.empty; pieces = PE }

let get_text piecerope = 
  fold (fun (acc: string) pc ->
    let text = Buffer.substring pc.start pc.length piecerope.buffer in
    acc ^ text
  ) "" piecerope.pieces

let insert index (str: string) piecerope =
  let pcStart = Buffer.size piecerope.buffer in
  let pcLines = find_line_breaks str (String.length str - 1) pcStart 0 [] in
  let buffer = Buffer.append str piecerope.buffer in
  let pieces = insert_tree index pcStart (String.length str) pcLines piecerope.pieces in
  { buffer; pieces }

let delete start length piecerope =
  let pieces = delete_tree start length piecerope.pieces in
  { piecerope with pieces = pieces; }

let create str = insert 0 str empty

let total_length piecerope = tree_size piecerope.pieces

let total_lines piecerope = n_lines piecerope.pieces
