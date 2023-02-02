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

let topLevelCont x = x

let create start length lines = { 
  start = start; 
  length = length; 
  left_idx = 0; 
  right_idx = 0;
  left_lns = 0;
  right_lns = 0; 
  lines = lines; 
}

let addLeft idxDelta lnDelta node =
  { node with left_idx = node.left_idx + idxDelta; left_lns = node.left_lns + lnDelta }

let addRight idxDelta lnDelta node =
  { node with right_idx = node.right_idx + idxDelta; right_lns = node.right_lns + lnDelta }

let setIdx left right node =
  { node with left_idx = left; right_idx = right; }

let setData (leftSize, leftLines) (rightSize, rightLines) node =
  { node with 
      left_idx = leftSize;
      left_lns = leftLines; 
      right_idx = rightSize;
      right_lns = rightLines; }
  
let nLength node = 
  match node with
  | PE -> 0
  | PT(_, _, v, _) -> v.length

let nLines node =
  match node with
  | PE -> 0
  | PT(_, _, v, _) -> Array.length v.lines

let size node =
  match node with
  | PE -> 0
  | PT(_, _, v, _) -> v.left_idx + v.right_idx + v.length

let idxLnSize node =
  match node with
  | PE -> 0, 0
  | PT(_, _, v, _) -> v.left_idx + v.right_idx + v.length,
                      v.left_lns + v.right_lns + Array.length v.lines

let stringLength node =
  match node with
  | PE -> 0
  | PT(_, _, v, _) -> v.length

let sizeLeft node = 
  match node with 
  | PE -> 0
  | PT(_, _, v, _) -> v.left_idx

let sizeRight node = 
  match node with 
  | PE -> 0
  | PT(_, _, v, _) -> v.right_idx

let linesLeft node =
  match node with
  | PE -> 0
  | PT(_, _, v, _) -> v.left_lns

let linesRight node =
  match node with
  | PE -> 0
  | PT(_, _, v, _) -> v.right_lns

(* Logic for handling piece nodes; not using a separate module because more work due to defining interface. *)

(* tryFindIndex function ported from F#: https://github.com/dotnet/fsharp/blob/main/src/FSharp.Core/array.fs#L1558 *)
let tryFindIndex (predicate: int -> bool) (lines: int array) =
  let len = Array.length lines in
  let rec find n =
    if n >= len then None
    else if predicate (Array.unsafe_get lines n) then Some n
    else find (n + 1)
  in
  find 0

let splitLines rStart (lines: int array) =
  match tryFindIndex (fun x -> x >= rStart) lines with
  | Some splitPoint ->
      let arrLeft = Array.sub lines 0 (splitPoint - 1) in
      let arrRight = Array.sub lines splitPoint (Array.length lines - splitPoint - 1) in
      arrLeft, arrRight
  | None ->
      lines, Array.make 0 0

let deleteLinesInRange p1Length p2Start lines =
  let p1Lines =
    match tryFindIndex (fun x -> x >= p1Length) lines with
    | Some x -> Array.sub lines 0 (x - 1)
    | None -> Array.make 0 0
  in
  let p2Lines =
    match tryFindIndex (fun x -> x >= p2Start) lines with
    | Some x -> Array.sub lines x (Array.length lines - 1 - x)
    | None -> Array.make 0 0
  in
  p1Lines, p2Lines

let deleteInRange curIndex start finish piece =
  let finishDifference = finish - curIndex in
  let p1Length = start - curIndex in
  let p2Start = finishDifference + piece.start in
  let (p1Lines, p2Lines) = deleteLinesInRange (p1Length + piece.start) p2Start piece.lines in
  let p2Length = piece.length - finishDifference in
  (p1Length, p1Lines, p2Start, p2Length, p2Lines)

let deleteAtStart curIndex finish piece =
  let difference = finish - curIndex in
  let newStart = piece.start + difference in
  let newLength = piece.length - difference in
  let newLines =
    match tryFindIndex (fun x -> x >= difference) piece.lines with
    | Some x -> Array.sub piece.lines x (Array.length piece.lines - 1 - x)
    | None -> Array.make 0 0 
  in
  (newStart, newLength, newLines)

let deleteAtEnd curIndex start piece =
  let length = start - curIndex in
  let lines = 
    match tryFindIndex (fun x -> x <= length) piece.lines with
    | Some x -> Array.sub piece.lines x (Array.length piece.lines - 1 - x)
    | None -> Array.make 0 0
  in
  (length, lines)

let text piece rope = Buffer.substring piece.start piece.length rope.buffer

let textInRange curIndex start finish piece table =
  let textStart = start - curIndex + piece.start in
  let textLength = finish - curIndex + piece.start - textStart in
  Buffer.substring textStart textLength table.buffer

let textAtStart curIndex finish piece table =
  let textLength = finish - curIndex in
  Buffer.substring piece.start textLength table.buffer

let textAtEnd curIndex start piece table =
  let textStart = start - curIndex + piece.start in
  let textLength = piece.start + piece.length - textStart in
  Buffer.substring textStart textLength table.buffer

let atStartAndLength start length table =
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
      let kx = setData (idxLnSize b) (idxLnSize c) kx in
      let innerNode = PT(lvx, b, kx, c) in
      let ky = setData (idxLnSize a) (idxLnSize innerNode) ky in
      PT(lvx, a, ky, innerNode)
  | t -> t

let split = function
  | PT(lvx, a, kx, PT(lvy, b, ky, PT(lvz, c, kz, d))) when lvx = lvy && lvy = lvz ->
      let right = PT(lvx, c, kz, d) in
      let kx = setData (idxLnSize a) (idxLnSize b) kx in
      let left = PT(lvx, a, kx, b) in
      let ky = setData (idxLnSize left) (idxLnSize right) ky in
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
      let kl = setData (idxLnSize a) (idxLnSize lb) kl in
      let leftNode = PT(lv1, a, kl, lb) in
      let kt = setData (idxLnSize rb) (idxLnSize rt) kt in
      let rightNode = PT(lvt - 1, rb, kt, rt) in
      let kb = setData (idxLnSize leftNode) (idxLnSize rightNode) kb in
      PT(lvb + 1, leftNode, kb, rightNode)
  | PT(lvt, lt, kt, rt) when lvl rt < lvt -> 
      PT(lvt - 1, lt, kt, rt) |> split
  | PT(lvt, lt, kt, PT(_, (PT(lva, c, ka, d) as a), kr, b)) ->
      let kt = setData (idxLnSize lt) (idxLnSize c) kt in
      let leftNode = PT(lvt - 1, lt, kt, c) in
      let kr = setData (idxLnSize d) (idxLnSize b) kr in
      let rightNode = PT(nlvl a, d, kr, b) |> split in
      let ka = setData (idxLnSize leftNode) (idxLnSize rightNode) ka in
      PT(lva + 1, leftNode, ka, rightNode)
  | t -> t

let rec splitMax = function
  | PT(_, l, v, PE) ->
      let v' = setData (idxLnSize l) (0, 0) v in
      l, v'
  | PT(h, l, v, r) -> 
      let (r', b) = splitMax r in
      let (r'Size, r'Lines) = idxLnSize r' in
      let v' = {v with right_idx = r'Size; right_lns = r'Lines} in
      let newLeft = PT(h, l, v', r') in
      let (leftSize, leftLines) = idxLnSize newLeft in
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
let text piecerope = 
  fold (fun (acc: string) pc ->
    let text = Buffer.substring pc.start pc.length piecerope.buffer in
    acc ^ text
  ) "" piecerope.pieces

let insMin pcStart pcLength pcLines tree =
  let rec min node cont =
    match node with
    | PE -> PT(1, PE, create pcStart pcLength pcLines, PE) |> cont
    | PT(h, l, v, r) ->
        min l (fun l' ->
          let v' = addLeft pcLength (Array.length pcLines) v in
          PT(h, l', v', r) |> skew |> split |> cont
        )
  in
  min tree topLevelCont

let insMax pcStart pcLength (pcLines: int array) tree = 
  let rec max node cont =
    match node with
    | PE -> PT(1, PE, create pcStart pcLength pcLines, PE) |> cont
    | PT(h, l, v, r) ->
        max r (fun r' ->
          let v' = addRight pcLength (Array.length pcLines) v in
          PT(h, l, v', r') |> skew |> split |> cont
        )
  in
  max tree topLevelCont

let isConsecutive v pcStart = v.start + v.length = pcStart
