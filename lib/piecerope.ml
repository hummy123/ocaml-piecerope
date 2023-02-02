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

let setData leftSize leftLines rightSize rightLines node =
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
