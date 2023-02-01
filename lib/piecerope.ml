open Buffer

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

(* Logic for handling piece nodes. *)
let splitLines rStart (lines: int array) =
  let rec findIndex pos =
    if pos >= Array.length lines then
      None
    else
      let cur = Array.get lines pos in
      if cur >= rStart then
        Some pos
      else
        findIndex (pos + 1)
  in
  match findIndex 0 with
  | Some splitPoint ->
      let arrLeft = Array.sub lines 0 (splitPoint - 1) in
      let arrRight = Array.sub lines splitPoint (Array.length lines - splitPoint - 1) in
      arrLeft, arrRight
  | None ->
      lines, Array.make 0 0

