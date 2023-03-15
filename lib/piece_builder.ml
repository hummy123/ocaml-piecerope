open Piece_types

let top_level_cont x = x

type wb_tree = WE | WT of node * int * wb_tree * wb_tree

let weight = 4
let size = function WE -> 0 | WT (_, count, _, _) -> count
let n_con v l r = WT (v, 1 + size l + size r, l, r)

let single_l a x r =
  match r with
  | WT (b, _, y, z) -> n_con b (n_con a x y) z
  | _ -> failwith "unexpected single_l"

let double_l a x r =
  match r with
  | WT (c, _, WT (b, _, y1, y2), z) -> n_con b (n_con a x y1) (n_con c y2 z)
  | _ -> failwith "unexpected double_l"

let single_r b l z =
  match l with
  | WT (a, _, x, y) -> n_con a x (n_con b y z)
  | _ -> failwith "unexpected single_r"

let double_r c l z =
  match l with
  | WT (a, _, x, WT (b, _, y1, y2)) -> n_con b (n_con a x y1) (n_con c y2 z)
  | _ -> failwith "unexpected double_r"

let t_con v l r =
  let ln = size l in
  let rn = size r in
  if ln + rn < 2 then n_con v l r
  else if rn > weight * ln then
    match r with
    | WT (_, _, rl, rr) ->
        let rln = size rl in
        let rrn = size rr in
        if rln < rrn then single_l v l r else double_l v l r
    | WE -> failwith "unexpected t_con"
  else if ln > weight * rn then
    match l with
    | WT (_, _, ll, lr) ->
        let lln = size ll in
        let lrn = size lr in
        if lrn < lln then single_r v l r else double_r v l r
    | WE -> failwith "unexpected t_con"
  else n_con v l r

let rec ins_max x = function
  | WE -> WT (x, 1, WE, WE)
  | WT (v, _, l, r) -> t_con v l (ins_max x r)

let from_tree pt = Piece_tree.fold (fun acc node -> ins_max node acc) WE pt
