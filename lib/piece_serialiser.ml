open Piece_types
open Json_types_t

(* We are using a weight-balanced tree to hold the pieces because its rank and select functions
   let the index work as a unique ID for each json_piece. *)
type wb_tree = WE | WT of json_piece * int * wb_tree * wb_tree

let top_level_cont x = x

let fold_back f x t =
  let rec fld x t cont =
    match t with
    | WE -> cont x
    | WT (v, _, l, r) ->
        fld x r (fun x ->
            let x = f x v in
            fld x l (fun x -> cont x))
  in
  fld x t top_level_cont

let weight = 4
let ratio = 2
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

let rec add x = function
  | WE -> WT (x, 1, WE, WE)
  | WT (v, _, l, r) as tree ->
      if x < v then t_con v (add x l) r
      else if x > v then t_con v l (add x r)
      else tree

let rank x tree =
  let rec rnk acc = function
    | WE -> failwith "piece_serialiser.rank: element not found"
    | WT (v, _, l, r) ->
        if x < v then rnk acc l
        else if x > v then rnk (acc + size l + 1) r
        else acc + size l
  in
  rnk 0 tree

let rec fold state folder = function
  | WE -> state
  | WT (v, _, l, r) ->
      let state = fold state folder l in
      let state = folder state v in
      fold state folder r

let convert_to_json_doc (piecerope : piece_rope) : json_doc =
  (* Helper functions. *)
  let build_json_tree_from_piece_tree pc_tree json_tree =
    Piece_tree.fold
      (fun acc node ->
        let piece = { start = node.start; length = node.utf32_length } in
        add piece acc)
      json_tree pc_tree
  in
  let build_json_tree_from_stack tree_list json_tree =
    List.fold_left
      (fun acc_json_tree pc_tree ->
        build_json_tree_from_piece_tree pc_tree acc_json_tree)
      json_tree tree_list
  in
  let piece_tree_to_json_list pc_tree json_tree =
    Piece_tree.fold_back
      (fun lst node ->
        let piece = { start = node.start; length = node.utf32_length } in
        let piece_index = rank piece json_tree in
        piece_index :: lst)
      [] pc_tree
  in
  let stack_to_json_list stack json_tree =
    List.fold_left
      (fun acc_list pc_tree ->
        let cur_list = piece_tree_to_json_list pc_tree json_tree in
        cur_list :: acc_list)
      [] stack
  in
  (* Calling helper functions to build wb_tree containing json_pieces. *)
  let json_tree =
    build_json_tree_from_piece_tree piecerope.pieces WE
    |> build_json_tree_from_stack piecerope.undo
    |> build_json_tree_from_stack piecerope.redo
  in

  (* wb_tree of json_pieces to json_piece list for serialisation. *)
  let json_pieces =
    fold_back (fun acc json_piece -> json_piece :: acc) [] json_tree
  in

  (* Convert piece_tree, undo_stack and redo_stack to formats suitable to serialise. *)
  let current_tree_json = piece_tree_to_json_list piecerope.pieces json_tree in
  let undo_json = stack_to_json_list piecerope.undo json_tree in
  let redo_json = stack_to_json_list piecerope.redo json_tree in

  (* When undo and redo stacks are created, build them as well. *)
  let buffer_list =
    Piece_buffer.fold_back (fun acc str -> str :: acc) [] piecerope.buffer
  in
  {
    buffer = buffer_list;
    pieces = json_pieces;
    current = current_tree_json;
    undo = undo_json;
    redo = redo_json;
  }
