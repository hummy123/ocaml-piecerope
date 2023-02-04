let target_size = 512

type node = {
  left_idx: int;
  right_idx: int;
  value: string;
}

let create str = {left_idx = 0; right_idx = 0; value = str;}

type t = 
  | N0
  | N1 of t
  | N2 of t * node * t
  (* Auxillary *)
  | L2 of node
  | N3 of t * node * t * node * t

let empty = N0

let n1 = function
  | L2 a -> N2(N0, a, N0)
  | N3(t1, a1, t2, a2, t3) -> N2(N2(t1, a1, t2), a2, N1(t3))
  | t -> N1(t)

let n2 (x0, a2, t) = 
  match x0, a2, t with 
  | L2 a1, a2, t -> N3 (N0, a1, N0, a2, t)
  | N3 (t1, a1, t2, a2, t3), a3, N1 t4 ->
      N2 (N2 (t1, a1, t2), a2, N2 (t3, a3, t4))
  | N3 (t1, a1, t2, a2, t3), a3, t4 -> N3 (N2 (t1, a1, t2), a2, N1 t3, a3, t4)
  | t1, a1, L2 a2 -> N3 (t1, a1, N0, a2, N0)
  | N1 t1, a1, N3 (t2, a2, t3, a3, t4) ->
      N2 (N2 (t1, a1, t2), a2, N2 (t3, a3, t4))
  | t1, a1, N3 (t2, a2, t3, a3, t4) -> N3 (t1, a1, N1 t2, a2, N2 (t3, a3, t4))
  | t1, a1, t2 -> N2(t1, a1, t2)


let rec appendRec str = function
  | N0 -> L2(create str)
  | N1 t -> appendRec str t |> n1
  | N2(l, a, r) when String.length str + String.length a.value < target_size ->
      let a' = {a with value = a.value ^ str} in
      N2(l, a', r)
  | N2(l, a, r) ->
      let a' = {a with right_idx = a.right_idx + String.length str} in
      n2(l, a', appendRec str r)
  | _ -> failwith "unexpected Buffer.append"

let tree = function
  | L2 a -> N2(N0, a, N0)
  | N3(t1, a1, t2, a2, t3) -> N2(N2(t1, a1, t2), a2 ,(N1 t3))
  | t -> t

let append str node = appendRec str node |> tree

let rec size = function
  | N0 -> 0
  | N1 t -> size t
  | N2(_, a, _) -> a.left_idx + a.right_idx + String.length a.value
  | _ -> failwith "unexpected Buffer.size"

let rec string_length = function
  | N0 -> 0
  | N1 t -> string_length t
  | N2(_, a, _) -> String.length a.value
  | _ -> failwith "unexpected Buffer.string_length"

let rec size_left = function
  | N0 -> 0
  | N1 t -> size_left t
  | N2(_, a, _) -> a.left_idx
  | _ -> failwith "unexpected Buffer.size_left"

let rec size_right = function
  | N0 -> 0
  | N1 t -> size_right t
  | N2(_, a, _) -> a.right_idx
  | _ -> failwith "unexpected Buffer.size_right"

(* let top_level_cont x = x *)

let substring start length buffer =
  let finish = start + length in
  let rec sub (curIndex: int) node (acc: string) =
    match node with
    | N0 -> acc
    | N1 t -> sub curIndex t acc
    | N2(l, a, r) ->
        let left =
          if start < curIndex
          then sub (curIndex - string_length l - size_right l) l acc
          else acc 
        in
        let nextIndex = curIndex + String.length a.value in
        let middle = 
          if start <= curIndex && finish >= nextIndex then 
            (* Node is fully in range. *)
            left ^ a.value
          else if start >= curIndex && finish <= nextIndex then
            (* Range is within node. *)
            let strStart = start - curIndex in
            left ^ String.sub a.value strStart length
          else if finish < nextIndex && finish >= curIndex then
            (* Start of node is within range. *)
            let length = finish - curIndex in
            left ^ String.sub a.value 0 length
          else if start > curIndex && start <= nextIndex then
            (* End of node is within range. *)
            let strStart = start - curIndex in
            let len = String.length a.value - strStart - 1 in
            left ^ String.sub a.value strStart len
          else
            left
        in
        if finish > nextIndex
        then sub (nextIndex + size_left r) r middle
        else middle
    | _ -> failwith "unexpected Buffer.substring case"
  in
  sub (size_right buffer) buffer ""
