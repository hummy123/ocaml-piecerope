type t =
    | BE
    | BT of int * t * int * string * int * t

let target_size = 512

let size = function
  | BE -> 0
  | BT(_, _, lm, v, rm, _) -> lm + String.length v + rm

let string_length = function
  | BE -> 0
  | BT(_, _, _, v, _, _) -> String.length v

let size_left = function
  | BE -> 0
  | BT(_, _, lm, _, _, _) -> lm

let size_right = function
  | BE -> 0
  | BT(_, _, _, _, rm, _) -> rm

(* AVL Tree-specific functions. *)
let ht = function
  | BE -> 0
  | BT(h, _, _, _, _, _) -> h

let mk l a r = 
  let h = (if ht l > ht r then ht l else ht r) + 1 in
  BT(h, l, (size l), a, (size r), r)

let balR a x bc =
  if ht bc = ht a + 2 then
    match bc with
    | BT(_, b, _, y, _, c) -> 
        if ht b <= ht c then
          mk (mk a x b) y c
        else
          (match b with
          | BT(_, b1, _, bx, _, b2) -> mk (mk a x b1) bx (mk b2 y c)
          | x -> x)
    | x -> x
  else
    mk a x bc

let top_level_cont x = x

let empty = BE

let append str buffer = 
  let rec ins_max node cont =
    match node with
    | BE -> BT(1, BE, 0, str, 0, BE) |> cont
    | BT(h, l, lm, v, rm, BE) when String.length v + String.length str <= target_size ->
        BT(h, l, lm, v ^ str, rm, BE) |> cont
    | BT(_, l, _, v, _, r) ->
        ins_max r (fun r' ->
          (balR l v r') |> cont
        )
  in
  ins_max buffer top_level_cont

let in_range start curIndex finish nodeEndIndex =
  start <= curIndex && finish >= nodeEndIndex

let start_is_in_range start curIndex finish nodeEndIndex =
  start <= curIndex && finish < nodeEndIndex && curIndex < finish

let end_is_in_range start curIndex finish nodeEndIndex =
  start > curIndex && finish >= nodeEndIndex && start <= nodeEndIndex

let middle_is_in_range start curIndex finish nodeEndIndex =
  start >= curIndex && finish <= nodeEndIndex

let substring start length buffer =
  let finish = start + length in
  let rec sub (curIndex: int) node (acc: string list) cont =
    match node with
    | BE -> acc |> cont
    | BT(_, l, _, v, _, r) when in_range start curIndex finish (curIndex + String.length v) ->
        let nodeEndIndex = curIndex + String.length v in

        sub (nodeEndIndex + size_left r) r acc (fun right ->
          let middle = v::right in
          sub (curIndex - string_length l - size_right l) l middle (fun x -> x |> cont)
        )

    | BT(_, l, _, v, _, _) when start_is_in_range start curIndex finish (curIndex + String.length v) ->
        let length = finish - curIndex in
        let acc = (String.sub v 0 length)::acc in
        sub (curIndex - string_length l - size_right l) l acc (fun x -> x |> cont)

    | BT(_, _, _, v, _, r) when end_is_in_range start curIndex finish (curIndex + String.length v) ->
        let strStart = start - curIndex in
        let len = String.length v - strStart in
        let acc = (String.sub v strStart len)::acc in
        sub (curIndex + String.length v + size_left r) r acc (fun x -> x |> cont)

    | BT(_, _, _, v, _, _) when middle_is_in_range start curIndex finish (curIndex + String.length v) ->
        let strStart = start - curIndex in
        (String.sub v strStart length)::acc |> cont

    | BT(_, l, _, _, _, _) when start < curIndex ->
        sub (curIndex - string_length l - size_right l) l acc (fun x -> x |> cont)

    | BT(_, _, _, v, _, r) when finish > curIndex + String.length v ->
        sub (curIndex + String.length v + size_left r) r acc (fun x -> x |> cont)

    | BT(_, _, _, _, _, _) -> 
        failwith "unreachable Buffer.substring case"
  in
  let strList = sub (size_left buffer) buffer [] top_level_cont in
  String.concat "" strList

