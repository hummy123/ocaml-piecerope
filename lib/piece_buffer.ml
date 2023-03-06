type t =
    | BE
    | BT of int * t * int * string * int * t * int

let target_size = 1024

let size = function
  | BE -> 0
  | BT(_, _, lm, _, rm, _, length) -> lm + length + rm

let string_length = function
  | BE -> 0
  | BT(_, _, _, _, _, _, length) -> length

let size_left = function
  | BE -> 0
  | BT(_, _, lm, _, _, _, _) -> lm

let size_right = function
  | BE -> 0
  | BT(_, _, _, _, rm, _, _) -> rm

(* AVL Tree-specific functions. *)
let ht = function
  | BE -> 0
  | BT(h, _, _, _, _, _, _) -> h

let mk l a aLength r = 
  let h = (if ht l > ht r then ht l else ht r) + 1 in
  BT(h, l, (size l), a, (size r), r, aLength)

let balR a x xl bc =
  if ht bc = ht a + 2 then
    match bc with
    | BT(_, b, _, y, _, c, yl) -> 
        if ht b <= ht c then
          mk (mk a x xl b) y yl c
        else
          (match b with
          | BT(_, b1, _, bx, _, b2, bxl) -> mk (mk a x xl b1) bx bxl (mk b2 y yl c)
          | x -> x)
    | x -> x
  else
    mk a x xl bc

let top_level_cont x = x

let empty = BE

let append str strLength buffer = 
  let rec ins_max node cont =
    match node with
    | BE -> BT(1, BE, 0, str, 0, BE, strLength) |> cont
    | BT(h, l, lm, v, rm, BE, length) when String.length v + String.length str <= target_size ->
        BT(h, l, lm, v ^ str, rm, BE, length + strLength) |> cont
    | BT(_, l, _, v, _, r, length) ->
        ins_max r (fun r' ->
          (balR l v length r') |> cont
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
    | BT(_, l, _, v, _, r, vLen) when in_range start curIndex finish (curIndex + vLen) ->
        let nodeEndIndex = curIndex + vLen in

        sub (nodeEndIndex + size_left r) r acc (fun right ->
          let middle = v::right in
          sub (curIndex - string_length l - size_right l) l middle (fun x -> x |> cont)
        )

    | BT(_, l, _, v, _, _, vLen) when start_is_in_range start curIndex finish (curIndex + vLen) ->
        let length = finish - curIndex in

        let acc =
          if vLen = String.length v then
            (String.sub v 0 length)::acc 
          else
            (String_processor.codepointSub v 0 length)::acc 
        in

        sub (curIndex - string_length l - size_right l) l acc (fun x -> x |> cont)
    | BT(_, _, _, v, _, r, vLen) when end_is_in_range start curIndex finish (curIndex + vLen) ->
        let strStart = start - curIndex in
        let len = vLen - strStart in

        let nodeText =
          if vLen = String.length v then
            String.sub v strStart len
          else
            String_processor.codepointSub v strStart len
        in

        sub (curIndex + vLen + size_left r) r acc (fun x -> nodeText::x |> cont)
    | BT(_, _, _, v, _, _, vLen) when middle_is_in_range start curIndex finish (curIndex + vLen) ->
        let strStart = start - curIndex in
        if vLen = String.length v then
          [String.sub v strStart length] |> cont
        else
          [String_processor.codepointSub v strStart length] |> cont

    | BT(_, l, _, _, _, _, _) when start < curIndex ->
        sub (curIndex - string_length l - size_right l) l acc (fun x -> x |> cont)

    | BT(_, _, _, _, _, r, vLen) when finish > curIndex + vLen ->
        sub (curIndex + vLen + size_left r) r acc (fun x -> x |> cont)

    | _ ->
        failwith "unreachable Buffer.substring case"
  in
  let strList = sub (size_left buffer) buffer [] top_level_cont in
  String.concat "" strList

