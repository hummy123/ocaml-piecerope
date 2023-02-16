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

let skew = function
  | BT(lvx, BT(lvy, a, lky, ky, rky, b), _, kx, rkx, c) when lvx = lvy ->
    let innerNode =  BT(lvx, b, rky, kx, rkx, c) in
    BT(lvx, a, lky, ky, size innerNode, innerNode)
  | t -> t

let split = function
  | BT(lvx, a, lkx, kx, _, BT(lvy, b, lky, ky, _, BT(lvz, c, lkz, kz, rkz, d))) when lvx = lvy && lvy = lvz -> 
    let right = BT(lvx, c, lkz, kz, rkz, d) in
    let left = BT(lvx, a, lkx, kx, lky, b) in
    BT(lvx + 1, left, size left, ky, size right, right)
  | t -> t

let top_level_cont x = x

let empty = BE

let append str buffer = 
  let rec ins_max node cont =
    match node with
    | BE -> BT(1, BE, 0, str, 0, BE) |> cont
    | BT(h, l, lm, v, rm, BE) when String.length v + String.length str <= target_size ->
        BT(h, l, lm, v ^ str, rm, BE) |> cont
    | BT(h, l, lm, v, rm, r) ->
        ins_max r (fun r' ->
          BT(h, l, lm, v, rm + String.length str, r') |> skew |> split |> cont
        )
  in
  ins_max buffer top_level_cont

let in_range start curIndex finish nextIndex = start <= curIndex && finish >= nextIndex 

let range_in_node start curIndex finish nextIndex = start >= curIndex && finish <= nextIndex 

let start_in_range curIndex finish nextIndex = finish < nextIndex && finish >= curIndex 

let end_in_range start curIndex nextIndex = start > curIndex && start <= nextIndex 

let substring start length buffer =
  let finish = start + length in
  let rec sub (curIndex: int) node (acc: string list) cont =
    match node with
    | BE -> acc |> cont
    | BT(_, l, lm, _, _, _) when curIndex >= finish -> 
        if curIndex - lm > finish then
          acc |> cont
        else
          sub (curIndex - string_length l - size_right l) l acc (fun x -> x |> cont)

    | BT(_, _, _, v, rm, r) when curIndex + String.length v <= start ->
        if curIndex + rm < start then
          acc |> cont
        else
          sub (curIndex + String.length v + size_left r) r acc (fun x -> x |> cont)

    | BT(_, l, _, v, _, r) when in_range start curIndex finish (curIndex + String.length v) ->
        let nodeEndIndex = curIndex + String.length v in

        sub (nodeEndIndex + size_left r) r acc (fun right ->
          let middle = v::right in
          sub (curIndex - string_length l - size_right l) l middle (fun x -> x |> cont)
        )

    | BT(_, _, _, v, _, _) when range_in_node start curIndex finish (curIndex + String.length v) ->
        let strStart = start - curIndex in
        (String.sub v strStart length)::acc |> cont

    | BT(_, l, _, v, _, _) when start_in_range curIndex finish (curIndex + String.length v) ->
        let length = finish - curIndex in
        let acc = (String.sub v 0 length)::acc in
        sub (curIndex - string_length l - size_right l) l acc (fun x -> x |> cont)

    | BT(_, _, _, v, _, r) when end_in_range start curIndex (curIndex + String.length v) ->
        let strStart = start - curIndex in
        let len = String.length v - strStart - 1 in

        (* We "clip" len to 0 because there's a chance of failure otherwise
           where we are asked for a substring with length of -1.
           Unsure if the failure is because of an error elsewhere; should check. *)
        let len = if len > 0 then len else 0 in
        let acc = (String.sub v strStart len)::acc in
        sub (curIndex + String.length v + size_left r) r acc (fun x -> x |> cont)

    | BT(_, _, _, _, _, _) -> 
        acc
  in
  let strList = sub (size_left buffer) buffer [] top_level_cont in
  String.concat "" strList

