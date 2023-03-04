type t =
  | CE 
  | CT of int * t * int * Codepoint_converter.t * t

let ht = function
  | CE -> 0
  | CT(h, _, _, _, _) -> h

let mk l a b r = 
  let h = (if ht l > ht r then ht l else ht r) + 1 in
  CT(h, l, a, b, r)

let balL ab xk xv c =
  if ht ab = ht c + 2 then
    match ab with
    | CT(_, a, yk, yv, b) ->
        if ht a >= ht b then
          mk a yk yv (mk b xk xv c)
        else
          (match b with
           | CT(_, b1, bxk, bxv, b2) -> 
               mk (mk a yk yv b1) bxk bxv (mk b2 xk xv c)
           | _ -> mk ab xk xv c)
    | _ -> mk ab xk xv c
  else
    mk ab xk xv c

let balR a xk xv bc =
  if ht bc = ht a + 2 then
    match bc with
    | CT(_, b, yk, yv, c) -> 
        if ht b <= ht c then
          mk (mk a xk xv b) yk yv c
        else
          (match b with
          | CT(_, b1, bxk, bxv, b2) -> mk (mk a xk xv b1) bxk bxv (mk b2 yk yv c)
          | _ -> mk a xk xv bc)
    | _ -> mk a xk xv bc
  else
    mk a xk xv bc

let rec insert k v = function
  | CE -> mk CE k v CE
  | CT(_, nl, nk, nv, nr) ->
      if k < nk then
        balL (insert k v nl) nk nv nr
      else
        balR nl nk nv (insert k v nr)

let rec find k = function
  | CE -> Codepoint_converter.Any
  | CT(_, nl, nk, nv, nr) ->
      if k < nk then
        find k nl
      else if k > nk then
        find k nr
      else
        nv

