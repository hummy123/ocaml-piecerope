open OUnit2
open Piecerope

let print x = x

(* Get line after create tests. *)
let test_get_line_returns_inserted_line_after_create _ =
  let rope = Piece_rope.create "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n" in
  let num_list = [0;1;2;3;4;5;6;7;8;9] in
  let _ = List.fold_left (fun _ x -> 
    let expected = string_of_int x ^ "\n" in
    let result = Piece_rope.get_line x rope in
    assert_equal ~printer:print expected result |> ignore
  ) () num_list in
  let lastLine = Piece_rope.get_line 10 rope in
  assert_equal "" lastLine

(* Get line after insert tests. *)
let test_get_line_returns_line_when_we_insert_line_break_at_middle _ =
  let str = "abcdefghij" in
  let rope = Piece_rope.create str 
  |> Piece_rope.insert 4 "\n" in
  let str = "abcd\nefgij" in
  let str_lines = String.split_on_char '\n' str in
  let _ = assert_equal (List.nth str_lines 0 ^ "\n") (Piece_rope.get_line 0 rope) in
  assert_equal (List.nth str_lines 1) (Piece_rope.get_line 1 rope) |> ignore


