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

let test_get_line_splits_correctly_when_we_insert_into_middle_of_piece _ =
    let initString = 
      "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"
    in
    let initRope = Piece_rope.create initString in

    let testRope = Piece_rope.insert 27 "\n" initRope
    |> Piece_rope.insert 207 "\n" in

    let full_string = Piece_rope.get_text testRope in
    let split_string = String.split_on_char '\n' full_string in

    (* Test last line is same first. *)
    let last_line_idx = List.length split_string - 1 in
    let last_split_line = List.nth split_string last_line_idx in
    let last_rope_line = Piece_rope.get_line last_line_idx testRope in
    let _ = assert_equal last_split_line last_rope_line in

    (* Test all lines before last are same. *)
    for i = 0 to List.length split_string - 2 do
      let cur_split_string = List.nth split_string i ^ "\n" in
      let cur_rope_line = Piece_rope.get_line i testRope in
      assert_equal cur_split_string cur_rope_line
    done

