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
  assert_equal ~printer:print "" lastLine

(* Get line after insert tests. *)
let test_get_line_returns_line_when_we_insert_line_break_at_middle _ =
  let str = "abcdefghij" in
  let rope = Piece_rope.create str 
  |> Piece_rope.insert 4 "\n" in
  let str = "abcd\nefghij" in
  let str_lines = String.split_on_char '\n' str in
  let _ = assert_equal ~printer:print (List.nth str_lines 0 ^ "\n") (Piece_rope.get_line 0 rope) in
  assert_equal ~printer:print (List.nth str_lines 1) (Piece_rope.get_line 1 rope) |> ignore

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
    let _ = assert_equal ~printer:print last_split_line last_rope_line in

    (* Test all lines before last are same. *)
    for i = 0 to List.length split_string - 2 do
      let cur_split_string = List.nth split_string i ^ "\n" in
      let cur_rope_line = Piece_rope.get_line i testRope in
      assert_equal ~printer:print cur_split_string cur_rope_line
    done

(* Get line under delete tests. *)
let test_get_line_returns_whole_string_when_we_delete_line_break_in_middle _ =
  let initString = "abcde\nfghij" in
  let initRope = Piece_rope.create initString in
  
  let testRope = Piece_rope.delete 5 1 initRope in
  
  let expected = "acbdefghij" in 
  let line = Piece_rope.get_line 0 testRope in
  assert_equal ~printer:print expected line

let test_get_line_returns_correct_segments_when_we_delete_line_breaks_in_complex_string _ =
  let initString = 
    "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"
  in
  let rope = Piece_rope.create initString in

  let rope = Piece_rope.delete 11 1 rope in
  let line1 = Piece_rope.get_line 0 rope in
  let line2 = Piece_rope.get_line 1 rope in
  let _ = assert_equal ~printer:print "Lorem ipsumdolor sit amet,\n" line1 in
  let _ = assert_equal ~printer:print "consectetur\n" line2 in

  let rope = Piece_rope.delete 38 1 rope in
  let line1 = Piece_rope.get_line 0 rope in
  let line2 = Piece_rope.get_line 1 rope in
  let _ = assert_equal ~printer:print "Lorem ipsumdolor sit amet,\n" line1 in
  let _ = assert_equal ~printer:print "consecteturadipiscing elit. \n" line2 in 

  let rope = Piece_rope.delete 71 1 rope in
  let line1 = Piece_rope.get_line 0 rope in
  let line2 = Piece_rope.get_line 1 rope in
  let line3 = Piece_rope.get_line 2 rope in
  let _ = assert_equal ~printer:print "Lorem ipsumdolor sit amet,\n" line1 in
  let _ = assert_equal ~printer:print "consecteturadipiscing elit. \n" line2 in
  assert_equal ~printer:print "Aenean ornare, lacus vitae \n" line3

let test_get_line_returns_correct_segments_when_we_delete_line_breaks_in_first_half _ =
  let initString = 
    "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"
  in
  let rope = Piece_rope.create initString in

  let rope = Piece_rope.delete 11 17 rope in
  let line1 = Piece_rope.get_line 0 rope in
  let line2 = Piece_rope.get_line 1 rope in
  let _ = assert_equal ~printer:print "Lorem ipsumconsectetur\n" line1 in
  let _ = assert_equal ~printer:print "adipiscing elit. \n" line2 in

  let rope = Piece_rope.delete 57 29 rope in
  (* CHeck previous assertsions to see they still work. *)
  let line1 = Piece_rope.get_line 0 rope in
  let line2 = Piece_rope.get_line 1 rope in
  let _ = assert_equal ~printer:print "Lorem ipsumconsectetur\n" line1 in
  let _ = assert_equal ~printer:print "adipiscing elit. \n" line2 in

  (* Current assertions. *)
  let line3 = Piece_rope.get_line 2 rope in 
  let line4 = Piece_rope.get_line 3 rope in
  let _ = assert_equal ~printer:print "Aenean ornare, \n" line3 in
  let _ = assert_equal ~printer:print "leo nulla\n" line4 in

  (* Assertions for subsequent lines to check they still work *)
  let line5 = Piece_rope.get_line 4 rope in
  let line6 = Piece_rope.get_line 5 rope in
  let line7 = Piece_rope.get_line 6 rope in
  let line8 = Piece_rope.get_line 7 rope in
  let line9 = Piece_rope.get_line 8 rope in

  let _ = assert_equal ~printer:print "sollicitudin elit,\n" line5 in
  let _ = assert_equal ~printer:print "in ultrices mi dui et\n" line6 in
  let _ = assert_equal ~printer:print "ipsum. Cras condimentum\n" line7 in
  let _ = assert_equal ~printer:print "purus in metus \n" line8 in
  assert_equal ~printer:print "sodales tincidunt. Praesent" line9

let test_get_line_returns_correct_segments_when_we_delete_line_breaks_in_last_half _ =
  let initString = 
    "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"
  in
  let rope = Piece_rope.create initString in

  let rope = Piece_rope.delete 177 17 rope in
  let line11 = Piece_rope.get_line 10 rope in
  let _ = assert_equal ~printer:print "ipsum. Cras condimentumsodales tincidunt. Praesent" line11 in

  let rope = Piece_rope.delete 123 9 rope in
  let line9 = Piece_rope.get_line 8 rope in
  let _ = assert_equal ~printer:print "sollicitudin ultrices mi dui et\n" line9 in

  let rope = Piece_rope.delete 83 25 rope in
  let line1 = Piece_rope.get_line 0 rope in
  let line2 = Piece_rope.get_line 1 rope in
  let line3 = Piece_rope.get_line 2 rope in
  let line4 = Piece_rope.get_line 3 rope in
  let line5 = Piece_rope.get_line 4 rope in
  let line6 = Piece_rope.get_line 5 rope in
  let line7 = Piece_rope.get_line 6 rope in
  let line8 = Piece_rope.get_line 7 rope in

  let _ = assert_equal ~printer:print "Lorem ipsum\n" line1 in
  let _ = assert_equal ~printer:print "dolor sit amet,\n" line2 in
  let _ = assert_equal ~printer:print "consectetur\n" line3 in
  let _ = assert_equal ~printer:print "adipiscing elit. \n" line4 in
  let _ = assert_equal ~printer:print "Aenean ornare, \n" line5 in
  let _ = assert_equal ~printer:print "lacus vitulla\n" line6 in
  let _ = assert_equal ~printer:print "sollicitudin ultrices mi dui et\n" line7 in
  assert_equal ~printer:print "ipsum. Cras condimentumsodales tincidunt. Praesent" line8

let test_get_line_returns_correct_segments_when_we_delete_multiple_line_breaks_in_middle _ =
  let initString = 
    "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"
  in
  let rope = Piece_rope.create initString in

  let rope = Piece_rope.delete 58 18 rope in
  let rope = Piece_rope.delete 71 26 rope in
  let rope = Piece_rope.delete 12 16 rope in

  let line1 = Piece_rope.get_line 0 rope in
  let line2 = Piece_rope.get_line 1 rope in
  let line3 = Piece_rope.get_line 2 rope in
  let line4 = Piece_rope.get_line 3 rope in
  let line5 = Piece_rope.get_line 4 rope in
  let line6 = Piece_rope.get_line 5 rope in
  let line7 = Piece_rope.get_line 6 rope in
  let line8 = Piece_rope.get_line 7 rope in
  let line9 = Piece_rope.get_line 8 rope in

  let _ = assert_equal ~printer:print "Lorem ipsum\n" line1 in
  let _ = assert_equal ~printer:print "consectetur\n" line2 in
  let _ = assert_equal ~printer:print "adipiscing elit. \n" line3 in
  let _ = assert_equal ~printer:print "lacus vitae \n" line4 in 
  let _ = assert_equal ~printer:print "sollicitudin elit,\n" line5 in
  let _ = assert_equal ~printer:print "in ultrices mi dui et\n" line6 in
  let _ = assert_equal ~printer:print "ipsum. Cras condimentum\n" line7 in
  let _ = assert_equal ~printer:print "purus in metus \n" line8 in
  assert_equal ~printer:print "sodales tincidunt. Praesent" line9

let test_suite = 
  "Get_line_tests" >::: [
    "test_get_line_returns_inserted_line_after_create " >:: test_get_line_returns_inserted_line_after_create;
    "test_get_line_returns_line_when_we_insert_line_break_at_middle" >:: test_get_line_returns_line_when_we_insert_line_break_at_middle ;
    "test_get_line_splits_correctly_when_we_insert_into_middle_of_piece" >:: test_get_line_splits_correctly_when_we_insert_into_middle_of_piece;
    "test_get_line_returns_whole_string_when_we_delete_line_break_in_middle" >:: test_get_line_returns_whole_string_when_we_delete_line_break_in_middle;
    (* "test_get_line_returns_correct_segments_when_we_delete_line_breaks_in_complex_string" >:: test_get_line_returns_correct_segments_when_we_delete_line_breaks_in_complex_string ; *)
    (* "test_get_line_returns_correct_segments_when_we_delete_line_breaks_in_first_half" >:: test_get_line_returns_correct_segments_when_we_delete_line_breaks_in_first_half ; *)
    (* "test_get_line_returns_correct_segments_when_we_delete_line_breaks_in_last_half " >:: test_get_line_returns_correct_segments_when_we_delete_line_breaks_in_last_half ; *)
    (* "test_get_line_returns_correct_segments_when_we_delete_multiple_line_breaks_in_middle " >:: test_get_line_returns_correct_segments_when_we_delete_multiple_line_breaks_in_middle ; *)
  ]
