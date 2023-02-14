open OUnit2
open Piecerope

(* Constant data. *)
let text =
    "During the development of the .NET Framework, the class libraries were originally written using a managed code compiler system called \"Simple Managed C\" (SMC)."

let insText = "TEST!"

let initialTable = Piece_rope.create text

let print_text x = x

(* Tests. *)
let test_can_get_a_substring_from_start_of_original_buffer _ =
  let substring = Piece_rope.substring 0 2 initialTable in
  assert_equal ~printer:print_text "Du" substring

let test_can_get_a_substring_from_the_whole_of_a_table's_add_buffer _ =
  let substring = Piece_rope.insert 5 insText initialTable 
  |> Piece_rope.substring 5 (String.length insText) in
  assert_equal ~printer:print_text insText substring

let test_can_get_a_substring_from_around_a_table's_add_buffer _ =
  let substring = Piece_rope.insert 5 insText initialTable
  |> Piece_rope.substring 4 (String.length insText + 2) in
  assert_equal ~printer:print_text "nTEST!g" substring

let test_can_get_a_substring_from_the_end_of_original_buffer _ =
  let ropeSub = Piece_rope.substring (String.length text - 5) 5 initialTable in
  let stringSub = String.sub text (String.length text - 5) 5 in
  assert_equal ~printer:print_text stringSub ropeSub

let test_can_get_a_substring_from_end_of_add_buffer _ =
  let ropeSub = Piece_rope.insert 0 text Piece_rope.empty 
  |> Piece_rope.substring (String.length text - 5) 5 in
  let stringSub = String.sub text (String.length text - 5) 5 in
  assert_equal ~printer:print_text stringSub ropeSub

let test_can_get_substring_from_middle_of_original_buffer _ =
  let ropeSub = Piece_rope.substring 1 1 initialTable in
  let expected = "u" in
  assert_equal ~printer:print_text expected ropeSub

let test_can_get_substring_from_middle_of_add_buffer _ =
  let ropeSub = Piece_rope.insert 1 "abc" initialTable 
  |> Piece_rope.substring 2 1 in
  let expected = "b" in 
  assert_equal ~printer:print_text expected ropeSub

let test_can_get_substring_from_start_when_zipper_is_at_end _ =
  let ropeSub = Piece_rope.insert (String.length text) insText initialTable
  |> Piece_rope.substring 0 10 in
  let expected = String.sub text 0 10 in
  assert_equal ~printer:print_text expected ropeSub

let test_can_get_substring_from_middle_when_zipper_is_at_end _ =
  let ropeSub = Piece_rope.insert (String.length text) insText initialTable
  |> Piece_rope.substring 20 10 in
  let expected = String.sub text 20 10 in
  assert_equal ~printer:print_text expected ropeSub

let test_can_get_substring_from_end_when_zipper_is_at_end _ =
  let ropeSub = Piece_rope.insert (String.length text) insText initialTable
  |> Piece_rope.substring (String.length text) (String.length insText) in
  let expected = insText in
  assert_equal ~printer:print_text expected ropeSub

(* List of test suites to export. *)
let test_suite = 
  "Substring_tests" >::: [
    "test_can_get_a_substring_from_start_of_original_buffer " >:: test_can_get_a_substring_from_start_of_original_buffer ;
    "test_can_get_a_substring_from_the_whole_of_a_table's_add_buffer " >:: test_can_get_a_substring_from_the_whole_of_a_table's_add_buffer;
    "test_can_get_a_substring_from_around_a_table's_add_buffer" >:: test_can_get_a_substring_from_around_a_table's_add_buffer ;
    "test_can_get_a_substring_from_the_end_of_original_buffer " >:: test_can_get_a_substring_from_the_end_of_original_buffer ;
    "test_can_get_a_substring_from_end_of_add_buffer" >:: test_can_get_a_substring_from_end_of_add_buffer;
    "test_can_get_substring_from_middle_of_original_buffer " >:: test_can_get_substring_from_middle_of_original_buffer ;
    "test_can_get_substring_from_middle_of_add_buffer " >:: test_can_get_substring_from_middle_of_add_buffer ;
    "test_can_get_substring_from_start_when_zipper_is_at_end " >:: test_can_get_substring_from_start_when_zipper_is_at_end ;
    "test_can_get_substring_from_middle_when_zipper_is_at_end ">:: test_can_get_substring_from_middle_when_zipper_is_at_end ;
    "test_can_get_substring_from_end_when_zipper_is_at_end " >:: test_can_get_substring_from_end_when_zipper_is_at_end ;

]
