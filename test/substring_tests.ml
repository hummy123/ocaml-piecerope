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
  let tableText = Piece_rope.insert 5 insText initialTable 
  |> Piece_rope.substring 5 (String.length insText) in
  assert_equal ~printer:print_text insText tableText

(* List of test suites to export. *)
let test_suite = 
  "Substring_tests" >::: [
    "test_can_get_a_substring_from_start_of_original_buffer " >:: test_can_get_a_substring_from_start_of_original_buffer ;
    "test_can_get_a_substring_from_the_whole_of_a_table's_add_buffer " >:: test_can_get_a_substring_from_the_whole_of_a_table's_add_buffer;

]
