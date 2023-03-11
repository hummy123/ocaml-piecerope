open OUnit2
open Piecerope

(* Constant data. *)
let text =
  "During the development of the .NET Framework, the class libraries were \
   originally written using a managed code compiler system called \"Simple \
   Managed C\" (SMC)."

let insText = "TEST!"
let initialTable = Piece_rope.create text
let print_text x = x

(* Tests. *)
let test_initial_table's_text_return_input_text _ =
  assert_equal ~printer:print_text text (Piece_rope.get_text initialTable)

let test_can_insert_into_the_start_of_a_table's_original_buffer _ =
  let table = Piece_rope.insert 0 insText initialTable in
  assert_equal ~printer:print_text (insText ^ text) (Piece_rope.get_text table)

let test_can_insert_into_the_middle_of_a_table's_original_buffer _ =
  let table = Piece_rope.insert 3 insText initialTable in
  let firstStr = String.sub text 0 3 in
  let thirdStr = String.sub text 3 (String.length text - 3) in
  let str = firstStr ^ insText ^ thirdStr in
  assert_equal ~printer:print_text str (Piece_rope.get_text table)

let test_can_insert_into_the_end_of_a_table's_original_buffer _ =
  let table = Piece_rope.insert (String.length text) insText initialTable in
  let str = text ^ insText in
  assert_equal ~printer:print_text str (Piece_rope.get_text table)

let test_can_insert_into_the_start_of_a_table's_add_buffer _ =
  let tableText =
    Piece_rope.empty |> Piece_rope.insert 0 text
    |> Piece_rope.insert 0 insText
    |> Piece_rope.get_text
  in
  assert_equal ~printer:print_text (insText ^ text) tableText

let test_can_insert_into_the_middle_of_a_table's_add_buffer _ =
  let table =
    Piece_rope.empty |> Piece_rope.insert 0 text |> Piece_rope.insert 3 insText
  in
  let firstStr = String.sub text 0 3 in
  let thirdStr = String.sub text 3 (String.length text - 3) in
  let str = firstStr ^ insText ^ thirdStr in
  assert_equal ~printer:print_text str (Piece_rope.get_text table)

let test_can_insert_into_the_end_of_a_table's_add_buffer _ =
  Piece_rope.empty |> Piece_rope.insert 0 text
  |> Piece_rope.insert (String.length text) insText
  |> Piece_rope.get_text
  |> assert_equal ~printer:print_text (text ^ insText)

let test_can_continuously_insert_at_start _ =
  let rec test loops runningStr table =
    if loops > 10 then ()
    else
      let table = Piece_rope.insert 0 "hello" table in
      let run = "hello" ^ runningStr in
      let _ =
        assert_equal ~printer:print_text run (Piece_rope.get_text table)
      in
      test (loops + 1) run table
  in
  test 0 "" Piece_rope.empty

let test_can_continuously_insert_at_middle _ =
  let rec test loops runningStr table =
    if loops > 10 then ()
    else
      let halfLength = String.length runningStr / 2 in
      let table = Piece_rope.insert halfLength "hello" table in
      let run =
        String.sub runningStr 0 halfLength
        ^ "hello"
        ^ String.sub runningStr halfLength
            (String.length runningStr - halfLength)
      in
      let tableText = Piece_rope.get_text table in
      let _ = assert_equal run tableText in
      test (loops + 1) run table
  in
  test 0 "" Piece_rope.empty

let test_can_continuously_insert_at_end _ =
  let rec test loops runningStr table =
    if loops > 10 then ()
    else
      let pos = String.length runningStr in
      let table = Piece_rope.insert pos "hello" table in
      let run = runningStr ^ "hello" in
      let tableText = Piece_rope.get_text table in
      let _ = assert_equal run tableText in
      test (loops + 1) run table
  in
  test 0 "" Piece_rope.empty

(* List of test suites to export. *)
let test_suite =
  "Insert_Tests"
  >::: [
         "test_initial_table's_text_return_input_text"
         >:: test_initial_table's_text_return_input_text;
         "test_can_insert_into_the_start_of_a_table's_original_buffer"
         >:: test_can_insert_into_the_start_of_a_table's_original_buffer;
         "test_can_insert_into_the_middle_of_a_table's_original_buffer"
         >:: test_can_insert_into_the_middle_of_a_table's_original_buffer;
         "test_can_insert_into_the_end_of_a_table's_original_buffer"
         >:: test_can_insert_into_the_end_of_a_table's_original_buffer;
         "test_can_insert_into_the_start_of_a_table's_add_buffer"
         >:: test_can_insert_into_the_start_of_a_table's_add_buffer;
         "test_can_insert_into_the_middle_of_a_table's_add_buffer"
         >:: test_can_insert_into_the_middle_of_a_table's_add_buffer;
         "test_can_insert_into_the_end_of_a_table's_add_buffer"
         >:: test_can_insert_into_the_end_of_a_table's_add_buffer;
         "test_can_continuously_insert_at_start"
         >:: test_can_continuously_insert_at_start;
         "test_can_continuously_insert_at_middle"
         >:: test_can_continuously_insert_at_middle;
         "test_can_continuously_insert_at_end"
         >:: test_can_continuously_insert_at_end;
       ]
