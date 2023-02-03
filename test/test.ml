open OUnit2

(* Constant data. *)
let text =
    "During the development of the .NET Framework, the class libraries were originally written using a managed code compiler system called \"Simple Managed C\" (SMC)."

let insText = "TEST!"

let initialTable = Piecerope.create text 

let print_text x = x

(* Tests. *)
let test_initial_table's_text_return_input_text _ =
  assert_equal ~printer:print_text text (Piecerope.get_text initialTable)

let test_can_insert_into_the_start_of_a_table's_original_buffer _ =
  let table = Piecerope.insert 0 insText initialTable in
  assert_equal ~printer:print_text (insText ^ text) (Piecerope.get_text table)

let test_can_insert_into_the_middle_of_a_table's_original_buffer _ =
  let table = Piecerope.insert 3 insText initialTable in
  let firstStr = String.sub text 0 3 in
  let thirdStr = String.sub text 3 (String.length text - 3) in
  let str = firstStr ^ insText ^ thirdStr in
  assert_equal ~printer:print_text str (Piecerope.get_text table)
  
let test_can_insert_into_the_end_of_a_table's_original_buffer _ =
  let table = Piecerope.insert (String.length text) insText initialTable in
  let str = text ^ insText in
  assert_equal ~printer:print_text str (Piecerope.get_text table)

let test_can_insert_into_the_start_of_a_table's_add_buffer _ =
  let tableText = Piecerope.empty
  |> Piecerope.insert 0 text
  |> Piecerope.insert 0 insText
  |> Piecerope.get_text in
  assert_equal ~printer:print_text (insText ^ text) tableText

let test_can_insert_into_the_middle_of_a_table's_add_buffer _ =
  let table = Piecerope.empty 
  |> Piecerope.insert 0 text
  |> Piecerope.insert 3 insText in
  let firstStr = String.sub text 0 3 in
  let thirdStr = String.sub text 3 (String.length text - 3) in
  let str = firstStr ^ insText ^ thirdStr in
  assert_equal ~printer:print_text str (Piecerope.get_text table)

let test_can_insert_into_the_end_of_a_table's_add_buffer _ =
  Piecerope.empty 
  |> Piecerope.insert 0 text
  |> Piecerope.insert (String.length text) insText
  |> Piecerope.get_text
  |> assert_equal ~printer:print_text (text ^ insText)

let test_can_continuously_insert_at_start _ =
  let rec test loops runningStr table =
    if loops > 10 then
      ()
    else
      let table = Piecerope.insert 0 "hello" table in
      let run = "hello" ^ runningStr in
      let _ = assert_equal ~printer:print_text run (Piecerope.get_text table) in
      test (loops + 1) run table
  in
  test 0 "" Piecerope.empty

(* List of test suites to export. *)
let insert_test_list = 
  "Insert_Tests" >::: [
    "test_initial_table's_text_return_input_text" >:: test_initial_table's_text_return_input_text;

    "test_can_insert_into_the_start_of_a_table's_original_buffer" >:: test_can_insert_into_the_start_of_a_table's_original_buffer;
    "test_can_insert_into_the_middle_of_a_table's_original_buffer" >:: test_can_insert_into_the_middle_of_a_table's_original_buffer;
    "test_can_insert_into_the_end_of_a_table's_original_buffer" >:: test_can_insert_into_the_end_of_a_table's_original_buffer;

    "test_can_insert_into_the_start_of_a_table's_add_buffer" >:: test_can_insert_into_the_start_of_a_table's_add_buffer;
    "test_can_insert_into_the_middle_of_a_table's_add_buffer" >:: test_can_insert_into_the_middle_of_a_table's_add_buffer;
    "test_can_insert_into_the_end_of_a_table's_add_buffer" >:: test_can_insert_into_the_end_of_a_table's_add_buffer;

    "test_can_continuously_insert_at_start " >:: test_can_continuously_insert_at_start;
]

let () =
  run_test_tt_main insert_test_list
