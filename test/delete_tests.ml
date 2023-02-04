open OUnit2

(* Constant data. *)
let text =
    "During the development of the .NET Framework, the class libraries were originally written using a managed code compiler system called \"Simple Managed C\" (SMC)."

let insText = "TEST!"

let initialTable = Piecerope.create text

let print_text x = x

(* Tests. *)
let test_can_delete_from_start_of_original_buffer _ =
  let expected = String.sub text 2 (String.length text - 2) in
  let table = Piecerope.delete 0 2 initialTable in
  let tableText = Piecerope.get_text table in
  assert_equal ~printer:print_text expected tableText

let test_can_delete_from_middle_of_original_buffer _ =
  let expected = (String.make 1 (String.get text 0)) ^ String.sub text 2 (String.length text - 2) in
  let tableText = Piecerope.delete 1 1 initialTable 
  |> Piecerope.get_text in
  assert_equal ~printer:print_text expected tableText

let test_can_delete_from_end_of_original_buffer _ =
  let expected = String.sub text 0 (String.length text - 5) in
  let tableText = Piecerope.empty
  |> Piecerope.insert 0 text
  |> Piecerope.delete (String.length text - 5) 5
  |> Piecerope.get_text in
  assert_equal ~printer:print_text expected tableText

let test_can_delete_from_start_of_add_buffer _ =
  let expected = String.sub text 2 (String.length text - 2) in
  let tableText = Piecerope.empty
  |> Piecerope.insert 0 text
  |> Piecerope.delete 0 2
  |> Piecerope.get_text in
  assert_equal ~printer:print_text expected tableText

let test_can_delete_from_start_when_we_inserted_multiple_times _ =
  let expected = String.sub (insText ^ text) 10 (String.length (insText ^ text) - 10) in
  let tableText = Piecerope.insert 0 insText initialTable
  |> Piecerope.delete 0 10
  |> Piecerope.get_text in
  assert_equal ~printer:print_text expected tableText

let test_can_delete_from_start_after_we_insert_at_end _ =
  let expected = String.sub text 10 (String.length text - 10) ^ insText in
  let tableText = Piecerope.insert (String.length text) insText initialTable
  |> Piecerope.delete 0 10
  |> Piecerope.get_text in
  assert_equal ~printer:print_text expected tableText

let test_can_delete_from_middle_after_we_insert_at_end _ =
  let expected = String.make 1 (String.get text 0) ^ String.sub text 2 (String.length text - 2) ^ insText in
  let tableText = Piecerope.insert (String.length text) insText initialTable
  |> Piecerope.delete 1 1
  |> Piecerope.get_text in
  assert_equal ~printer:print_text expected tableText

let test_can_delete_at_end_after_we_insert_at_end _ =
  let expected = text in
  let tableText = Piecerope.insert (String.length text) insText initialTable
  |> Piecerope.delete (String.length text) (String.length insText)
  |> Piecerope.get_text in
  assert_equal ~printer:print_text expected tableText

let test_can_delete_at_start_after_we_insert_in_middle _ =
  let expected = String.sub text 5 (String.length text / 2 - String.length insText)
  ^ insText
  ^ String.sub text (String.length text / 2) (String.length text / 2 + 1) in
  let tableText = Piecerope.insert (String.length text / 2) insText initialTable
  |> Piecerope.delete 0 5
  |> Piecerope.get_text in
  assert_equal ~printer:print_text expected tableText

let test_can_delete_in_middle_after_we_insert_in_middle _ =
  let expected = text in
  let tableText = Piecerope.insert (String.length text / 2) insText initialTable
  |> Piecerope.delete (String.length text / 2) (String.length insText) 
  |> Piecerope.get_text in
  assert_equal ~printer:print_text expected tableText

let test_can_delete_at_end_after_we_insert_at_middle _ =
  let expected = String.sub text 0 (String.length text / 2) 
  ^ insText
  ^ String.sub text (String.length text / 2) (String.length text / 2 - String.length insText + 1) in
  let tableText = Piecerope.insert (String.length text / 2) insText initialTable
  |> Piecerope.delete (String.length text) (String.length insText)
  |> Piecerope.get_text in
  assert_equal ~printer:print_text expected tableText

let test_can_delete_around_place_we_inserted_at _ =
  let expected  = String.sub text 0 (String.length text / 2 - 1) 
  ^ String.sub text (String.length text / 2 + 1) (String.length text / 2) in
  let tableText = Piecerope.insert (String.length text / 2) insText initialTable
  |> Piecerope.delete  (String.length text / 2 - 1) (String.length insText + 2)
  |> Piecerope.get_text in
  assert_equal ~printer:print_text expected tableText

(* List of test suites to export. *)
let test_suite = 
  "Delete_tests" >::: [
    "test_can_delete_from_start_of_original_buffer" >:: test_can_delete_from_start_of_original_buffer;
    "test_can_delete_from_middle_of_original_buffer" >:: test_can_delete_from_middle_of_original_buffer;
    "test_can_delete_from_end_of_original_buffer" >:: test_can_delete_from_end_of_original_buffer;
    
    "test_can_delete_from_start_of_add_buffer" >:: test_can_delete_from_start_of_add_buffer;
    
    "test_can_delete_from_start_when_we_inserted_multiple_times" >:: test_can_delete_from_start_when_we_inserted_multiple_times;
    "test_can_delete_from_start_after_we_insert_at_end" >:: test_can_delete_from_start_after_we_insert_at_end;
    
    
    "test_can_delete_from_middle_after_we_insert_at_end" >:: test_can_delete_from_middle_after_we_insert_at_end;
    "test_can_delete_at_end_after_we_insert_at_end" >:: test_can_delete_at_end_after_we_insert_at_end;

    "test_can_delete_at_start_after_we_insert_in_middle" >:: test_can_delete_at_start_after_we_insert_in_middle;
    "test_can_delete_in_middle_after_we_insert_in_middle" >:: test_can_delete_in_middle_after_we_insert_in_middle;
    "test_can_delete_at_end_after_we_insert_at_middle" >:: test_can_delete_at_end_after_we_insert_at_middle;    
    "test_can_delete_around_place_we_inserted_at" >:: test_can_delete_around_place_we_inserted_at;
]

