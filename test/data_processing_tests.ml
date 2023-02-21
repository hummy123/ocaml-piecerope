open Piecerope
open OUnit2
open Txns

let print_text x = "\n" ^ x ^ "\n"

let print_num (x: int) = string_of_int x

let test_svelte_rope_has_same_length_as_sum_of_txns _ =
  let (rope, count) = Utils.run_txns_result Sveltecomponent.data in
  let ropeText = Piece_rope.get_text rope in
  let _ = assert_equal count (Piece_rope.total_length rope) in
  assert_equal ~printer:string_of_int count (String.length ropeText)

let test_rust_rope_has_same_length_as_sum_of_txns _ =
  let (rope, count) = Utils.run_txns_result Rustcode.data in
  let ropeText = Piece_rope.get_text rope in
  let _ = assert_equal count (Piece_rope.total_length rope) in
  assert_equal ~printer:string_of_int count (String.length ropeText)

let test_seph_rope_has_same_length_as_sum_of_txns _ =
  let (rope, count) = Utils.run_txns_result Sephblog.data in
  let ropeText = Piece_rope.get_text rope in
  let _ = assert_equal count (Piece_rope.total_length rope) in
  assert_equal ~printer:string_of_int count (String.length ropeText)

let test_automerge_rope_has_same_length_as_sum_of_txns _ =
  let (rope, count) = Utils.run_txns_result Automerge.data in
  let ropeText = Piece_rope.get_text rope in
  let _ = assert_equal count (Piece_rope.total_length rope) in
  assert_equal ~printer:string_of_int count (String.length ropeText)

let test_whole_substrings rope count =
  let wholeText = Piece_rope.get_text rope in
  let substringText = Piece_rope.substring 0 count rope in
  assert_equal wholeText substringText

let test_whole_svelte_substring _ =
  let (rope, count) = Utils.run_txns_result Sveltecomponent.data in
  test_whole_substrings rope count

let test_whole_rust_substring _ =
  let (rope, count) = Utils.run_txns_result Rustcode.data in
  test_whole_substrings rope count

let test_whole_seph_substring _ =
  let (rope, count) = Utils.run_txns_result Sephblog.data in
  test_whole_substrings rope count

let test_whole_automerge_substring _ =
  let (rope, count) = Utils.run_txns_result Automerge.data in
  test_whole_substrings rope count

(* We are not using a loop for repetitive tests because we can get exact line numbers/stack traces with manual. *)
let test_substrings_by_tenths rope total_length =
  let tenth_length = total_length / 10 in

  let pos = 0 in (* 1/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let ropeText = Piece_rope.get_text rope in
  let expected = String.sub ropeText pos tenth_length in
  let _ = assert_equal ~printer:print_text expected ropeSub in

  let pos = pos + tenth_length in (* 2/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  let _ = assert_equal ~printer:print_text expected ropeSub in

  let pos = pos + tenth_length in (* 3/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  let _ = assert_equal ~printer:print_text expected ropeSub in

  let pos = pos + tenth_length in (* 4/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  let _ = assert_equal ~printer:print_text expected ropeSub in

  let pos = pos + tenth_length in (* 5/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  let _ = assert_equal ~printer:print_text expected ropeSub in

  let pos = pos + tenth_length in (* 6/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  let _ = assert_equal ~printer:print_text expected ropeSub in

  let pos = pos + tenth_length in (* 7/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  let _ = assert_equal ~printer:print_text expected ropeSub in

  let pos = pos + tenth_length in (* 8/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  let _ = assert_equal ~printer:print_text expected ropeSub in

  let pos = pos + tenth_length in (* 9/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  let _ = assert_equal ~printer:print_text expected ropeSub in

  let pos = pos + tenth_length in (* 10/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  assert_equal ~printer:print_text expected ropeSub

let test_svelte_substrings _ =
  let (rope, total_length) = Utils.run_txns_result Sveltecomponent.data in
  test_substrings_by_tenths rope total_length

let test_rust_substrings _ =
  let (rope, total_length) = Utils.run_txns_result Rustcode.data in
  test_substrings_by_tenths rope total_length

let test_seph_substrings _ =
  let (rope, total_length) = Utils.run_txns_result Sephblog.data in
  test_substrings_by_tenths rope total_length

let test_automerge_substrings _ =
  let (rope, total_length) = Utils.run_txns_result Automerge.data in
  test_substrings_by_tenths rope total_length

let test_rope_lines rope =
  let totalLines = Piece_rope.total_lines rope - 1 in
  let totalLines = if totalLines > 0 then totalLines else 0 in
  let splitString = Piece_rope.get_text rope |> String.split_on_char '\n' in

  (* Remove \n from last line for comparison. *)
  let lastStr = List.nth splitString totalLines in
  let lastRopeLine = Piece_rope.get_line totalLines rope in
  let lastRopeLine = String.sub lastRopeLine 0 (String.length lastRopeLine - 1) in
  let _ = assert_equal ~printer:print_text lastStr lastRopeLine in

  let lineStartIdx = ref 0 in
  for i = 0 to totalLines do (* All except last line. *)
    let strLine = List.nth splitString i  ^ "\n" in
    let (ropeLine, ropeIdx) = Piece_rope.get_line_and_line_start_index i rope in
    let _ = assert_equal ~printer:print_text  strLine ropeLine in
    let _ = assert_equal ~printer:print_num !lineStartIdx ropeIdx in
    lineStartIdx := !lineStartIdx + String.length strLine 
  done

let test_svelte_lines _ =
  let (rope, _) = Utils.run_txns_result Sveltecomponent.data in
  test_rope_lines rope

let test_rust_lines _ =
  let (rope, _) = Utils.run_txns_result Rustcode.data in
  test_rope_lines rope

let test_seph_lines _ =
  let (rope, _) = Utils.run_txns_result Sephblog.data in
  test_rope_lines rope

let test_automerge_lines _ =
  let (rope, _) = Utils.run_txns_result Automerge.data in
  test_rope_lines rope

(* List of test suites to export. *)
let test_suite = 
  "Transaction_tests" >::: [
   "svelte_length" >:: test_svelte_rope_has_same_length_as_sum_of_txns;
   "rust_length" >:: test_rust_rope_has_same_length_as_sum_of_txns;
   "seph_length" >:: test_seph_rope_has_same_length_as_sum_of_txns;
   "automerge_length" >:: test_automerge_rope_has_same_length_as_sum_of_txns;

   "whole_svelte_substring" >:: test_whole_svelte_substring;
   "whole_rust_substring" >:: test_whole_rust_substring;
   "whole_seph_substring" >:: test_whole_seph_substring;
   "whole_automerge_substring" >:: test_whole_automerge_substring;

   "svelte_substrings" >:: test_svelte_substrings;
   "rust_substrings" >:: test_rust_substrings;
   "seph_substrings" >:: test_seph_substrings;
   "automerge_substrings" >:: test_automerge_substrings;

   "svelte_lines" >:: test_svelte_lines;
   "rust_lines" >:: test_rust_lines;
   "seph_lines" >:: test_seph_lines;
   "automerge_lines" >:: test_automerge_lines;
]

