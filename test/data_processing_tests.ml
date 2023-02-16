open Piecerope
open OUnit2
open Txns

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

let test_substrings rope total_length =
  let tenth_length = total_length / 10 in

  let pos = 0 in (* 1/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  let _ = assert_equal expected ropeSub in

  let pos = pos + tenth_length in (* 2/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  let _ = assert_equal expected ropeSub in

  let pos = pos + tenth_length in (* 3/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  let _ = assert_equal expected ropeSub in

  let pos = pos + tenth_length in (* 4/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  let _ = assert_equal expected ropeSub in

  let pos = pos + tenth_length in (* 5/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  let _ = assert_equal expected ropeSub in

  let pos = pos + tenth_length in (* 6/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  let _ = assert_equal expected ropeSub in

  let pos = pos + tenth_length in (* 7/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  let _ = assert_equal expected ropeSub in

  let pos = pos + tenth_length in (* 8/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  let _ = assert_equal expected ropeSub in

  let pos = pos + tenth_length in (* 9/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  let _ = assert_equal expected ropeSub in

  let pos = pos + tenth_length in (* 10/10 *)
  let ropeSub = Piece_rope.substring pos tenth_length rope in
  let expected = String.sub (Piece_rope.get_text rope) pos tenth_length in
  assert_equal expected ropeSub

let test_svelte_substrings _ =
  let (rope, total_length) = Utils.run_txns_result Sveltecomponent.data in
  test_substrings rope total_length

let test_rust_substrings _ =
  let (rope, total_length) = Utils.run_txns_result Rustcode.data in
  test_substrings rope total_length

let test_seph_substrings _ =
  let (rope, total_length) = Utils.run_txns_result Sephblog.data in
  test_substrings rope total_length

let test_automerge_substrings _ =
  let (rope, total_length) = Utils.run_txns_result Automerge.data in
  test_substrings rope total_length

(* List of test suites to export. *)
let test_suite = 
  "Transaction_tests" >::: [
   "svelte_length" >:: test_svelte_rope_has_same_length_as_sum_of_txns;
   "rust_length" >:: test_rust_rope_has_same_length_as_sum_of_txns;
   "seph_length" >:: test_seph_rope_has_same_length_as_sum_of_txns;
   "automerge_length" >:: test_automerge_rope_has_same_length_as_sum_of_txns;

   "svelte_substrings" >:: test_svelte_substrings ;
   "rust_substrings" >:: test_rust_substrings;
   "seph_substrings" >:: test_seph_substrings;
   "automerge_substrings" >:: test_automerge_substrings;
]

