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

(* List of test suites to export. *)
let test_suite = 
  "Transaction_tests" >::: [
   "svelte_length" >:: test_svelte_rope_has_same_length_as_sum_of_txns;
   "rust_length" >:: test_rust_rope_has_same_length_as_sum_of_txns;
   "seph_length" >:: test_seph_rope_has_same_length_as_sum_of_txns;
   "automerge_length" >:: test_automerge_rope_has_same_length_as_sum_of_txns;
]

