open OUnit2

let () =
  run_test_tt_main (Insert_tests.test_suite)
