open OUnit2

let () =
  let _ = run_test_tt_main (Insert_tests.test_suite) in
  run_test_tt_main Delete_tests.test_suite
