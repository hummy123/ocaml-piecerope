(* Tests. *)
let of_string_returns_string () =
  let result = Piece_rope.get_text Constants.initial in
  Alcotest.(check string) "of_string" result Constants.text

let can_prepend () =
  let rope = Piece_rope.prepend Constants.insText Constants.initial in
  let result = Piece_rope.get_text rope in
  let expected = Constants.insText ^ Constants.text in
  Alcotest.(check string) "can_prepend" expected result

let can_append () =
  let rope = Piece_rope.append Constants.insText Constants.initial in
  let result = Piece_rope.get_text rope in
  let expected = Constants.text ^ Constants.insText in
  Alcotest.(check string) "can_append" expected result

let fold_match_indices_executes_on_every_match () =
  let rope = Piece_rope.of_string "a a a a" in
  let count_of_a =
    Piece_rope.fold_match_indices "a" rope 0 (fun acc _ -> acc + 1)
  in
  let expected = 4 in
  Alcotest.(check int) "fold_match_indices" expected count_of_a

let find_and_replace_replaces_strings_with_same_length () =
  let start = "a bc b bc c bc d b" in
  let rope = Piece_rope.of_string start in
  let rope = Piece_rope.find_and_replace "bc" "00" rope in
  let result = Piece_rope.get_text rope in
  let expected = "a 00 b 00 c 00 d b" in
  Alcotest.(check string) "find_and_replace same length" expected result

let find_and_replace_when_replace_string_is_shorter () =
  let start = "a bc b bc c bc d b" in
  let rope = Piece_rope.of_string start in
  let rope = Piece_rope.find_and_replace "bc" "0" rope in
  let result = Piece_rope.get_text rope in
  let expected = "a 0 b 0 c 0 d b" in
  Alcotest.(check string) "replace string is shorter" expected result

let find_and_replace_when_replace_string_is_longer () =
  let start = "a bc b bc c bc d b" in
  let rope = Piece_rope.of_string start in
  let rope = Piece_rope.find_and_replace "bc" "000" rope in
  let result = Piece_rope.get_text rope in
  let expected = "a 000 b 000 c 000 d b" in
  Alcotest.(check string) "replace string is longer" expected result

let () =
  Alcotest.run "PieceRope"
    [
      ( "of_string",
        [ Alcotest.test_case "Of string" `Quick of_string_returns_string ] );
      ("can_prepend", [ Alcotest.test_case "Can prepend" `Quick can_prepend ]);
      ("can_append", [ Alcotest.test_case "Can append" `Quick can_append ]);
      ( "fold_match_indices",
        [
          Alcotest.test_case "Can fold all indices" `Quick
            fold_match_indices_executes_on_every_match;
        ] );
      ( "fund_and_replace",
        [
          Alcotest.test_case "can replace with same length" `Quick
            find_and_replace_replaces_strings_with_same_length;
          Alcotest.test_case "can replace with shorter" `Quick
            find_and_replace_when_replace_string_is_shorter;
          Alcotest.test_case "can replace with longer" `Quick
            find_and_replace_when_replace_string_is_longer;
        ] );
    ]
