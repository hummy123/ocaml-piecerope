(*
    I personally find Expecto (F#) and Jest (Javascript)
    to have syntactically cleaner testing styles with a better signal-to-noise ratio.
    The partial function below helps mimic that syntax.
*)
let test title f = Alcotest.test_case title `Quick f

let () =
  Alcotest.run "PieceRope"
    [
      ( "Piece_rope.of_string",
        [
          test "returns input string" (fun () ->
              let rope = Piece_rope.of_string Constants.text in
              let result = Piece_rope.get_text rope in
              let expected = Constants.text in
              Alcotest.(check string) "returns input string" expected result);
        ] );
      ( "Piece_rope.prepend",
        [
          test "returns rope with new string at start" (fun () ->
              let rope = Piece_rope.of_string Constants.text in
              let rope = Piece_rope.prepend Constants.insText rope in
              let result = Piece_rope.get_text rope in
              let expected = Constants.insText ^ Constants.text in
              Alcotest.(check string)
                "returns rope with new string at start" expected result);
        ] );
      ( "Piece_rope.append",
        [
          test "returns rope with new string at end" (fun () ->
              let rope = Piece_rope.of_string Constants.text in
              let rope = Piece_rope.append Constants.insText rope in
              let result = Piece_rope.get_text rope in
              let expected = Constants.text ^ Constants.insText in
              Alcotest.(check string)
                "Append returns rope with new string at end" expected result);
        ] );
      ( "Piece_rope.fold_match_indices",
        [
          test "folds each match in string" (fun () ->
              let rope = Piece_rope.of_string "a a a a" in
              let count_of_a =
                Piece_rope.fold_match_indices "a" rope 0 (fun acc _ -> acc + 1)
              in
              let expected = 4 in
              Alcotest.(check int)
                "folds each match in string" expected count_of_a);
        ] );
      ( "Piece_rope.find_and_replace can replace when",
        [
          test "replace_string is same length" (fun () ->
              let start = "a bc b bc c bc d b" in
              let rope = Piece_rope.of_string start in
              let rope = Piece_rope.find_and_replace "bc" "00" rope in
              let result = Piece_rope.get_text rope in
              let expected = "a 00 b 00 c 00 d b" in
              Alcotest.(check string)
                "find_and_replace same length" expected result);
          test "replace_string is shorter" (fun () ->
              let start = "a bc b bc c bc d b" in
              let rope = Piece_rope.of_string start in
              let rope = Piece_rope.find_and_replace "bc" "0" rope in
              let result = Piece_rope.get_text rope in
              let expected = "a 0 b 0 c 0 d b" in
              Alcotest.(check string)
                "replace string is shorter" expected result);
          test "replace_string is longer" (fun () ->
              let start = "a bc b bc c bc d b" in
              let rope = Piece_rope.of_string start in
              let rope = Piece_rope.find_and_replace "bc" "000" rope in
              let result = Piece_rope.get_text rope in
              let expected = "a 000 b 000 c 000 d b" in
              Alcotest.(check string) "replace string is longer" expected result);
        ] );
    ]
