(*
    I personally find Expecto (F#) and Jest (Javascript)
    to have syntactically cleaner testing styles with a better signal-to-noise ratio.
    The partially applied function below helps mimic that syntax.
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
      ( "Piece_rope.find_matches",
        [
          test "returns indices in order" (fun () ->
              let rope = Piece_rope.of_string "a bc b bc c bc d b" in
              let expected_indices = [| 2; 7; 12 |] in
              let expected_0 = Array.get expected_indices 0 in
              let expected_1 = Array.get expected_indices 1 in
              let expected_2 = Array.get expected_indices 2 in

              let result = Piece_rope.find_matches "bc" rope in
              let result_0 = Array.get result 0 in
              let result_1 = Array.get result 1 in
              let result_2 = Array.get result 2 in

              let _ =
                Alcotest.(check int) "first is same" expected_0 result_0
              in
              let _ =
                Alcotest.(check int) "second is same" expected_1 result_1
              in
              Alcotest.(check int) "third is same" expected_2 result_2);
        ] );
      ( "Piece_rope.find_and_replace",
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
      ( "Piece_rope.fold_lines",
        [
          test "executes folder when there is only one line" (fun () ->
              let rope = Piece_rope.of_string "qwerty" in
              let result =
                Piece_rope.fold_lines rope 0 (fun acc _ -> acc + 1)
              in
              let expected = 1 in
              Alcotest.(check int) "folder executes once" expected result);
          test "executes folder on every line" (fun () ->
              let rope =
                Piece_rope.of_string "one line \n two lines \n three lines"
              in
              let result =
                Piece_rope.fold_lines rope 0 (fun acc _ -> acc + 1)
              in
              let expected = 3 in
              Alcotest.(check int) "folder executes once" expected result);
        ] );
      ( "Piece_rope.get_line",
        [
          test
            "returns first line when there are no line breaks and we ask for \
             the first line" (fun () ->
              let expected = "asdflkjhg" in
              let rope = Piece_rope.of_string expected in
              let result = Piece_rope.get_line 0 rope in
              Alcotest.(check string)
                "returns first line when no line breaks" expected result.line);
          test "returns first line when there is one line break" (fun () ->
              let str = "asdf\nvjriouvr" in
              let expected = "asdf\n" in
              let rope = Piece_rope.of_string str in
              let result = Piece_rope.get_line 0 rope in
              Alcotest.(check string) "returns first line" expected result.line);
          test "returns second line when there is one line break" (fun () ->
              let str = "asdf\necjeev" in
              let expected = "ecjeev" in
              let rope = Piece_rope.of_string str in
              let result = Piece_rope.get_line 1 rope in
              Alcotest.(check string) "returns second line" expected result.line);
          test "returns \r\n when line ends with that" (fun () ->
              let str = "asdf\r\nvrefvij" in
              let expected = "asdf\r\n" in
              let rope = Piece_rope.of_string str in
              let result = Piece_rope.get_line 0 rope in
              Alcotest.(check string) "returns \r\n" expected result.line);
          test "returns first line when we split string with \n" (fun () ->
              (* No lines in input string. *)
              let str = "asdfvrefvij" in
              let rope = Piece_rope.of_string str in
              let rope = Piece_rope.insert 1 "\n" rope in
              let expected = "a\n" in
              let result = Piece_rope.get_line 0 rope in
              Alcotest.(check string) "returns first line" expected result.line);
          test "returns second line when we split string with \n" (fun () ->
              let str = "asdfvrefvij" in
              let rope = Piece_rope.of_string str in
              let rope = Piece_rope.insert 1 "\n" rope in
              let expected = "sdfvrefvij" in
              let result = Piece_rope.get_line 1 rope in
              Alcotest.(check string) "returns second line" expected result.line);
          test "returns second line when we insert into string with \r\n"
            (fun () ->
              let str = "asdfvrefvij" in
              let rope = Piece_rope.of_string str in
              let rope = Piece_rope.insert 1 "\r\n" rope in
              let expected = "sdfvrefvij" in
              let result = Piece_rope.get_line 1 rope in
              Alcotest.(check string) "returns first line" expected result.line);
          test "returns whole line when we delete only line break in string"
            (fun () ->
              let str = "asdf\nvrefvij" in
              let rope = Piece_rope.of_string str in
              let rope = Piece_rope.delete 4 1 rope in
              let expected = "asdfvrefvij" in
              let result = Piece_rope.get_line 0 rope in
              Alcotest.(check string) "returns whole line" expected result.line);
          test "deleting last char of \r\n deletes both" (fun () ->
              let str = "asdf\r\n1234" in
              let rope = Piece_rope.of_string str in
              let rope = Piece_rope.delete 5 1 rope in
              let expected = "asdf1234" in
              let result = Piece_rope.get_text rope in
              Alcotest.(check string) "returns string with \r" expected result);
          test "deleting first char of \r\n deletes both" (fun () ->
              let str = "asdf\r\n1234" in
              let rope = Piece_rope.of_string str in
              let rope = Piece_rope.delete 4 1 rope in
              let expected = "asdf1234" in
              let result = Piece_rope.get_text rope in
              Alcotest.(check string) "returns string with \n" expected result);
          test "returns line when line spans multiple pieces (a)" (fun () ->
              let initial_str = "a\nsdf" in
              let rope = Piece_rope.of_string initial_str in
              let rope = Piece_rope.prepend "zxcv" rope in
              let expected = "zxcva\n" in
              let result = Piece_rope.get_line 0 rope in
              Alcotest.(check string) "returns line" expected result.line);
          test "returns line when line spans multiple pieces (b)" (fun () ->
              let initial_str = "a\nsdf" in
              let rope = Piece_rope.of_string initial_str in
              let rope = Piece_rope.prepend "zxcv" rope in
              let rope = Piece_rope.append "qw\nerty" rope in
              let expected = "zxcva\n" in
              let result = Piece_rope.get_line 0 rope in
              let _ =
                Alcotest.(check string) "returns line" expected result.line
              in
              let expected = "sdfqw\n" in
              let result = Piece_rope.get_line 1 rope in
              let _ =
                Alcotest.(check string) "returns line" expected result.line
              in
              let expected = "erty" in
              let result = Piece_rope.get_line 2 rope in
              Alcotest.(check string) "returns line" expected result.line);
          test "returns middle '\n' when there are 3 '\n' in piece" (fun () ->
              let str = "asdf\nqwer\nzxcv" in
              let rope = Piece_rope.of_string str in
              let expected = "qwer\n" in
              let result = Piece_rope.get_line 1 rope in
              Alcotest.(check string) "returns middle line" expected result.line);
          test "returns middle '\r\n' when there are 3 '\r\n' in piece"
            (fun () ->
              let str = "asdf\r\nqwer\r\nzxcv" in
              let rope = Piece_rope.of_string str in
              let expected = "qwer\r\n" in
              let result = Piece_rope.get_line 1 rope in
              Alcotest.(check string) "returns middle line" expected result.line);
          test "returns '\n' when there are consecutive '\n's" (fun () ->
              let str = "\n\n\n" in
              let rope = Piece_rope.of_string str in
              let expected = "\n" in
              let result = Piece_rope.get_line 1 rope in
              Alcotest.(check string) "returns '\n'" expected result.line);
          test "returns '\r\n' when there are consecutive '\r\n's" (fun () ->
              let str = "\r\n\r\n\r\n" in
              let rope = Piece_rope.of_string str in
              let expected = "\r\n" in
              let result = Piece_rope.get_line 1 rope in
              Alcotest.(check string) "returns '\r'\n" expected result.line);
        ] );
      ( "Piece_rope.insert",
        [
          test "trying to insert into middle of \r\n inserts after \r\n pair"
            (fun () ->
              let rope = Piece_rope.of_string "az\r\n23" in
              let rope = Piece_rope.insert 3 "_" rope in
              let expected = "az\r\n_23" in
              let result = Piece_rope.get_text rope in
              let _ =
                Alcotest.(check string)
                  "inserts string after \r\n" expected result
              in

              (* Also check if get_line returns expected result after above insertion. *)
              let line1 = Piece_rope.get_line 0 rope in
              let expected_line1 = "az\r\n" in
              let line2 = Piece_rope.get_line 1 rope in
              let expected_line2 = "_23" in
              let _ =
                Alcotest.(check string)
                  "first line contains \r\n" expected_line1 line1.line
              in
              Alcotest.(check string)
                "second line starts after \r\n" expected_line2 line2.line);
        ] );
      ( "Piece_rope.delete",
        [
          test "deletes \r\n when \r\n is at start and we only try deleting \n"
            (fun () ->
              let rope = Piece_rope.of_string "\r\n_+!£%&()" in
              let rope = Piece_rope.delete 1 1 rope in
              let expected = "_+!£%&()" in
              let result = Piece_rope.get_text rope in
              Alcotest.(check string) "deletes \r\n" expected result);
          test "deletes \r\n when \r\n is at start and we only try deleting \r"
            (fun () ->
              let rope = Piece_rope.of_string "\r\n_+!£%&()" in
              let rope = Piece_rope.delete 0 1 rope in
              let expected = "_+!£%&()" in
              let result = Piece_rope.get_text rope in
              Alcotest.(check string) "deletes \r\n" expected result);
          test "deletes \n at start properly try deleting at start" (fun () ->
              let rope = Piece_rope.of_string "\nxcvbnm" in
              let rope = Piece_rope.delete 0 1 rope in
              let expected = "xcvbnm" in
              let result = Piece_rope.get_text rope in
              Alcotest.(check string) "deletes start" expected result);
          test
            "deletes \r\n\
            \ at end when \r\n\
            \ is at end and we only try deleting \r" (fun () ->
              let rope = Piece_rope.of_string "qwerty\r\n" in
              let rope = Piece_rope.delete 6 1 rope in
              let expected = "qwerty" in
              let result = Piece_rope.get_text rope in
              Alcotest.(check string) "deletes end" expected result);
          test
            "deletes \r\n\
            \ at end when \r\n\
            \ is at end and we only try deleting \n" (fun () ->
              let rope = Piece_rope.of_string "qwerty\r\n" in
              let rope = Piece_rope.delete 7 1 rope in
              let expected = "qwerty" in
              let result = Piece_rope.get_text rope in
              Alcotest.(check string) "deletes end" expected result);
        ] );
      ( "Piece_rope.serialise",
        [
          test "returns true" (fun () ->
              let file_path = "svelte_data.json" in
              let expected = true in
              (* Arrange: create piece_rope with random edits. *)
              let rope =
                Piece_rope.empty
                |> Piece_rope.insert 0 "12344092firheu"
                |> Piece_rope.delete 0 8
                |> Piece_rope.prepend "1234ecdfc"
                |> Piece_rope.append "123454"
              in
              (* Assert: test if true is returned. *)
              let result = Piece_rope.serialise file_path rope in
              let _ = Sys.remove file_path in
              Alcotest.(check bool) "returns true" expected result);
        ] );
      ( "Piece_rope.deserialise",
        [
          test "returns same rope we serialised" (fun () ->
              let file_path = "svelte_data.json" in
              (* Arrange: create piece_rope with random edits. *)
              let input_rope =
                Piece_rope.empty
                |> Piece_rope.insert 0 "12344092firheu"
                |> Piece_rope.delete 0 8
                |> Piece_rope.prepend "1234ecdfc"
                |> Piece_rope.append "123454"
              in
              (* Act: serialise and deserialise. *)
              let input_text = Piece_rope.get_text input_rope in
              let _ = Piece_rope.serialise file_path input_rope in
              let output_rope = Piece_rope.deserialise file_path in
              let _ = Sys.remove file_path in
              (* Assert: does input (before serialisation) contain same text as output (after serialisation)? *)
              let output_text = Piece_rope.get_text output_rope in
              Alcotest.(check string) "is same" input_text output_text);
        ] );
    ]
