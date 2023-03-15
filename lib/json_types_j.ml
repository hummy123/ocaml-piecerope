(* Auto-generated from "json_types.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type json_piece = Json_types_t.json_piece = { start : int; length : int }

type json_doc = Json_types_t.json_doc = {
  buffer : string list;
  pieces : json_piece list;
  current : int list;
  undo : int list list;
  redo : int list list;
}

let write_json_piece : _ -> json_piece -> _ =
 fun ob (x : json_piece) ->
  Buffer.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Buffer.add_char ob ',';
  Buffer.add_string ob "\"start\":";
  Yojson.Safe.write_int ob x.start;
  if !is_first then is_first := false else Buffer.add_char ob ',';
  Buffer.add_string ob "\"length\":";
  Yojson.Safe.write_int ob x.length;
  Buffer.add_char ob '}'

let string_of_json_piece ?(len = 1024) x =
  let ob = Buffer.create len in
  write_json_piece ob x;
  Buffer.contents ob

let read_json_piece p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_start = ref None in
  let field_length = ref None in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s then
        invalid_arg
          (Printf.sprintf
             "out-of-bounds substring position or length: string = %S, \
              requested position = %i, requested length = %i"
             s pos len);
      match len with
      | 5 ->
          if
            String.unsafe_get s pos = 's'
            && String.unsafe_get s (pos + 1) = 't'
            && String.unsafe_get s (pos + 2) = 'a'
            && String.unsafe_get s (pos + 3) = 'r'
            && String.unsafe_get s (pos + 4) = 't'
          then 0
          else -1
      | 6 ->
          if
            String.unsafe_get s pos = 'l'
            && String.unsafe_get s (pos + 1) = 'e'
            && String.unsafe_get s (pos + 2) = 'n'
            && String.unsafe_get s (pos + 3) = 'g'
            && String.unsafe_get s (pos + 4) = 't'
            && String.unsafe_get s (pos + 5) = 'h'
          then 1
          else -1
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    (match i with
    | 0 -> field_start := Some (Atdgen_runtime.Oj_run.read_int p lb)
    | 1 -> field_length := Some (Atdgen_runtime.Oj_run.read_int p lb)
    | _ -> Yojson.Safe.skip_json p lb);
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s then
          invalid_arg
            (Printf.sprintf
               "out-of-bounds substring position or length: string = %S, \
                requested position = %i, requested length = %i"
               s pos len);
        match len with
        | 5 ->
            if
              String.unsafe_get s pos = 's'
              && String.unsafe_get s (pos + 1) = 't'
              && String.unsafe_get s (pos + 2) = 'a'
              && String.unsafe_get s (pos + 3) = 'r'
              && String.unsafe_get s (pos + 4) = 't'
            then 0
            else -1
        | 6 ->
            if
              String.unsafe_get s pos = 'l'
              && String.unsafe_get s (pos + 1) = 'e'
              && String.unsafe_get s (pos + 2) = 'n'
              && String.unsafe_get s (pos + 3) = 'g'
              && String.unsafe_get s (pos + 4) = 't'
              && String.unsafe_get s (pos + 5) = 'h'
            then 1
            else -1
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 -> field_start := Some (Atdgen_runtime.Oj_run.read_int p lb)
      | 1 -> field_length := Some (Atdgen_runtime.Oj_run.read_int p lb)
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    ({
       start =
         (match !field_start with
         | Some x -> x
         | None -> Atdgen_runtime.Oj_run.missing_field p "start");
       length =
         (match !field_length with
         | Some x -> x
         | None -> Atdgen_runtime.Oj_run.missing_field p "length");
     }
      : json_piece)

let json_piece_of_string s =
  read_json_piece (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write__string_list =
  Atdgen_runtime.Oj_run.write_list Yojson.Safe.write_string

let string_of__string_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_list ob x;
  Buffer.contents ob

let read__string_list =
  Atdgen_runtime.Oj_run.read_list Atdgen_runtime.Oj_run.read_string

let _string_list_of_string s =
  read__string_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write__json_piece_list = Atdgen_runtime.Oj_run.write_list write_json_piece

let string_of__json_piece_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__json_piece_list ob x;
  Buffer.contents ob

let read__json_piece_list = Atdgen_runtime.Oj_run.read_list read_json_piece

let _json_piece_list_of_string s =
  read__json_piece_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write__int_list = Atdgen_runtime.Oj_run.write_list Yojson.Safe.write_int

let string_of__int_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__int_list ob x;
  Buffer.contents ob

let read__int_list =
  Atdgen_runtime.Oj_run.read_list Atdgen_runtime.Oj_run.read_int

let _int_list_of_string s =
  read__int_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write__int_list_list = Atdgen_runtime.Oj_run.write_list write__int_list

let string_of__int_list_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__int_list_list ob x;
  Buffer.contents ob

let read__int_list_list = Atdgen_runtime.Oj_run.read_list read__int_list

let _int_list_list_of_string s =
  read__int_list_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_json_doc : _ -> json_doc -> _ =
 fun ob (x : json_doc) ->
  Buffer.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Buffer.add_char ob ',';
  Buffer.add_string ob "\"buffer\":";
  write__string_list ob x.buffer;
  if !is_first then is_first := false else Buffer.add_char ob ',';
  Buffer.add_string ob "\"pieces\":";
  write__json_piece_list ob x.pieces;
  if !is_first then is_first := false else Buffer.add_char ob ',';
  Buffer.add_string ob "\"current\":";
  write__int_list ob x.current;
  if !is_first then is_first := false else Buffer.add_char ob ',';
  Buffer.add_string ob "\"undo\":";
  write__int_list_list ob x.undo;
  if !is_first then is_first := false else Buffer.add_char ob ',';
  Buffer.add_string ob "\"redo\":";
  write__int_list_list ob x.redo;
  Buffer.add_char ob '}'

let string_of_json_doc ?(len = 1024) x =
  let ob = Buffer.create len in
  write_json_doc ob x;
  Buffer.contents ob

let read_json_doc p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_buffer = ref None in
  let field_pieces = ref None in
  let field_current = ref None in
  let field_undo = ref None in
  let field_redo = ref None in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s then
        invalid_arg
          (Printf.sprintf
             "out-of-bounds substring position or length: string = %S, \
              requested position = %i, requested length = %i"
             s pos len);
      match len with
      | 4 -> (
          match String.unsafe_get s pos with
          | 'r' ->
              if
                String.unsafe_get s (pos + 1) = 'e'
                && String.unsafe_get s (pos + 2) = 'd'
                && String.unsafe_get s (pos + 3) = 'o'
              then 4
              else -1
          | 'u' ->
              if
                String.unsafe_get s (pos + 1) = 'n'
                && String.unsafe_get s (pos + 2) = 'd'
                && String.unsafe_get s (pos + 3) = 'o'
              then 3
              else -1
          | _ -> -1)
      | 6 -> (
          match String.unsafe_get s pos with
          | 'b' ->
              if
                String.unsafe_get s (pos + 1) = 'u'
                && String.unsafe_get s (pos + 2) = 'f'
                && String.unsafe_get s (pos + 3) = 'f'
                && String.unsafe_get s (pos + 4) = 'e'
                && String.unsafe_get s (pos + 5) = 'r'
              then 0
              else -1
          | 'p' ->
              if
                String.unsafe_get s (pos + 1) = 'i'
                && String.unsafe_get s (pos + 2) = 'e'
                && String.unsafe_get s (pos + 3) = 'c'
                && String.unsafe_get s (pos + 4) = 'e'
                && String.unsafe_get s (pos + 5) = 's'
              then 1
              else -1
          | _ -> -1)
      | 7 ->
          if
            String.unsafe_get s pos = 'c'
            && String.unsafe_get s (pos + 1) = 'u'
            && String.unsafe_get s (pos + 2) = 'r'
            && String.unsafe_get s (pos + 3) = 'r'
            && String.unsafe_get s (pos + 4) = 'e'
            && String.unsafe_get s (pos + 5) = 'n'
            && String.unsafe_get s (pos + 6) = 't'
          then 2
          else -1
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    (match i with
    | 0 -> field_buffer := Some (read__string_list p lb)
    | 1 -> field_pieces := Some (read__json_piece_list p lb)
    | 2 -> field_current := Some (read__int_list p lb)
    | 3 -> field_undo := Some (read__int_list_list p lb)
    | 4 -> field_redo := Some (read__int_list_list p lb)
    | _ -> Yojson.Safe.skip_json p lb);
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s then
          invalid_arg
            (Printf.sprintf
               "out-of-bounds substring position or length: string = %S, \
                requested position = %i, requested length = %i"
               s pos len);
        match len with
        | 4 -> (
            match String.unsafe_get s pos with
            | 'r' ->
                if
                  String.unsafe_get s (pos + 1) = 'e'
                  && String.unsafe_get s (pos + 2) = 'd'
                  && String.unsafe_get s (pos + 3) = 'o'
                then 4
                else -1
            | 'u' ->
                if
                  String.unsafe_get s (pos + 1) = 'n'
                  && String.unsafe_get s (pos + 2) = 'd'
                  && String.unsafe_get s (pos + 3) = 'o'
                then 3
                else -1
            | _ -> -1)
        | 6 -> (
            match String.unsafe_get s pos with
            | 'b' ->
                if
                  String.unsafe_get s (pos + 1) = 'u'
                  && String.unsafe_get s (pos + 2) = 'f'
                  && String.unsafe_get s (pos + 3) = 'f'
                  && String.unsafe_get s (pos + 4) = 'e'
                  && String.unsafe_get s (pos + 5) = 'r'
                then 0
                else -1
            | 'p' ->
                if
                  String.unsafe_get s (pos + 1) = 'i'
                  && String.unsafe_get s (pos + 2) = 'e'
                  && String.unsafe_get s (pos + 3) = 'c'
                  && String.unsafe_get s (pos + 4) = 'e'
                  && String.unsafe_get s (pos + 5) = 's'
                then 1
                else -1
            | _ -> -1)
        | 7 ->
            if
              String.unsafe_get s pos = 'c'
              && String.unsafe_get s (pos + 1) = 'u'
              && String.unsafe_get s (pos + 2) = 'r'
              && String.unsafe_get s (pos + 3) = 'r'
              && String.unsafe_get s (pos + 4) = 'e'
              && String.unsafe_get s (pos + 5) = 'n'
              && String.unsafe_get s (pos + 6) = 't'
            then 2
            else -1
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 -> field_buffer := Some (read__string_list p lb)
      | 1 -> field_pieces := Some (read__json_piece_list p lb)
      | 2 -> field_current := Some (read__int_list p lb)
      | 3 -> field_undo := Some (read__int_list_list p lb)
      | 4 -> field_redo := Some (read__int_list_list p lb)
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    ({
       buffer =
         (match !field_buffer with
         | Some x -> x
         | None -> Atdgen_runtime.Oj_run.missing_field p "buffer");
       pieces =
         (match !field_pieces with
         | Some x -> x
         | None -> Atdgen_runtime.Oj_run.missing_field p "pieces");
       current =
         (match !field_current with
         | Some x -> x
         | None -> Atdgen_runtime.Oj_run.missing_field p "current");
       undo =
         (match !field_undo with
         | Some x -> x
         | None -> Atdgen_runtime.Oj_run.missing_field p "undo");
       redo =
         (match !field_redo with
         | Some x -> x
         | None -> Atdgen_runtime.Oj_run.missing_field p "redo");
     }
      : json_doc)

let json_doc_of_string s =
  read_json_doc (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
