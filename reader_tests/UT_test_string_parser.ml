#use "topfind";;

#use "reader.ml";;

(* TESTER: test_string_parser.ml *)
let string_parser_tester_suite = [
  ("test_simple_string", [(String "abc")], (Reader.read_sexprs "\"abc\""));
  ("test_simple_string1", [(String "  dbc  ")], (Reader.read_sexprs " \"  dbc  \"    "));

  ("test_special_string", [(String "\\''")], (Reader.read_sexprs "\"\\\\''\""));
  ("test_complex_string", [(String "A4ge4...\\\"\\\"\\\\\\\\\\\\\\\\ \\t lo \\t \\r \\\\ \\n  tr3")],
  (Reader.read_sexprs "    \"A4ge4...\\\\\\\"\\\\\\\"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ \\\\t lo \\\\t \\\\r \\\\\\\\ \\\\n  tr3\"     "));
  ("test_complex_string1", [(String "hello \t \r \\ \n  world!")],
  (Reader.read_sexprs "\"hello \\t \\r \\\\ \\n  world!\""));
  ("test_complex_string2", [(String "A4ge4...\"\"\\\\\\t\t\t\t\t\t\t\n\\\\\\\'\'\'\"\" \t lo \t \r \\ \n  tr3")],
  (Reader.read_sexprs "\"A4ge4...\\\"\\\"\\\\\\\\\\\\t\\t\\t\\t\\t\\t\\t\\n\\\\\\\\\\\\'''\\\"\\\" \\t lo \\t \\r \\\\ \\n  tr3\""));
];;
