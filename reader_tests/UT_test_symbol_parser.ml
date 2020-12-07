#use "topfind";;

#use "reader.ml";;

(* TESTER: test_symbol_parser.ml *)
let symbol_parser_tester_suite = [
  ("test_symbol_0", [(Symbol "a")], (Reader.read_sexprs "a"));
  ("test_symbol_1", [(Symbol "!0$aa!1")], (Reader.read_sexprs "!0$aA!1"));
  ("test_symbol_2", [(Symbol "zzaa!:=9")], (Reader.read_sexprs "zZaA!:=9"));
  ("test_symbol_3", [(Symbol "123a")], (Reader.read_sexprs "123a"));
  ("test_symbol_3", [(Symbol "123a")], (Reader.read_sexprs "123A"));

  ("test_symbol_space_left", [(Symbol "!0$aa!1")], (Reader.read_sexprs "!0$aA!1"));
  ("test_symbol_space_right", [(Symbol "!0$aa!1")], (Reader.read_sexprs "!0$aA!1"));
  ("test_symbol_spaces_both_sides", [(Symbol "!0$aa!1")], (Reader.read_sexprs "!0$aA!1"));
];;