#use "topfind";;

#use "reader.ml";;

(* TESTER: test_char_parser.ml *)
let char_parser_tester_suite = [
  ("test_newline", [(Char '\n')], (Reader.read_sexprs " #\\newline"));
  ("test_newline_with_spaces", [(Char '\n')], (Reader.read_sexprs "   #\\newline "));
  ("test_newline_with_spaces2", [(Char '\n')], (Reader.read_sexprs "   #\\nEwline "));
  ("test_return_with_spaces", [(Char '\r')], (Reader.read_sexprs "   #\\return    "));
  ("test_charS_with_spaces", [(Char 'S')], (Reader.read_sexprs "   #\\S  "));
  ("test_chara", [(Char 'a')], (Reader.read_sexprs "   #\\a "));
  ("test_page_with_spaces", [(Char '\012')], (Reader.read_sexprs "   #\\page    "));
  ("test_page_with_spaces2", [(Char '\012')], (Reader.read_sexprs "   #\\pAGE    "));
  ("test_char_with_extras", [Char 'A'; Symbol "b"], (Reader.read_sexprs "#\\Ab"));
];;
