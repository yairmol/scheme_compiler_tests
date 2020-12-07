#use "topfind";;

#use "reader.ml";;

(* TESTER: test_list_parser.ml *)
let list_parser_tester_suite = [
  ("test_number_list", [(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (5, 1)), Nil)))], (Reader.read_sexprs "(1 5)"));
  ("test_combined_list_1", [(Pair (Number (Fraction (1, 1)), Pair (Symbol ("b"), Pair (Number (Fraction (5, 1)), Nil))))], (Reader.read_sexprs "(1 b 5)"));
  ("test_nested_list_1", [(Pair (Pair (Number (Fraction (1, 1)), (Pair (Pair (Symbol ("b"), Nil), Nil))), Pair (Number (Fraction (5, 1)), Nil)))], (Reader.read_sexprs "((1 (b)) 5)"));

  ("test_empty_list", [(Nil)], (Reader.read_sexprs "()"));
  ("test_comment_in_list_1", [(Pair (String "this", Nil))], (Reader.read_sexprs "(\"this\" ; hahahahahaa \n)"));
  ("test_comment_in_list_2", ([Nil]), (Reader.read_sexprs "( ;sad\n )"));
  ("test_sexp_comment_in_list_1", ([Nil]), (Reader.read_sexprs "( #; 1 )"));


  ("test_dotted_list_1", [(Pair (Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (5, 1)), Nil)), Number (Fraction (6, 1))))], (Reader.read_sexprs "((1 5) . 6)"));
  ("test_dotted_list_2", [(Pair (Number (Float 1.2), Number (Fraction(3, 1))))], (Reader.read_sexprs "(1.2 . 3)"));
  ("test_dotted_list_3", [(Pair (Symbol ("a"), Symbol ("b")))], (Reader.read_sexprs "(a . b)"));
  ("test_dotted_list_4", [(Pair (Symbol ("b"), Pair (Symbol ("a"), Symbol ("b"))))], (Reader.read_sexprs "(b a . b)"));

  ("test_dotted_list_nested_1", [(Pair (Symbol ("a"), (Pair (Symbol ("b"), Symbol ("c")))))], (Reader.read_sexprs "(a . (b . c))"));
  ("test_dotted_list_nested_2", [(Pair (Symbol ("a"), (Pair (Symbol ("b"), Pair (Symbol ("c"), (Pair (Symbol ("d"), Symbol ("e"))))))))], (Reader.read_sexprs "(a . (b . (c . (d . e))))"));
];;

