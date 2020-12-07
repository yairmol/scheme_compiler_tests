#use "topfind";;
#directory "../";;
#directory "../../";;
#use "reader.ml";;

(* TESTER: test_bool_parser.ml *)
let bool_parser_tester_suite = [
  ("test_true_lower", [(Bool true)], (Reader.read_sexprs "#t"));
  ("test_true_upper", [(Bool true)], (Reader.read_sexprs "#T"));
  ("test_false_lower", [(Bool false)], (Reader.read_sexprs "#f"));
  ("test_false_upper", [(Bool false)], (Reader.read_sexprs "#F"));

  ("test_true_lower_whitespace_both_sides", [(Bool true)], (Reader.read_sexprs "  #t "));
  ("test_true_lower_whitespace_right_side", [(Bool true)], (Reader.read_sexprs "#t   "));
  ("test_true_lower_whitespace_left_side", [(Bool true)], (Reader.read_sexprs "   #t"));
];;

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

(* TESTER: test_number_parser.ml *)
let number_parser_tester_suite = [  
  ("test_negetive_int_signed", [(Number (Fraction ((-123), 1)))], (Reader.read_sexprs "-123"));
  ("test_positive_int_signed", [(Number (Fraction (123, 1)))], (Reader.read_sexprs "+123"));
  ("test_positive_int_not_signed", [(Number (Fraction (123, 1)))], (Reader.read_sexprs "123"));

  ("test_negetive_int_signed_whitespace_both_sides", [(Number (Fraction ((-123), 1)))], (Reader.read_sexprs "  -123 "));
  ("test_positive_int_signed_whitespace_both_sides", [(Number (Fraction (123, 1)))], (Reader.read_sexprs "  +123 "));
  ("test_positive_int_not_signed_whitespace_both_sides", [(Number (Fraction (123, 1)))], (Reader.read_sexprs "   123 "));

(* Edge cases of integers *)
  ("edge_int_leading_zeroes_positive", [(Number (Fraction (123, 1)))], (Reader.read_sexprs "000123"));
  ("edge_int_leading_zeroes_negative", [(Number (Fraction ((-123), 1)))], (Reader.read_sexprs "-000123"));
  ("edge_int_spaces_between", ([Number (Fraction (1, 1)); Number (Fraction (123, 1))]), (Reader.read_sexprs "1   123"));
  ("edge_int_space_to_float", ([Number (Fraction (5, 1)); Number(Float (3.6))]), (Reader.read_sexprs "5 3.6"));

(* Test positive & negative floats *)

  ("test_negetive_float_signed", [(Number (Float (-123.1)))], (Reader.read_sexprs "-123.1"));
  ("test_negetive_float_signed_2", [(Number (Float (-0.4321)))], (Reader.read_sexprs "-0.4321"));
  ("test_positive_float_signed", [(Number (Float 123.1))], (Reader.read_sexprs "+123.1"));
  ("test_positive_float_signed_2", [(Number (Float 0.4321))], (Reader.read_sexprs "+0.4321"));
  ("test_positive_float_not_signed", [(Number (Float 123.112))], (Reader.read_sexprs "123.112"));

  ("test_negative_float_signed_whitespace_both_sides", [(Number (Float (-123.112)))], (Reader.read_sexprs " -123.112  "));
  ("test_positive_float_signed_whitespace_both_sides", [(Number (Float 123.112))], (Reader.read_sexprs " +123.112  "));
  ("test_positive_float_not_signed_whitespace_both_sides", [(Number (Float 123.112))], (Reader.read_sexprs "  123.112  "));

(* Edge cases of floats *)
  ("edge_float_leading_zeroes_positive", [(Number (Float 123.5))], (Reader.read_sexprs "000123.5"));
  ("edge_float_leading_zeroes_negative", [(Number (Float (-123.5)))], (Reader.read_sexprs "-00123.5"));
  ("edge_float_leading_and_trailing_zeroes_positive", [(Number (Float 123.5))], (Reader.read_sexprs "000123.5000"));
  ("edge_float_spaces_between", ([Number (Float 1.1); Number (Fraction (123, 1))]), (Reader.read_sexprs "1.1 123"));

  ("test_fraction_1", ([Number (Fraction (1, 2))]), (Reader.read_sexprs "64/128"));
  ("test_fraction_2", ([Number (Fraction (1, 3))]), (Reader.read_sexprs "8/24"));

(* test numbers *)
  ("test_number_1", [(Number (Float 123.555))], (Reader.read_sexprs "000123.555"));
  ("test_number_2", [(Number (Float 123.00555))], (Reader.read_sexprs "000123.00555"));
  ("test_number_3", [(Number (Fraction (1000, 1)))], (Reader.read_sexprs "0001000"));
  ("test_number_4", ([Number (Float 1.2); Number(Float (3.45))]), (Reader.read_sexprs " 1.2 3.45"));
  ("test_number_5", ([Number (Float 53.2); Number (Float 3.45); Number (Float 23.4)]), (Reader.read_sexprs "53.2 ;hello its a comment\n 3.45 ;another comment,,,, \n     23.4    ;another comment here.......\n"));
  ("test_number_6", ([Number (Fraction ((-1), 1)); Number (Float 3.45); Number (Float 57.4)]), (Reader.read_sexprs "-001 ;hello its a comment\n 3.45;another comment,,,, \n   #;  64.4 57.4    ;another comment here.......\n"));
];;

let quote_parser_tester_suite = [
  ("simple_quote_test", [Pair ((Symbol "quote"), Pair ((Bool true), Nil))], (Reader.read_sexprs "'#t"));
  ("simple_quasiquote_test", [Pair ((Symbol "quasiquote"), Pair ((Bool true), Nil))], (Reader.read_sexprs "`#t"));
  ("simple_unquote_test", [Pair ((Symbol "unquote"), Pair ((Bool true), Nil))], (Reader.read_sexprs ",#t"));
  ("simple_unquotesplice_test", [Pair ((Symbol "unquote-splicing"), Pair ((Bool true), Nil))], (Reader.read_sexprs ",@#t"));

  ("simple_quote_test_with_spaces", [Pair ((Symbol "quote"), Pair ((Bool true), Nil))], (Reader.read_sexprs "   '  #t  "));
  ("simple_quasiquote_test_with_spaces", [Pair ((Symbol "quasiquote"), Pair ((Bool true), Nil))], (Reader.read_sexprs "  `    #t  "));
  ("simple_unquote_test_with_spaces", [Pair ((Symbol "unquote"), Pair ((Bool true), Nil))], (Reader.read_sexprs "   ,   #t   "));
  ("simple_unquotesplice_test_with_spaces", [Pair ((Symbol "unquote-splicing"), Pair ((Bool true), Nil))], (Reader.read_sexprs "  ,@   #t"));

  ("nested_quotes_1", [
    Pair (Symbol "quasiquote",
        Pair
        (Pair (Bool true,
            Pair
            (Pair (Symbol "unquote",
                Pair
                (Pair (Symbol "+",
                Pair (Number (Fraction (1, 1)),
                    Pair (Number (Fraction (2, 1)),
                    Pair (Number (Fraction (3, 1)), Nil)))),
                Nil)),
            Pair (Bool false, Nil))),
        Nil))
    ], (Reader.read_sexprs "`(#t ,(+ 1 2 3) #f)"));

  ("nested_quotes_2", [
    Pair (Symbol "quasiquote",
        Pair
        (Pair (Symbol "a",
            Pair
            (Pair (Symbol "unquote-splicing",
                Pair
                (Pair (Symbol "append",
                Pair
                    (Pair (Symbol "quote",
                    Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)), Nil)),
                    Pair
                    (Pair (Symbol "quote",
                    Pair (Pair (Symbol "z", Pair (Symbol "w", Nil)), Nil)),
                    Nil))),
                Nil)),
            Pair (Symbol "b", Nil))),
        Nil))
    ], (Reader.read_sexprs "`(a ,@(append '(x y) '(z w)) b)"));
];;

let scientific_parser_tester_suite = [
  ("test_positive_int_signed1", [(Number (Float (120.)))], (Reader.read_sexprs "+12e1  "));
  ("test_positive_int_signed2", [(Number (Float (10.)))], (Reader.read_sexprs "+1e1  "));
  ("test_positive_int_signed3", [(Number (Float (10.)))], (Reader.read_sexprs "1E1 "));
  ("test_positive_int_signed4", [(Number (Float (1230.)))], (Reader.read_sexprs "    +000000012.3E00000002"));

  ("test_positive_int_second_signed1", [(Number (Float (10.)))], (Reader.read_sexprs "1E+1 "));
  ("test_positive_int_second_signed2", [(Number (Float (0.1)))], (Reader.read_sexprs "1E-1 "));
  ("test_positive_int_second_signed3", [(Number (Float (13.4)))], (Reader.read_sexprs "0000134E-1 "));
  ("test_positive_int_second_signed4", [(Number (Float 7200000000.))], (Reader.read_sexprs "00072E+00008 "));
  ("test_positive_float_second_signed5", [(Number (Float 3140000000.))], (Reader.read_sexprs "  3.14e+9 "));

  ("test_positive_int_unsigned", [(Number (Float (100.)))], (Reader.read_sexprs " 10e1  "));
  ("test_negative_float_second_signed", [(Number (Float 0.))], (Reader.read_sexprs "  3.14E-512 "));
  ("test_negative_float_signed_both", [(Number (Float (-0.05)))], (Reader.read_sexprs "    -5.000000000e-2  "));
];;

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
