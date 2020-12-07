#use "topfind";;

#use "reader.ml";;

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