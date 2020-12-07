#use "topfind";;

#use "reader.ml";;

(* TESTER: test_scientific_parser.ml *)
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

