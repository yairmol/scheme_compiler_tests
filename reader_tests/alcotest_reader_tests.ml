#use "reader_tests.ml";;
#use "topfind";;
#require "alcotest";;

let rec string_of_sexp se = 
    let string_of_number n = match n with
    | Fraction((a, b)) -> "Fraction(" ^ (string_of_int a) ^ "/" ^ (string_of_int b) ^ ")"
    | Float(f) -> "Float(" ^ (string_of_float f) ^ ")" in  
    match se with
    | Bool(b) -> "Bool(" ^ (string_of_bool b) ^ ")"
    | Nil -> "Nil"
    | Number(n) -> string_of_number n
    | Char(c) ->  "Char(" ^ (String.make 1 c) ^")"
    | String(s) -> "String(" ^ s ^ ")"
    | Symbol(s) -> "Symbol(" ^ s ^ ")"
    | Pair((se1, se2)) -> "Pair(" ^ (string_of_sexp se1) ^ ", " ^ (string_of_sexp se2) ^ ")"

let sexp_testable = Alcotest.testable (fun ppf (se: sexpr) -> (Format.fprintf ppf "%s" (string_of_sexp se))) sexpr_eq;;

let map_test_suite_to_alcotest_test_suite test_suite = List.map (fun (name, expected, input) -> 
  (name, fun () -> Alcotest.(check (list sexp_testable)) "same sexpression?" (Reader.read_sexprs input) expected)) test_suite;;

let () =
  let open Alcotest in
  run "Test Parser" [
      "test boolean", (List.map (fun (desc, test) -> test_case desc `Quick test) (map_test_suite_to_alcotest_test_suite bool_parser_tester_suite));
      "Test Char", (List.map (fun (desc, test) -> test_case desc `Quick test) (map_test_suite_to_alcotest_test_suite char_parser_tester_suite));
      "Test symbol",  (List.map (fun (desc, test) -> test_case desc `Quick test) (map_test_suite_to_alcotest_test_suite symbol_parser_tester_suite));
      "Test list",  (List.map (fun (desc, test) -> test_case desc `Quick test) (map_test_suite_to_alcotest_test_suite list_parser_tester_suite));
      "Test string",  (List.map (fun (desc, test) -> test_case desc `Quick test) (map_test_suite_to_alcotest_test_suite string_parser_tester_suite));
      "Test number",  (List.map (fun (desc, test) -> test_case desc `Quick test) (map_test_suite_to_alcotest_test_suite number_parser_tester_suite));
      "Test scientific",  (List.map (fun (desc, test) -> test_case desc `Quick test) (map_test_suite_to_alcotest_test_suite scientific_parser_tester_suite));
      "Test quote",  (List.map (fun (desc, test) -> test_case desc `Quick test) (map_test_suite_to_alcotest_test_suite quote_parser_tester_suite));
      (* add more tests here *)
    ]