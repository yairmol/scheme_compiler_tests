#use "topfind";;

#use "tag_parser_utils.ml";;
#require "alcotest";;

let base_case () = 
  let and_exp = Tag_Parser.tag_parse_expressions [Pair (Symbol "and", Nil)] in
  let expected = [Const (Sexpr (Bool true))] in
    Alcotest.(check (list expr_testable)) "same sexp?" expected and_exp;;

let base_case_2 () = 
  let and_exp = Tag_Parser.tag_parse_expressions [Pair (Symbol "and", Pair (Number (Fraction (1, 1)), Nil))] in
  let expected = [Const (Sexpr (Number (Fraction (1, 1))))] in
    Alcotest.(check (list expr_testable)) "same sexp?" expected and_exp;;

let test_simple_and () = 
  let and_exp = Tag_Parser.tag_parse_expressions [Pair (Symbol "and", Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil))))] in
  let expected = [If (Var "a", If (Var "b", Var "c", Const (Sexpr (Bool false))), Const (Sexpr (Bool false)))] in
    Alcotest.(check (list expr_testable)) "same sexp?" expected and_exp;;

let test_complex_and () = 
  let and_exp = Tag_Parser.tag_parse_expressions [
    Pair (Symbol "and",
      Pair (Pair (Symbol "quote", Pair (Nil, Nil)),
      Pair (Pair (Symbol "+", Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),
      Pair (Pair (Symbol "quote", Pair (Pair (Bool true, Pair (Char 'c', Pair (String "hello", Symbol ".sym?"))), Nil)),
     Pair (Bool false, Nil)))))] in
  let expected = [
    If (
      Const (Sexpr Nil),
      If (
        Applic (Var "+", [Const (Sexpr (Number (Fraction (2, 1)))); Const (Sexpr (Number (Fraction (3, 1))))]),
        If (
          Const (Sexpr (Pair (Bool true, Pair (Char 'c', Pair (String "hello", Symbol ".sym?"))))),
          Const (Sexpr (Bool false)),
          Const (Sexpr (Bool false))
        ),
        Const (Sexpr (Bool false))  
      ),
      Const (Sexpr (Bool false))

    )
  ] in
    Alcotest.(check (list expr_testable)) "same sexp?" expected and_exp;;


let and_expander_test_suite = [
    ("and base case 1", base_case);
    ("and base case 2", base_case_2);
    ("test simple and expansion", test_simple_and);
    ("test complex and expansion", test_complex_and)
];;

let () =
  let open Alcotest in
  run "Test Macro Expansions" [
      "test and expander", (List.map (fun (desc, test) -> test_case desc `Quick test) and_expander_test_suite)
      (* add more tests here *)
    ]