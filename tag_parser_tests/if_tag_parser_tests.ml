#use "topfind";;
#use "tag_parser_utils.ml";;
#require "alcotest";;

let test_simple_if () = 
  let parsed = Tag_Parser.tag_parse_expressions [
    Pair (Symbol "if", 
      Pair (Pair (Symbol "quote", Pair (Symbol "test", Nil)),
      Pair (Pair (Symbol "quote", Pair (Symbol "then", Nil)),
      Pair (Pair (Symbol "quote", Pair (Symbol "else", Nil)), Nil))))] in
  let expected = [
    If (Const (Sexpr (Symbol "test")),
        Const (Sexpr (Symbol "then")),
        Const (Sexpr (Symbol "else")))] in
    Alcotest.(check (list expr_testable)) "same expr?" expected parsed;; 

let test_simple_if_no_else () = 
  let parsed = Tag_Parser.tag_parse_expressions [
    Pair (Symbol "if", 
      Pair (Pair (Symbol "quote", Pair (Symbol "well", Nil)),
      Pair (Pair (Symbol "quote", Pair (Symbol "you-know", Nil)), Nil)))] in
  let expected = [
    If (Const (Sexpr (Symbol "well")),
        Const (Sexpr (Symbol "you-know")),
        Const Void)] in
    Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;

 let test_nested_if () = 
  let parsed = Tag_Parser.tag_parse_expressions [
    Pair (Symbol "if", 
      Pair (Pair (Symbol "quote", Pair (Symbol "test", Nil)),
      Pair (Pair (Symbol "quote", Pair (Symbol "then", Nil)),
      Pair (Pair (Symbol "if",
        Pair (Number (Fraction (1, 1)),
        Pair (Number (Fraction (2, 1)),
        Pair (Number (Fraction (3, 1)), Nil)))), Nil))))] in
  let expected = [
    If (Const (Sexpr (Symbol "test")),
        Const (Sexpr (Symbol "then")),
        If (Const (Sexpr (Number (Fraction (1,1)))),
            Const (Sexpr (Number (Fraction (2,1)))),
            Const (Sexpr (Number (Fraction (3,1))))))] in
    Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;

let test_nested_if_no_else () = 
  let parsed = Tag_Parser.tag_parse_expressions [
    Pair (Symbol "if", 
      Pair (Pair (Symbol "quote", Pair (Symbol "ocaml?", Nil)),
      Pair (Pair (Symbol "quote", Pair (Symbol "good", Nil)),
      Pair (Pair (Symbol "if",
        Pair (Pair (Symbol "quote", Pair (Symbol "java?", Nil)),
        Pair (Pair (Symbol "quote", Pair (Symbol "bad", Nil)), Nil))), Nil))))] in
  let expected = [
    If (Const (Sexpr (Symbol "ocaml?")),
        Const (Sexpr (Symbol "good")),
        If (Const (Sexpr (Symbol "java?")),
            Const (Sexpr (Symbol "bad")),
            Const Void))] in
    Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;

let if_test_1 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "cond", Pair(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Pair(Pair (Number (Fraction (4, 1)), Pair (Number (Fraction (5, 1)), Pair (Number (Fraction (6, 1)), Nil))),Nil)))] in
    let expected = [If (Const (Sexpr (Number (Fraction (1, 1)))),Seq [Const (Sexpr (Number (Fraction (2, 1)))); Const (Sexpr (Number (Fraction (3, 1))))],If (Const (Sexpr (Number (Fraction (4, 1)))),Seq [Const (Sexpr (Number (Fraction (5, 1)))); Const (Sexpr (Number (Fraction (6, 1))))],Const Void))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let if_test_2 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "cond", Pair(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Pair(Pair (Number (Fraction (4, 1)), Pair (Number (Fraction (5, 1)), Pair (Number (Fraction (6, 1)), Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction (7, 1)), Pair (Number (Fraction (8, 1)), Pair (Number (Fraction (9, 1)), Nil)))),Nil))))] in
    let expected = [If (Const (Sexpr (Number (Fraction (1, 1)))),Seq [Const (Sexpr (Number (Fraction (2, 1)))); Const (Sexpr (Number (Fraction (3, 1))))],If (Const (Sexpr (Number (Fraction (4, 1)))),Seq [Const (Sexpr (Number (Fraction (5, 1)))); Const (Sexpr (Number (Fraction (6, 1))))],Seq[Const (Sexpr (Number (Fraction (7, 1)))); Const (Sexpr (Number (Fraction (8, 1))));Const (Sexpr (Number (Fraction (9, 1))))]))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let if_test_3 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "cond", Pair(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction (4, 1)), Pair (Number (Fraction (5, 1)), Pair (Number (Fraction (6, 1)), Nil)))),Pair(Pair (Number (Fraction (7, 1)), Pair (Number (Fraction (8, 1)), Pair (Number (Fraction (9, 1)), Nil))),Nil))))] in
    let expected = [If (Const (Sexpr (Number (Fraction (1, 1)))),Seq [Const (Sexpr (Number (Fraction (2, 1)))); Const (Sexpr (Number (Fraction (3, 1))))],Seq[Const (Sexpr (Number (Fraction (4, 1)))); Const (Sexpr (Number (Fraction (5, 1))));Const (Sexpr (Number (Fraction (6, 1))))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let if_test_4 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "and", Pair (Number (Fraction (1, 1)),Pair (Number (Fraction (2, 1)),Pair (Number (Fraction (3, 1)),Pair (Number (Fraction (4, 1)),Pair (Number (Fraction (5, 1)),Pair (Number (Fraction (6, 1)),Pair (Number (Fraction (7, 1)),Pair (Number (Fraction (8, 1)),Pair (Number (Fraction (9, 1)), Pair (Number (Fraction (10, 1)), Nil)))))))))))] in
    let expected = [If (Const (Sexpr (Number (Fraction (1, 1)))),If (Const (Sexpr (Number (Fraction (2, 1)))),If (Const (Sexpr (Number (Fraction (3, 1)))),If (Const (Sexpr (Number (Fraction (4, 1)))),If (Const (Sexpr (Number (Fraction (5, 1)))),If (Const (Sexpr (Number (Fraction (6, 1)))),If (Const (Sexpr (Number (Fraction (7, 1)))),If (Const (Sexpr (Number (Fraction (8, 1)))),If (Const (Sexpr (Number (Fraction (9, 1)))), Const (Sexpr (Number (Fraction (10, 1)))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false)))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;

(* Name the test cases and group them together *)
let tag_constant_parser_tester_suite =[
  ("test simple if", test_simple_if);
  ("test simple if no else", test_simple_if_no_else);
  ("test nested if", test_nested_if);
  ("test nested if with no else", test_nested_if_no_else);
  ("if_test_1", if_test_1);
  ("if_test_2", if_test_2);
  ("if_test_3", if_test_3);
  ("if_test_4", if_test_4);
]
;;

let () =
  let open Alcotest in
  run "Test If expressions tag parser" [
      "test if expressions", (List.map (fun (desc, test) -> test_case desc `Quick test) tag_constant_parser_tester_suite)
      (* add more tests here *)
    ]

