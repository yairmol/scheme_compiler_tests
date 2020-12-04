#use "topfind";;
#use "tag_parser_utils.ml";;
#require "alcotest";;

(* tests for Define *)

let simple_define_test () =
  let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "define", Pair (Symbol "var", Pair (Bool true, Nil)))] in
  let expected = [Def (Var "var", Const (Sexpr (Bool true)))] in
      Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;

let complex_define_test () = 
  let parsed = Tag_Parser.tag_parse_expressions [
    Pair (Symbol "define", 
      Pair (Symbol "var",
      Pair (
        Pair (Symbol "if", 
          Pair (Pair (Symbol "or", Pair (Bool false, Pair (Bool true, Pair (Number (Fraction (1, 1)), Nil)))),
          Pair (Char 'c', 
          Pair (String "well", Nil)))),
    Nil)))
  ] in
  let expected = [
    Def (Var "var", 
      If (
        Or [
          Const (Sexpr (Bool false));
          Const (Sexpr (Bool true));
          Const (Sexpr (Number (Fraction (1, 1))))
        ], 
        Const (Sexpr (Char 'c')), Const (Sexpr (String "well"))))] in
      Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;

(* test MIT Define macro *)
let mit_def_test_1 () = 
  let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "define",Pair (Pair (Symbol "square", Pair (Symbol "x", Nil)),Pair (Pair (Symbol "*", Pair (Symbol "x", Pair (Symbol "x", Nil))), Nil)))] in
  let expected = [Def (Var "square",LambdaSimple (["x"], Applic (Var "*", [Var "x"; Var "x"])))] in
      Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let mit_def_test_2 () = 
  let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "define",Pair (Pair (Symbol "cmp", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "if",Pair (Pair (Symbol ">", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (-1, 1)), Nil)))),Nil)))] in
  let expected = [Def (Var "cmp",LambdaSimple (["a"; "b"],If (Applic (Var ">", [Var "a"; Var "b"]), Const (Sexpr (Number (Fraction (1, 1)))),Const (Sexpr (Number (Fraction (-1, 1)))))))] in
      Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let mit_def_test_3 () = 
  let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "define",Pair(Pair (Symbol "square",Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil)))),Pair(Pair (Symbol "*",Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil)))),Nil)))] in
  let expected = [Def (Var "square",LambdaSimple (["a"; "b"; "c"],Applic (Var "*", [Var "a"; Var "b"; Var "c"])))] in
      Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let mit_def_test_4 () = 
  let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "define",Pair (Pair (Symbol "returnonly", Pair (Symbol "x", Nil)),Pair(Pair (Symbol "begin",Pair (String "return only", Pair (Symbol "x", Nil))),Nil)))] in
  let expected = [Def (Var "returnonly",LambdaSimple (["x"], Seq [Const (Sexpr (String "return only")); Var "x"]))] in
      Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let mit_def_test_5 () = 
  let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "define",Pair(Pair (Symbol "applic",Pair (Symbol "fun",Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "e", Nil))))))),Pair(Pair (Symbol "fun",Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "e", Nil)))))),Nil)))] in
  let expected = [Def (Var "applic",LambdaSimple (["fun"; "a"; "b"; "c"; "d"; "e"],Applic (Var "fun", [Var "a"; Var "b"; Var "c"; Var "d"; Var "e"])))] in
      Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let mit_def_test_6 () = 
  let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "define",Pair(Pair (Symbol "if_fun",Pair (Symbol "if_test",Pair (Symbol "if_then", Pair (Symbol "if_else", Nil)))),Pair(Pair (Symbol "if",Pair (Symbol "if_test",Pair (Symbol "if_then", Pair (Symbol "if_else", Nil)))),Nil)))] in
  let expected = [Def (Var "if_fun",LambdaSimple (["if_test"; "if_then"; "if_else"],If (Var "if_test", Var "if_then", Var "if_else")))] in
      Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let mit_def_test_7 () = 
  let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "define",Pair (Pair (Symbol "pairing", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "quote",Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Nil)),Nil)))] in
  let expected = [Def (Var "pairing",LambdaSimple (["a"; "b"],Const (Sexpr (Pair (Symbol "a", Pair (Symbol "b", Nil))))))] in
      Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;


let def_test_suite = [
  ("simple define test", simple_define_test);
  ("complex define test", complex_define_test);
  ("mit_def_test_1", mit_def_test_1);
  ("mit_def_test_2", mit_def_test_2);
  ("mit_def_test_3", mit_def_test_3);
  ("mit_def_test_4", mit_def_test_4);
  ("mit_def_test_5", mit_def_test_5);
  ("mit_def_test_6", mit_def_test_6);
  ("mit_def_test_7", mit_def_test_7);
]

let () = 
  let open Alcotest in
  run "Test Define tag parser" ["test Define expressions", (List.map (fun (desc, test) -> test_case desc `Quick test) def_test_suite)]