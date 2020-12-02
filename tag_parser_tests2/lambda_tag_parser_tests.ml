#use "topfind";;
#use "tag_parser_utils.ml";;
#require "alcotest";;

(* tests for Lambda *)
let lambda_test_1 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair (Nil, Pair (Number (Fraction (10, 1)), Nil))) ]) with X_syntax_error -> []) in
    let expected = [LambdaSimple ([], Const (Sexpr (Number (Fraction (10, 1)))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_2 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Pair (Symbol "a", Nil))) ]) with X_syntax_error -> []) in
    let expected = [LambdaSimple (["a"; "b"], Var "a")] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_3 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair (Pair (Symbol "a", Nil), Pair (Symbol "a", Nil))) ]) with X_syntax_error -> []) in
    let expected = [LambdaSimple (["a"], Var "a")] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_4 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),Pair (Pair (Symbol "+", Pair (Symbol "x", Pair (Symbol "y", Nil))), Nil)))]) with X_syntax_error -> []) in
    let expected = [LambdaSimple (["x"; "y"], Applic (Var "+", [Var "x"; Var "y"]))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_5 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil))),Pair(Pair (Symbol "if",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil)))),Nil)))]) with X_syntax_error -> []) in
    let expected = [LambdaSimple (["x"; "y"; "z"], If (Var "x", Var "y", Var "z"))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_6 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil))),Pair(Pair (Symbol "begin",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil)))),Nil)))]) with X_syntax_error -> []) in
    let expected = [LambdaSimple (["x"; "y"; "z"], Seq [Var "x"; Var "y"; Var "z"])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_7 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),Pair (Pair (Symbol "set", Pair (Symbol "x", Pair (Symbol "y", Nil))), Nil)))]) with X_syntax_error -> []) in
    let expected = [LambdaSimple (["x"; "y"], Applic (Var "set", [Var "x"; Var "y"]))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_8 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair(Pair (Symbol "x",Pair (Symbol "y", Pair (Symbol "z", Pair (Symbol "w", Nil)))),Pair(Pair (Symbol "if",Pair (Symbol "x",Pair (Pair (Symbol "+", Pair (Symbol "y", Pair (Symbol "z", Nil))),Pair (Pair (Symbol "+", Pair (Symbol "z", Pair (Symbol "w", Nil))), Nil)))),Nil)))]) with X_syntax_error -> []) in
    let expected = [LambdaSimple (["x"; "y"; "z"; "w"],If (Var "x", Applic (Var "+", [Var "y"; Var "z"]),Applic (Var "+", [Var "z"; Var "w"])))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_9 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),Pair (Pair (Symbol "or", Pair (Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil))), Nil)), Nil))) ]) with X_syntax_error -> []) in
    let expected = [LambdaSimple (["x"; "y"], Applic (Var "x", [Var "y"; Var "z"]))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_10 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "vs")),Pair(Pair (Symbol "begin",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "vs", Nil)))),Nil)))]) with X_syntax_error -> []) in
    let expected = [LambdaOpt (["x"; "y"], "vs", Seq [Var "x"; Var "y"; Var "vs"])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_11 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Symbol "vs"),Pair (Pair (Symbol "if", Pair (Symbol "x", Pair (Symbol "vs", Nil))), Nil)))]) with X_syntax_error -> []) in
    let expected = [LambdaOpt (["x"], "vs", If (Var "x", Var "vs", Const Void))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_12 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "vs")),Pair(Pair (Symbol "and",Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil)))),Nil)))]) with X_syntax_error -> []) in
    let expected = [LambdaOpt (["x"; "y"], "vs",If (Const (Sexpr (Number (Fraction (1, 1)))),If (Const (Sexpr (Number (Fraction (2, 1)))), Const (Sexpr (Number (Fraction (3, 1)))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_13 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair(Pair (Symbol "a",Pair (Symbol "b", Pair (Symbol "c", Pair (Symbol "d", Symbol "vs")))),Pair(Pair (Symbol "if",Pair (Pair (Symbol ">", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair (Pair (Symbol "+", Pair (Symbol "c", Pair (Symbol "d", Nil))),Pair (Pair (Symbol "list", Pair (Symbol "vs", Nil)), Nil)))),Nil)))]) with X_syntax_error -> []) in
    let expected = [LambdaOpt (["a"; "b"; "c"; "d"], "vs",If (Applic (Var ">", [Var "a"; Var "b"]),Applic (Var "+", [Var "c"; Var "d"]), Applic (Var "list", [Var "vs"])))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_14 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair (Pair (Symbol "b", Symbol "vs"),Pair(Pair (Symbol "begin",Pair (Symbol "b",Pair(Pair (Symbol "define", Pair (Symbol "x", Pair (Number (Fraction (10, 1)), Nil))),Pair(Pair (Symbol "set",Pair (Symbol "b",Pair(Pair (Symbol "+", Pair (Symbol "x", Pair (Number (Fraction (15, 1)), Nil))),Nil))),Nil)))),Nil)))]) with X_syntax_error -> []) in
    let expected = [LambdaOpt (["b"], "vs",Seq[Var "b"; Def (Var "x", Const (Sexpr (Number (Fraction (10, 1)))));Applic (Var "set",[Var "b"; Applic (Var "+", [Var "x"; Const (Sexpr (Number (Fraction (15, 1))))])])])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_15 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair (Pair (Symbol "a", Pair (Symbol "b", Symbol "vs")),Pair(Pair (Symbol "cond",Pair (Pair (Symbol "a", Pair (Number (Fraction (1, 1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction (2, 1)), Nil)),Pair(Pair (Symbol "else",Pair (Pair (Symbol "+", Pair (Symbol "a", Pair (Symbol "b", Nil))),Nil)),Nil)))),Nil)))]) with X_syntax_error -> []) in
    let expected = [LambdaOpt (["a"; "b"], "vs",If (Var "a", Const (Sexpr (Number (Fraction (1, 1)))),If (Var "b", Const (Sexpr (Number (Fraction (2, 1)))),Applic (Var "+", [Var "a"; Var "b"]))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_16 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Symbol "vs"), Pair (Symbol "vs", Nil))) ]) with X_syntax_error -> []) in
    let expected = [LambdaOpt (["x"], "vs", Var "vs")] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_17 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "vs")),Pair(Pair(Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "x", Nil)),Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "y", Nil)), Nil)),Nil)),Nil),Nil)))]) with X_syntax_error -> []) in
    let expected = [LambdaOpt (["x"; "y"], "vs",Applic(Applic (Var "cons",[Var "x"; Applic (Var "append", [Var "y"; Const (Sexpr Nil)])]),[]))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_18 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "vs")),Pair(Pair (Symbol "and",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "vs", Nil)))),Nil)))]) with X_syntax_error -> []) in
    let expected = [LambdaOpt (["x"; "y"], "vs",If (Var "x", If (Var "y", Var "vs", Const (Sexpr (Bool false))),Const (Sexpr (Bool false))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_19 = 
    let parsed = (try (Tag_Parser.tag_parse_expressions [Pair (Symbol "lambda",Pair (Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Symbol "d"))),Pair(Pair (Symbol "quasiquote",Pair(Pair(Pair (Symbol "a",Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)), Nil)),Pair(Pair (Symbol "b",Pair (Pair (Symbol "unquote", Pair (Symbol "b", Nil)), Nil)),Pair(Pair (Symbol "c",Pair (Pair (Symbol "unquote", Pair (Symbol "c", Nil)), Nil)),Pair(Pair (Symbol "d",Pair (Pair (Symbol "unquote", Pair (Symbol "d", Nil)), Nil)),Nil)))),Nil)),Nil)))]) with X_syntax_error -> []) in
    let expected = [LambdaOpt (["a"; "b"; "c"], "d",Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "cons", [Var "a"; Const (Sexpr Nil)])]);Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "b"));Applic (Var "cons", [Var "b"; Const (Sexpr Nil)])]);Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "c"));Applic (Var "cons", [Var "c"; Const (Sexpr Nil)])]);Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "d"));Applic (Var "cons", [Var "d"; Const (Sexpr Nil)])]);Const (Sexpr Nil)])])])]))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let lambda_test_suite = [
    ("lambda_test_1", lambda_test_1);
    ("lambda_test_2", lambda_test_2);
    ("lambda_test_3", lambda_test_3);
    ("lambda_test_4", lambda_test_4);
    ("lambda_test_5", lambda_test_5);
    ("lambda_test_6", lambda_test_6);
    ("lambda_test_7", lambda_test_7);
    ("lambda_test_8", lambda_test_8);
    ("lambda_test_9", lambda_test_9);
    ("lambda_test_10", lambda_test_10);
    ("lambda_test_11", lambda_test_11);
    ("lambda_test_12", lambda_test_12);
    ("lambda_test_13", lambda_test_13);
    ("lambda_test_14", lambda_test_14);
    ("lambda_test_15", lambda_test_15);
    ("lambda_test_16", lambda_test_16);
    ("lambda_test_17", lambda_test_17);
    ("lambda_test_18", lambda_test_18);
    ("lambda_test_19", lambda_test_19);
]

let () = 
    let open Alcotest in
    run "Test Lambda tag parser" ["test Lambda expressions", (List.map (fun (desc, test) -> test_case desc `Quick test) lambda_test_suite)]