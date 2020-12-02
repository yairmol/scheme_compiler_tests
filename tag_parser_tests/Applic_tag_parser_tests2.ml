#use "topfind";;
#use "tag_parser_utils.ml";;
#require "alcotest";;

(* tests for Applic *)
let applic_test_1 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Fraction (1, 1)), Nil)), Pair (Pair (Symbol "y", Pair (Number (Fraction (2, 1)), Nil)), Nil)), Pair (Symbol "y", Nil)))] in
    let expected = [Applic (LambdaSimple (["x"; "y"], Var "y"),[Const (Sexpr (Number (Fraction (1, 1)))); Const (Sexpr (Number (Fraction (2, 1))))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_2 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let", Pair (Nil, Pair (Number (Fraction (10, 1)), Nil)))] in
    let expected = [Applic (LambdaSimple ([], Const (Sexpr (Number (Fraction (10, 1))))), [])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_3 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Fraction (1, 1)), Nil)), Nil), Pair (Symbol "x", Nil)))] in
    let expected = [Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Number (Fraction (1, 1))))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_4 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let", Pair (Nil, Pair (Pair (Symbol "begin", Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil)))), Nil)))] in
    let expected = [Applic(LambdaSimple ([],Seq[Const (Sexpr (Number (Fraction (1, 1)))); Const (Sexpr (Number (Fraction (2, 1))));Const (Sexpr (Number (Fraction (3, 1))))]),[])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_5 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let", Pair (Pair (Pair (Symbol "a", Pair (Number (Fraction (3, 1)), Nil)), Pair (Pair (Symbol "b", Pair (Number (Fraction (10, 1)), Nil)), Nil)), Pair (Pair (Symbol "+", Pair (Symbol "a", Pair (Symbol "b", Nil))), Nil)))] in
    let expected = [Applic (LambdaSimple (["a"; "b"], Applic (Var "+", [Var "a"; Var "b"])),[Const (Sexpr (Number (Fraction (3, 1)))); Const (Sexpr (Number (Fraction (10, 1))))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_6 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Char 'a', Nil)), Nil), Pair (Symbol "x", Nil)))] in
    let expected = [Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Char 'a'))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_7 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let", Pair (Pair (Pair (Symbol "t", Pair (Bool true, Nil)), Pair (Pair (Symbol "th", Pair (Number (Fraction (3, 1)), Nil)), Pair (Pair (Symbol "el", Pair (Number (Fraction (4, 1)), Nil)), Nil))), Pair (Pair (Symbol "if", Pair (Symbol "t", Pair (Symbol "th", Pair (Symbol "el", Nil)))), Nil)))] in
    let expected = [Applic (LambdaSimple (["t"; "th"; "el"], If (Var "t", Var "th", Var "el")),[Const (Sexpr (Bool true)); Const (Sexpr (Number (Fraction (3, 1))));Const (Sexpr (Number (Fraction (4, 1))))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_8 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil), Pair (Pair (Symbol "define", Pair (Symbol "y", Pair (Number (Float 1.23), Nil))), Nil)))] in
    let expected = [Applic(LambdaSimple (["x"], Def (Var "y", Const (Sexpr (Number (Float 1.23))))),[Const (Sexpr (String "asd"))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_9 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil), Pair (Pair (Symbol "begin", Pair (Pair (Symbol "define", Pair (Symbol "y", Pair (Number (Float 1.23), Nil))), Pair (Pair (Symbol "set", Pair (Symbol "y", Pair (Number (Fraction (-1, 1)), Nil))), Nil))), Nil)))] in
    let expected = [Applic(LambdaSimple (["x"],Seq[Def (Var "y", Const (Sexpr (Number (Float 1.23))));Applic (Var "set", [Var "y"; Const (Sexpr (Number (Fraction (-1, 1))))])]),[Const (Sexpr (String "asd"))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_10 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil), Pair (Pair (Symbol "begin", Pair (Symbol "x", Nil)), Nil)))] in
    let expected = [Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (String "asd"))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_11 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair (Pair (Pair (Symbol "unquote", Pair (Symbol "x", Nil)), Nil), Nil)) ] in
    let expected = [Applic (Var "cons", [Var "x"; Const (Sexpr Nil)])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_12 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Nil)) ] in
    let expected = [Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_13 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),Pair (Symbol "b", Nil)),Nil))] in
    let expected = [Applic (Var "cons",[Var "a";Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_14 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair (Symbol "a",Pair (Pair (Symbol "unquote", Pair (Symbol "b", Nil)), Nil)),Nil))] in
    let expected = [Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "cons", [Var "b"; Const (Sexpr Nil)])])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_15 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Symbol "b", Nil)),Nil))] in
    let expected = [Applic (Var "append",[Var "a";Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_16 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair (Symbol "a",Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))] in
    let expected = [Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_17 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))] in
    let expected = [Applic (Var "cons",[Var "a"; Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_18 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))] in
    let expected = [Applic (Var "append",[Var "a"; Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_19 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Symbol "unquote", Pair (Symbol "b", Nil))),Nil))] in
    let expected = [Applic (Var "append", [Var "a"; Var "b"])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_20 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil))),Nil))] in
    let expected = [Applic (Var "cons", [Var "a"; Var "b"])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_21 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair(Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)), Nil),Nil),Nil),Nil))] in
    let expected = [Applic (Var "cons",[Applic (Var "cons",[Applic (Var "append", [Var "a"; Const (Sexpr Nil)]); Const (Sexpr Nil)]);Const (Sexpr Nil)])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_22 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let*", Pair(Pair (Pair (Symbol "x", Pair (Number (Fraction (1, 1)), Nil)),Pair (Pair (Symbol "y", Pair (Number (Fraction (2, 1)), Nil)), Nil)),Pair (Symbol "y", Nil)))] in
    let expected = [Applic(LambdaSimple (["x"],Applic (LambdaSimple (["y"], Var "y"), [Const (Sexpr (Number (Fraction (2, 1))))])),[Const (Sexpr (Number (Fraction (1, 1))))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_23 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let*", Pair(Pair (Pair (Symbol "x", Pair (Number (Fraction (1, 1)), Nil)),Pair (Pair (Symbol "y", Pair (Number (Fraction (2, 1)), Nil)),Pair (Pair (Symbol "z", Pair (Number (Fraction (3, 1)), Nil)),Pair (Pair (Symbol "a", Pair (Number (Fraction (4, 1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction (5, 1)), Nil)),Pair (Pair (Symbol "c", Pair (Number (Fraction (6, 1)), Nil)), Nil)))))),Pair(Pair (Symbol "begin",Pair (Symbol "x",Pair (Symbol "y",Pair (Symbol "z",Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil))))))),Nil)))] in
    let expected = [Applic(LambdaSimple (["x"],Applic(LambdaSimple (["y"],Applic(LambdaSimple (["z"],Applic(LambdaSimple (["a"],Applic(LambdaSimple (["b"],Applic(LambdaSimple (["c"],Seq [Var "x"; Var "y"; Var "z"; Var "a"; Var "b"; Var "c"]),[Const (Sexpr (Number (Fraction (6, 1))))])),[Const (Sexpr (Number (Fraction (5, 1))))])),[Const (Sexpr (Number (Fraction (4, 1))))])),[Const (Sexpr (Number (Fraction (3, 1))))])),[Const (Sexpr (Number (Fraction (2, 1))))])),[Const (Sexpr (Number (Fraction (1, 1))))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_24 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let*", Pair(Pair (Pair (Symbol "a", Pair (Number (Fraction (1, 1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction (2, 1)), Nil)),Pair (Pair (Symbol "c", Pair (Number (Fraction (3, 1)), Nil)),Pair (Pair (Symbol "d", Pair (Number (Fraction (4, 1)), Nil)),Pair (Pair (Symbol "e", Pair (Number (Fraction (5, 1)), Nil)),Pair (Pair (Symbol "f", Pair (Number (Fraction (5, 1)), Nil)),Pair (Pair (Symbol "g", Pair (Number (Fraction (6, 1)), Nil)), Nil))))))),Pair(Pair (Symbol "and",Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c",Pair (Symbol "d",Pair (Symbol "e", Pair (Symbol "f", Pair (Symbol "g", Nil)))))))),Nil)))] in
    let expected = [Applic(LambdaSimple (["a"],Applic(LambdaSimple (["b"],Applic(LambdaSimple (["c"],Applic(LambdaSimple (["d"],Applic(LambdaSimple (["e"],Applic(LambdaSimple (["f"],Applic(LambdaSimple (["g"],If (Var "a",If (Var "b",If (Var "c",If (Var "d",If (Var "e",If (Var "f", Var "g", Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false)))),[Const (Sexpr (Number (Fraction (6, 1))))])),[Const (Sexpr (Number (Fraction (5, 1))))])),[Const (Sexpr (Number (Fraction (5, 1))))])),[Const (Sexpr (Number (Fraction (4, 1))))])),[Const (Sexpr (Number (Fraction (3, 1))))])),[Const (Sexpr (Number (Fraction (2, 1))))])),[Const (Sexpr (Number (Fraction (1, 1))))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_25 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let*", Pair (Nil,Pair(Pair (Symbol "begin",Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil)))),Nil)))] in
    let expected = [Applic(LambdaSimple ([],Seq[Const (Sexpr (Number (Fraction (1, 1)))); Const (Sexpr (Number (Fraction (2, 1))));Const (Sexpr (Number (Fraction (3, 1))))]),[])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_26 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let*", Pair(Pair (Pair (Symbol "a", Pair (Number (Fraction (3, 1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction (10, 1)), Nil)), Nil)),Pair (Pair (Symbol "+", Pair (Symbol "a", Pair (Symbol "b", Nil))), Nil)))] in
    let expected = [Applic(LambdaSimple (["a"],Applic (LambdaSimple (["b"], Applic (Var "+", [Var "a"; Var "b"])),[Const (Sexpr (Number (Fraction (10, 1))))])),[Const (Sexpr (Number (Fraction (3, 1))))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_27 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let*", Pair (Pair (Pair (Symbol "x", Pair (Char 'a', Nil)), Nil),Pair (Symbol "x", Nil)))] in
    let expected = [Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Char 'a'))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_28 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let*", Pair(Pair (Pair (Symbol "t", Pair (Bool true, Nil)),Pair (Pair (Symbol "th", Pair (Number (Fraction (3, 1)), Nil)),Pair (Pair (Symbol "el", Pair (Number (Fraction (4, 1)), Nil)), Nil))),Pair(Pair (Symbol "if",Pair (Bool true, Pair (Number (Fraction (3, 1)), Pair (Number (Fraction (4, 1)), Nil)))),Nil)))] in
    let expected = [Applic(LambdaSimple (["t"],Applic(LambdaSimple (["th"],Applic(LambdaSimple (["el"],If (Const (Sexpr (Bool true)), Const (Sexpr (Number (Fraction (3, 1)))),Const (Sexpr (Number (Fraction (4, 1)))))),[Const (Sexpr (Number (Fraction (4, 1))))])),[Const (Sexpr (Number (Fraction (3, 1))))])),[Const (Sexpr (Bool true))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_29 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let*", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil),Pair(Pair (Symbol "define", Pair (Symbol "y", Pair (Number (Float 12.3), Nil))),Nil)))] in
    let expected = [Applic(LambdaSimple (["x"], Def (Var "y", Const (Sexpr (Number (Float 12.3))))),[Const (Sexpr (String "asd"))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_30 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let*", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil),Pair(Pair (Symbol "begin",Pair(Pair (Symbol "define",Pair (Symbol "y", Pair (Number (Float 1.23), Nil))),Pair(Pair (Symbol "set", Pair (Symbol "y", Pair (Number (Fraction (-1, 1)), Nil))),Nil))),Nil)))] in
    let expected = [Applic(LambdaSimple (["x"],Seq[Def (Var "y", Const (Sexpr (Number (Float 1.23))));Applic (Var "set", [Var "y"; Const (Sexpr (Number (Fraction (-1, 1))))])]),[Const (Sexpr (String "asd"))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_31 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "let*", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil),Pair (Pair (Symbol "begin", Pair (Symbol "x", Nil)), Nil)))] in
    let expected = [Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (String "asd"))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_32 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "cond", Pair (Pair (Symbol "a", Pair (Symbol "=>", Pair (Symbol "b", Nil))), Nil)) ] in
    let expected = [Applic(LambdaSimple (["value"; "f"],If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),Const Void)),[Var "a"; LambdaSimple ([], Var "b")])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_33 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "cond", Pair (Pair (Symbol "a", Pair (Symbol "=>", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil)))),Nil)))] in
    let expected = [Applic(LambdaSimple (["value"; "f"; "rest"],If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),Applic (Var "rest", []))),[Var "a"; LambdaSimple ([], Var "b");LambdaSimple ([],Seq[Const (Sexpr (Number (Fraction (1, 1)))); Const (Sexpr (Number (Fraction (2, 1))));Const (Sexpr (Number (Fraction (3, 1))))])])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_34 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (String "should not", Pair (String "be", Pair (String "list", Nil)))] in
    let expected = [Applic (Const (Sexpr (String "should not")),[Const (Sexpr (String "be")); Const (Sexpr (String "list"))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let applic_test_suite = [
    ("applic_test_1", applic_test_1);
    ("applic_test_2", applic_test_2);
    ("applic_test_3", applic_test_3);
    ("applic_test_4", applic_test_4);
    ("applic_test_5", applic_test_5);
    ("applic_test_6", applic_test_6);
    ("applic_test_7", applic_test_7);
    ("applic_test_8", applic_test_8);
    ("applic_test_9", applic_test_9);
    ("applic_test_10", applic_test_10);
    ("applic_test_11", applic_test_11);
    ("applic_test_12", applic_test_12);
    ("applic_test_13", applic_test_13);
    ("applic_test_14", applic_test_14);
    ("applic_test_15", applic_test_15);
    ("applic_test_16", applic_test_16);
    ("applic_test_17", applic_test_17);
    ("applic_test_18", applic_test_18);
    ("applic_test_19", applic_test_19);
    ("applic_test_20", applic_test_20);
    ("applic_test_21", applic_test_21);
    ("applic_test_22", applic_test_22);
    ("applic_test_23", applic_test_23);
    ("applic_test_24", applic_test_24);
    ("applic_test_25", applic_test_25);
    ("applic_test_26", applic_test_26);
    ("applic_test_27", applic_test_27);
    ("applic_test_28", applic_test_28);
    ("applic_test_29", applic_test_29);
    ("applic_test_30", applic_test_30);
    ("applic_test_31", applic_test_31);
    ("applic_test_32", applic_test_32);
    ("applic_test_33", applic_test_33);
    ("applic_test_34", applic_test_34);
]

let () = 
    let open Alcotest in
    run "Test Applic tag parser" ["test Applic expressions", (List.map (fun (desc, test) -> test_case desc `Quick test) applic_test_suite)]