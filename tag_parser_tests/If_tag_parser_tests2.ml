#use "topfind";;
#use "tag_parser_utils.ml";;
#require "alcotest";;

(* tests for If *)
let if_test_1 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "cond", Pair(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Pair(Pair (Number (Fraction (4, 1)), Pair (Number (Fraction (5, 1)), Pair (Number (Fraction (6, 1)), Nil))),Nil)))] in
    let expected = [If (Const (Sexpr (Number (Fraction (1, 1)))),Seq [Const (Sexpr (Number (Fraction (2, 1)))); Const (Sexpr (Number (Fraction (3, 1))))],If (Const (Sexpr (Number (Fraction (4, 1)))),Seq [Const (Sexpr (Number (Fraction (5, 1)))); Const (Sexpr (Number (Fraction (6, 1))))],Const Void))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let if_test_2 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "cond", Pair(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Pair(Pair (Number (Fraction (4, 1)), Pair (Number (Fraction (5, 1)), Pair (Number (Fraction (6, 1)), Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction (7, 1)), Pair (Number (Fraction (8, 1)), Pair (Number (Fraction (9, 1)), Nil)))),Nil))))] in
    let expected = [If (Const (Sexpr (Number (Fraction (1, 1)))),Seq [Const (Sexpr (Number (Fraction (2, 1)))); Const (Sexpr (Number (Fraction (3, 1))))],If (Const (Sexpr (Number (Fraction (4, 1)))),Seq [Const (Sexpr (Number (Fraction (5, 1)))); Const (Sexpr (Number (Fraction (6, 1))))],Seq[Const (Sexpr (Number (Fraction (7, 1)))); Const (Sexpr (Number (Fraction (8, 1))));Const (Sexpr (Number (Fraction (9, 1))))]))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let if_test_3 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "cond", Pair(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction (4, 1)), Pair (Number (Fraction (5, 1)), Pair (Number (Fraction (6, 1)), Nil)))),Pair(Pair (Number (Fraction (7, 1)), Pair (Number (Fraction (8, 1)), Pair (Number (Fraction (9, 1)), Nil))),Nil))))] in
    let expected = [If (Const (Sexpr (Number (Fraction (1, 1)))),Seq [Const (Sexpr (Number (Fraction (2, 1)))); Const (Sexpr (Number (Fraction (3, 1))))],Seq[Const (Sexpr (Number (Fraction (4, 1)))); Const (Sexpr (Number (Fraction (5, 1))));Const (Sexpr (Number (Fraction (6, 1))))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let if_test_4 = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "and", Pair (Number (Fraction (1, 1)),Pair (Number (Fraction (2, 1)),Pair (Number (Fraction (3, 1)),Pair (Number (Fraction (4, 1)),Pair (Number (Fraction (5, 1)),Pair (Number (Fraction (6, 1)),Pair (Number (Fraction (7, 1)),Pair (Number (Fraction (8, 1)),Pair (Number (Fraction (9, 1)), Pair (Number (Fraction (10, 1)), Nil)))))))))))] in
    let expected = [If (Const (Sexpr (Number (Fraction (1, 1)))),If (Const (Sexpr (Number (Fraction (2, 1)))),If (Const (Sexpr (Number (Fraction (3, 1)))),If (Const (Sexpr (Number (Fraction (4, 1)))),If (Const (Sexpr (Number (Fraction (5, 1)))),If (Const (Sexpr (Number (Fraction (6, 1)))),If (Const (Sexpr (Number (Fraction (7, 1)))),If (Const (Sexpr (Number (Fraction (8, 1)))),If (Const (Sexpr (Number (Fraction (9, 1)))), Const (Sexpr (Number (Fraction (10, 1)))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false)))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let if_test_suite = [
    ("if_test_1", if_test_1);
    ("if_test_2", if_test_2);
    ("if_test_3", if_test_3);
    ("if_test_4", if_test_4);
]

let () = 
    let open Alcotest in
    run "Test If tag parser" ["test If expressions", (List.map (fun (desc, test) -> test_case desc `Quick test) if_test_suite)]