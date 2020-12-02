#use "topfind";;
#use "tag_parser_utils.ml";;
#require "alcotest";;

(* tests for Const *)
let const_test_1 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote",Pair(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Nil))] in
    let expected = [Const(Sexpr(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil)))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_2 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote",Pair (Pair (Number (Float 1.2), Number (Float 3.4)), Nil))] in
    let expected = [Const (Sexpr (Pair (Number (Float 1.2), Number (Float 3.4))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_3 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote",Pair(Pair(Pair (Number (Float (-0.5)),Pair (Number (Float 999.99), Pair (Number (Float (-12.1)), Nil))),Pair (Number (Float 10.),Pair (Pair (Number (Fraction (1, 1)), Number (Fraction (2, 1))), Number (Float 1.)))),Nil))] in
    let expected = [Const(Sexpr(Pair(Pair (Number (Float (-0.5)),Pair (Number (Float 999.99), Pair (Number (Float (-12.1)), Nil))),Pair (Number (Float 10.),Pair (Pair (Number (Fraction (1, 1)), Number (Fraction (2, 1))), Number (Float 1.))))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_4 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote",Pair(Pair (Number (Float (-0.)),Pair (Number (Float 999.123),Pair (Pair (Number (Float 501.1), Number (Float 0.5)),Number (Float (-0.555))))),Nil))] in
    let expected = [Const(Sexpr(Pair (Number (Float (-0.)),Pair (Number (Float 999.123),Pair (Pair (Number (Float 501.1), Number (Float 0.5)),Number (Float (-0.555)))))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_5 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote",Pair(Pair (Number (Float (-0.52)),Pair (Pair (Number (Float 0.5234), Number (Float (-123.4))),Number (Float (-0.535)))),Nil))] in
    let expected = [Const(Sexpr(Pair (Number (Float (-0.52)),Pair (Pair (Number (Float 0.5234), Number (Float (-123.4))),Number (Float (-0.535))))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_6 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote", Pair (Pair (Number (Float 0.123), Nil), Nil))] in
    let expected = [Const (Sexpr (Pair (Number (Float 0.123), Nil)))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_7 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote", Pair (Pair (Char '\000', Nil), Nil))] in
    let expected = [Const (Sexpr (Pair (Char '\000', Nil)))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_8 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Char '\n'] in
    let expected = [Const (Sexpr (Char '\n'))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_9 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote",Pair(Pair (Number (Fraction (1, 1)),Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Pair (Char '\012', Nil)))),Nil))] in
    let expected = [Const(Sexpr(Pair (Number (Fraction (1, 1)),Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Pair (Char '\012', Nil))))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_10 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote",Pair(Pair (Pair (Char '\t', Nil),Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Nil))),Nil))] in
    let expected = [Const(Sexpr(Pair (Pair (Char '\t', Nil),Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Nil)))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_11 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote",Pair(Pair (String "a",Pair (String "b", Pair (Pair (String "c", String "d"), String "e"))),Nil))] in
    let expected = [Const(Sexpr(Pair (String "a",Pair (String "b", Pair (Pair (String "c", String "d"), String "e")))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_12 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote",Pair(Pair (String "hello",Pair (String "world",Pair (Pair (Number (Float 1.2), Number (Fraction (3, 1))), Char '\000'))),Nil))] in
    let expected = [Const(Sexpr(Pair (String "hello",Pair (String "world",Pair (Pair (Number (Float 1.2), Number (Fraction (3, 1))), Char '\000')))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_13 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote",Pair (Pair (String "should", Pair (String "be", String "list")), Nil))] in
    let expected = [Const (Sexpr (Pair (String "should", Pair (String "be", String "list"))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_14 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote",Pair(Pair (String "vary",Pair (String "long",Pair (Number (Fraction (0, 1)),Pair (Number (Float 2.1),Pair (Number (Float (-4.2)),Pair (String "complex", Pair (Char '\r', Pair (String "list", Nil)))))))),Nil))] in
    let expected = [Const(Sexpr(Pair (String "vary",Pair (String "long",Pair (Number (Fraction (0, 1)),Pair (Number (Float 2.1),Pair (Number (Float (-4.2)),Pair (String "complex",Pair (Char '\r', Pair (String "list", Nil))))))))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_15 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote",Pair(Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "f", Symbol "g"))))),Nil))] in
    let expected = [Const(Sexpr(Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "f", Symbol "g")))))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_16 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote",Pair(Pair (String "a long long string",Pair (String "another long one",Pair(Pair (String "some numbers",Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Number (Fraction (3, 1))))),Pair (String "Named Char", Char ' ')))),Nil))] in
    let expected = [Const(Sexpr(Pair (String "a long long string",Pair (String "another long one",Pair(Pair (String "some numbers",Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Number (Fraction (3, 1))))),Pair (String "Named Char", Char ' '))))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_17 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote",Pair(Pair (Number (Float (-0.2)),Pair (Number (Float 1.5),Pair(Pair (Number (Fraction (1, 1)),Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Pair (Pair (Number (Fraction (4, 1)), Number (Fraction (5, 1))),Pair (Pair (Symbol "a", Pair (Symbol "b", Symbol "c")),Pair (Symbol "d", Symbol "f")))))),Nil))] in
    let expected = [Const(Sexpr(Pair (Number (Float (-0.2)),Pair (Number (Float 1.5),Pair(Pair (Number (Fraction (1, 1)),Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Pair (Pair (Number (Fraction (4, 1)), Number (Fraction (5, 1))),Pair (Pair (Symbol "a", Pair (Symbol "b", Symbol "c")),Pair (Symbol "d", Symbol "f"))))))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_18 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote",Pair(Pair (Number (Float 0.1),Pair (Number (Float (-0.2)),Pair (Number (Float 3.4),Pair (Number (Fraction (5, 1)),Pair (Number (Float 6.7), Pair (Number (Fraction (8, 1)), Number (Fraction (9, 1)))))))),Nil))] in
    let expected = [Const(Sexpr(Pair (Number (Float 0.1),Pair (Number (Float (-0.2)),Pair (Number (Float 3.4),Pair (Number (Fraction (5, 1)),Pair (Number (Float 6.7), Pair (Number (Fraction (8, 1)), Number (Fraction (9, 1))))))))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_19 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote",Pair(Pair (Number (Fraction (1, 1)),Pair (Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil)),Pair (Pair (Number (Fraction (4, 1)), Pair (Number (Float 0.5), Nil)),Pair(Pair (Symbol "c",Pair (Symbol "d",Pair (Pair (Symbol "f", Symbol "g"), String "Hello World"))),Symbol "z")))),Nil))] in
    let expected = [Const(Sexpr(Pair (Number (Fraction (1, 1)),Pair (Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil)),Pair (Pair (Number (Fraction (4, 1)), Pair (Number (Float 0.5), Nil)),Pair(Pair (Symbol "c",Pair (Symbol "d",Pair (Pair (Symbol "f", Symbol "g"), String "Hello World"))),Symbol "z"))))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_20 () = 
    let parsed = Tag_Parser.tag_parse_expressions [String ""] in
    let expected = [Const (Sexpr (String ""))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_21 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote",Pair (Pair (Number (Float 1.2), Pair (Number (Float 3.4), Nil)), Nil))] in
    let expected = [Const (Sexpr (Pair (Number (Float 1.2), Pair (Number (Float 3.4), Nil))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_22 () = 
    let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote", Pair(Pair (Pair (Char '\t', Nil), Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Char '\000'))),Nil))] in
    let expected = [Const(Sexpr(Pair (Pair (Char '\t', Nil), Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Char '\000')))))] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;
let const_test_suite = [
    ("const_test_1", const_test_1);
    ("const_test_2", const_test_2);
    ("const_test_3", const_test_3);
    ("const_test_4", const_test_4);
    ("const_test_5", const_test_5);
    ("const_test_6", const_test_6);
    ("const_test_7", const_test_7);
    ("const_test_8", const_test_8);
    ("const_test_9", const_test_9);
    ("const_test_10", const_test_10);
    ("const_test_11", const_test_11);
    ("const_test_12", const_test_12);
    ("const_test_13", const_test_13);
    ("const_test_14", const_test_14);
    ("const_test_15", const_test_15);
    ("const_test_16", const_test_16);
    ("const_test_17", const_test_17);
    ("const_test_18", const_test_18);
    ("const_test_19", const_test_19);
    ("const_test_20", const_test_20);
    ("const_test_21", const_test_21);
    ("const_test_22", const_test_22);
]

let () = 
    let open Alcotest in
    run "Test Const tag parser" ["test Const expressions", (List.map (fun (desc, test) -> test_case desc `Quick test) const_test_suite)]