#use "topfind";;

#use "reader.ml";;
#use "sexp_test_utils.ml";;
#require "alcotest";;

let simple_quote_test () = Alcotest.(check (list sexp_testable)) "same quote sexp" [Pair ((Symbol "quote"), Pair ((Bool true), Nil))] (Reader.read_sexprs "'#t");;
let simple_quasiquote_test () = Alcotest.(check (list sexp_testable)) "same quote sexp" [Pair ((Symbol "quasiquote"), Pair ((Bool true), Nil))] (Reader.read_sexprs "`#t");;
let simple_unquote_test () = Alcotest.(check (list sexp_testable)) "same quote sexp" [Pair ((Symbol "unquote"), Pair ((Bool true), Nil))] (Reader.read_sexprs ",#t");;
let simple_unquotesplice_test () = Alcotest.(check (list sexp_testable)) "same quote sexp" [Pair ((Symbol "unquote-splicing"), Pair ((Bool true), Nil))] (Reader.read_sexprs ",@#t");;

let simple_quote_test_with_spaces () = Alcotest.(check (list sexp_testable)) "same quote sexp" [Pair ((Symbol "quote"), Pair ((Bool true), Nil))] (Reader.read_sexprs "   '  #t  ");;
let simple_quasiquote_test_with_spaces () = Alcotest.(check (list sexp_testable)) "same quote sexp" [Pair ((Symbol "quasiquote"), Pair ((Bool true), Nil))] (Reader.read_sexprs "  `    #t  ");;
let simple_unquote_test_with_spaces () = Alcotest.(check (list sexp_testable)) "same quote sexp" [Pair ((Symbol "unquote"), Pair ((Bool true), Nil))] (Reader.read_sexprs "   ,   #t   ");;
let simple_unquotesplice_test_with_spaces () = Alcotest.(check (list sexp_testable)) "same quote sexp" [Pair ((Symbol "unquote-splicing"), Pair ((Bool true), Nil))] (Reader.read_sexprs "  ,@   #t");;

let nested_quotes_1 () = Alcotest.(check (list sexp_testable)) "same quote sexp" [
    Pair (Symbol "quasiquote",
        Pair
        (Pair (Bool true,
            Pair
            (Pair (Symbol "unquote",
                Pair
                (Pair (Symbol "+",
                Pair (Number (Fraction (1, 1)),
                    Pair (Number (Fraction (2, 1)),
                    Pair (Number (Fraction (3, 1)), Nil)))),
                Nil)),
            Pair (Bool false, Nil))),
        Nil))
] (Reader.read_sexprs "`(#t ,(+ 1 2 3) #f)")

let nested_quotes_2 () = Alcotest.(check (list sexp_testable)) "same quote sexp" [
    Pair (Symbol "quasiquote",
        Pair
        (Pair (Symbol "a",
            Pair
            (Pair (Symbol "unquote-splicing",
                Pair
                (Pair (Symbol "append",
                Pair
                    (Pair (Symbol "quote",
                    Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)), Nil)),
                    Pair
                    (Pair (Symbol "quote",
                    Pair (Pair (Symbol "z", Pair (Symbol "w", Nil)), Nil)),
                    Nil))),
                Nil)),
            Pair (Symbol "b", Nil))),
        Nil))
] (Reader.read_sexprs "`(a ,@(append '(x y) '(z w)) b)")

let quote_parser_tester_suite = [
    ("simple_quote_test", simple_quote_test);
    ("simple_quasiquote_test", simple_quasiquote_test);
    ("simple_unquote_test", simple_unquote_test);
    ("simple_unquotesplice_test", simple_unquotesplice_test);
    ("simple_quote_test_with_spaces", simple_quote_test_with_spaces);
    ("simple_quasiquote_test_with_spaces", simple_quasiquote_test_with_spaces);
    ("simple_unquote_test_with_spaces", simple_unquote_test_with_spaces);
    ("simple_unquotesplice_test_with_spaces", simple_unquotesplice_test_with_spaces);
    ("nested_quotes_1", nested_quotes_1);
    ("nested_quotes_2", nested_quotes_2)
]
(* let () =
  let open Alcotest in
  run "Test Quote" [
      "test_quote", [test_case "simple_quote_test" `Quick simple_quote_test];
      "test_quasiqoute", [test_case "simple_quasiquote_test" `Quick simple_quasiquote_test]
      (* add more tests here *)
    ] *)