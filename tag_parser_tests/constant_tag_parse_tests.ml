#use "topfind";;
#use "tag_parser_utils.ml";;
#require "alcotest";;

let test_bool () = 
  let expected = [(Const(Sexpr (Bool (true))))] in
  let parsed = Tag_Parser.tag_parse_expressions [Bool true] in
    Alcotest.(check (list expr_testable)) "same expr?" expected parsed;; 

let test_number () = 
  let expected = [(Const(Sexpr (Number (Float(1.23)))))] in
  let parsed = Tag_Parser.tag_parse_expressions [(Number (Float(1.23)))] in
    Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;

let test_char () = 
  let expected = [(Const(Sexpr (Char ('a'))))] in
  let parsed = Tag_Parser.tag_parse_expressions [Char ('a')] in
    Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;

let test_string () = 
  let expected = [(Const(Sexpr (String ("hello"))))] in
  let parsed = Tag_Parser.tag_parse_expressions [String ("hello")] in
    Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;

let test_simple_quote () = 
  let expected = [Const (Sexpr(Symbol "a"))] in
  let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "quote", Pair (Symbol "a", Nil))] in
    Alcotest.(check (list expr_testable)) "same expr?" expected parsed

let test_complex_quote () =
  let expected = [Const (Sexpr 
  (Pair (Symbol "a",
    Pair (Number (Fraction (2, 1)),
      Pair (Pair (String "well", Pair (Nil, Nil)), Nil)))))] in
  let parsed = Tag_Parser.tag_parse_expressions [
    Pair (Symbol "quote", Pair (Pair (Symbol "a",
      Pair (Number (Fraction (2, 1)),
        Pair (Pair (String "well", Pair (Nil, Nil)), Nil))) , Nil))] in
    Alcotest.(check (list expr_testable)) "same expr?" expected parsed

(* Name the test cases and group them together *)
let tag_constant_parser_tester_suite =
 [("test bool", test_bool);
  ("test number", test_number);
  ("test char", test_char);
  ("test string", test_string);
  ("test simple quote", test_simple_quote);
  ("test comlplex quote", test_complex_quote)
  ]
;;

let () =
  let open Alcotest in
  run "Test Constants tag parser" [
      "test constants", (List.map (fun (desc, test) -> test_case desc `Quick test) tag_constant_parser_tester_suite)
      (* add more tests here *)
    ]

