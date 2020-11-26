#use "topfind";;
#use "tag_parser_utils.ml";;
#require "alcotest";;


let test_empty_sequence () = 
  let parsed = Tag_Parser.tag_parse_expressions [Pair (Symbol "begin", Nil)] in
  let expected = [Const Void] in
    Alcotest.(check (list expr_testable)) "same expr?" expected parsed;; 

let test_single_expr_sequence () = 
  let parsed = Tag_Parser.tag_parse_expressions [
    Pair (Symbol "begin", Pair (Number (Float 1.2), Nil))] in
  let expected = [Const (Sexpr (Number (Float 1.2)))] in
    Alcotest.(check (list expr_testable)) "same expr?" expected parsed;; 

let test_simple_explicit_sequence () =
  let parsed = Tag_Parser.tag_parse_expressions [
    Pair ("begin")
  ]


let sequence_tag_parser_test_suite = [
  ("test empty sequence", test_empty_sequence);
  ("test single expression sequence 1", test_single_expr_sequence);
  ("test single expression sequence 2", test_single_expr_sequence);
]


let () =
  let open Alcotest in
  run "Test If expressions tag parser" [
      "test if expressions", (List.map (fun (desc, test) -> test_case desc `Quick test) sequence_tag_parser_test_suite)
      (* add more tests here *)
    ]