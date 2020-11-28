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

let test_single_expr_sequence_2 () =
  let parsed = Tag_Parser.tag_parse_expressions [
    Pair (Symbol "begin", Pair (Pair (Symbol "if", Pair (Symbol "?", Pair (Symbol "!", Nil))), Nil))] in
  let expected = [If (Var "?", Var "!", Const Void)] in
  Alcotest.(check (list expr_testable)) "same expr?" expected parsed;; 


let test_regular_sequence () = 
  let expr1 = Number (Float 1.234) in
  let expr2 = Char ('c') in
  let expr3 = String ("on the fly") in
  let parsed = Tag_Parser.tag_parse_expressions [
    Pair (Symbol "begin",
      Pair (expr1,
      Pair (expr2,
      Pair (expr3, Nil))))] in
  let expected = [Seq ([Const (Sexpr expr1); Const (Sexpr expr2); Const (Sexpr expr3)])] in
  Alcotest.(check (list expr_testable)) "same expr?" expected parsed;; 

let test_nested_sequence_depth_one () = 
  let parsed =
    let seq_inner =
      let expr1 = Pair (Symbol "quote", Pair (Symbol "inner_seq", Nil)) in
      let expr2 = Bool (true) in 
    Pair (Symbol "begin", Pair (expr1, Pair (expr2, Nil))) in
    let expr1 = Pair (Symbol "set!", Pair (Symbol "var", Pair (Bool (true), Nil))) in
    let expr2 = seq_inner in
    let expr3 = Pair (Symbol "display", Pair (Symbol "var", Nil)) in
    let expr4 = Symbol "var" in
  Tag_Parser.tag_parse_expressions [
    Pair (Symbol "begin", 
      Pair (expr1,
      Pair (expr2,
      Pair (expr3,
      Pair (expr4, Nil)))))] in
  let expected = [Seq [
    Set (Var "var", Const (Sexpr (Bool true)));
    Const (Sexpr (Symbol "inner_seq"));
    Const (Sexpr (Bool true));
    Applic (Var "display", [Var "var"]);
    Var ("var")]] in
  Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;

let test_deep_nested_sequence () =
  let parsed =
    let seq1 = Pair (Symbol "begin", Pair (Number (Float (512.23)), Pair (Char ('v'), Nil))) in
    let seq2 = Pair (Symbol "begin", Pair (String "just a string", Pair (Bool (true), Nil))) in
    let seq3 = Pair (Symbol "begin", Pair (Pair (Symbol "set!", Pair (Symbol "var", Pair (String "well", Nil))), Pair (Pair (Symbol "display", Pair (Symbol "var", Nil)), Nil))) in
    let seq4 = Pair (Pair (Symbol "quote", Pair (Symbol "last_sequence!", Nil)))

let sequence_tag_parser_test_suite = [
  ("test empty sequence", test_empty_sequence);
  ("test single expression sequence 1", test_single_expr_sequence);
  ("test single expression sequence 2", test_single_expr_sequence_2);
  ("test sequence with expressions", test_regular_sequence);
  ("test nested sequnce with depth 1", test_nested_sequence_depth_one);
]


let () =
  let open Alcotest in
  run "Test If expressions tag parser" [
      "test if expressions", (List.map (fun (desc, test) -> test_case desc `Quick test) sequence_tag_parser_test_suite)
      (* add more tests here *)
    ]