#use "topfind";;
#use "tag_parser_utils.ml";;
#require "alcotest";;

(* tests for Applic *)
let applic_test_1 = 
    let parsed = Tag_Parser.tag_parse_expressions [Bool true] in
    let expected = [Applic (LambdaSimple (["x"; "y"], Var "y"),[Const (Sexpr (Number (Fraction (1, 1)))); Const (Sexpr (Number (Fraction (2, 1))))])] in
        Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;

let applic_test_suite = [
    ("applic_test_1", applic_test_1);
]

let () = 
    let open Alcotest in
    run "Test Applic tag parser" ["test Applic expressions", (List.map (fun (desc, test) -> test_case desc `Quick test) applic_test_suite)]