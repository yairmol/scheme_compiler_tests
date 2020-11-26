#use "topfind";;

#use "macro_expansions.ml";;
#require "alcotest";;

let base_case () = 
  let and_exp = Reader.read_sexprs "(and)" in
  let expected = Reader.read_sexprs "#t" in
    Alcotest.(check (list sexp_testable)) "same sexp?" expected (List.map and_expander and_exp);;

let base_case_2 () = 
  let and_exp = Reader.read_sexprs "(and 1)" in
  let expected = Reader.read_sexprs "1" in
    Alcotest.(check (list sexp_testable)) "same sexp?" expected (List.map and_expander and_exp);;

let test_simple_and () = 
  let and_exp = Reader.read_sexprs "(and a b c)" in
  let expected = Reader.read_sexprs "(if a (if b c #f) #f)" in
    Alcotest.(check (list sexp_testable)) "same sexp?" expected (List.map and_expander and_exp);;

let test_complex_and () = 
  let and_exp = Reader.read_sexprs "(and () (2 3) (#t #\\c \"hello\" . .sym?) #f)" in
  let expected = Reader.read_sexprs "(if () (if (2 3) (if (#t #\\c \"hello\" . .sym?) #f #f) #f) #f)" in
    Alcotest.(check (list sexp_testable)) "same sexp?" expected (List.map and_expander and_exp);;


let and_expander_test_suite = [
    ("and base case 1", base_case);
    ("and base case 2", base_case_2);
    ("test simple and expansion", test_simple_and);
    ("test complex and expansion", test_complex_and)
];;

let () =
  let open Alcotest in
  run "Test Macro Expansions" [
      "test and expander", (List.map (fun (desc, test) -> test_case desc `Quick test) and_expander_test_suite)
      (* add more tests here *)
    ]