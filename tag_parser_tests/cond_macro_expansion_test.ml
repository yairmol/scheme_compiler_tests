#use "topfind";;

#use "macro_expansions.ml";;
#require "alcotest";;

let simple_cond_1 () = 
  let cond_exp = Reader.read_sexprs "(cond ((eq? 1 1) 1))" in
  let expected = Reader.read_sexprs "(if (eq? 1 1) (begin 1))" in
    Alcotest.(check (list sexp_testable)) "same sexp?" expected (List.map cond_expander and_exp);;

let simple_cond_with_else () = 
  let and_exp = Reader.read_sexprs "(cond ((well? ahh) you-know) (else 1 2 3))" in
  let expected = Reader.read_sexprs "(if (well? ahh) (begin you-know) (begin 1 2 3))" in
    Alcotest.(check (list sexp_testable)) "same sexp?" expected (List.map cond_expander and_exp);;

let simple_cond_with_callback () = 
  let and_exp = Reader.read_sexprs "(cond (20 => f))" in
  let expected = Reader.read_sexprs "(let ((value 20) (f (lambda () f)) (rest ((lambda () (begin))))) (if value ((f) value) (rest)))" in
    Alcotest.(check (list sexp_testable)) "same sexp?" expected (List.map cond_expander and_exp);;

let cond_with_several_ribs () = 
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