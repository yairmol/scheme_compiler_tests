#use "topfind";;

#use "tag_parser_tests.ml";;
#require "alcotest";;

let string_of_sexpr sexpr = 
  let string_of_number n = match n with
    | Fraction((a, b)) ->  (string_of_int a) ^ "/" ^ (string_of_int b)
    | Float(f) -> (string_of_float f) in  
  let rec string_of_sexpr1 sexpr inside = match sexpr with
    | Bool(b) -> if b then "#t" else "#f"
    | Nil -> "()"
    | Number(n) -> string_of_number n
    | Char(c) ->  "#\\" ^ (String.make 1 c)
    | String(s) -> "\"" ^ s ^ "\""
    | Symbol(s) -> (match s with
      | "quote" -> "'"
      | "quasiquote" -> "`"
      | "unquote" -> ","
      | "unquote-splicing" -> ",@"
      | s -> s)
    | Pair(se1, Nil) -> (if inside then "" else "(") ^ (string_of_sexpr1 se1 false) ^ ")"
    | Pair(se1, Pair(se2, se3)) -> (if inside then "" else "(") ^ (string_of_sexpr1 se1 false) ^ " " ^ (string_of_sexpr1 (Pair (se2, se3)) true)
    | Pair(se1, se2) -> (if inside then "" else "(") ^ (string_of_sexpr1 se1 false) ^ " . " ^ (string_of_sexpr1 se2 false) ^ ")" in
  string_of_sexpr1 sexpr false;;

let rec string_of_expr expr = match expr with
    | Const Void -> "(void)"
    | Const (Sexpr s) -> (match s with
      | Number n -> (string_of_sexpr s)
      | String str -> (string_of_sexpr s)
      | Char c -> (string_of_sexpr s)
      | Bool b -> (string_of_sexpr s)
      | s -> "'" ^ (string_of_sexpr s))
    | Var v -> v
    | If (test, _then, _else) -> "(if " ^ (string_of_expr test) ^ " " ^ (string_of_expr _then) ^ " " ^ (string_of_expr _else) ^ ")"
    | Seq exprs -> (List.fold_left (fun seq expr -> seq ^ " " ^ (string_of_expr expr)) "(begin" exprs) ^ ")"
    | Set (var, _val) -> "(set! " ^ (string_of_expr var) ^ " " ^ (string_of_expr _val) ^ ")"
    | Def (var, _val) -> "(define " ^ (string_of_expr var) ^ " " ^ (string_of_expr _val) ^ ")"
    | Or exprs -> (List.fold_left (fun seq expr -> seq ^ " " ^ (string_of_expr expr)) "(or" exprs) ^ ")"
    | LambdaSimple (args, expr) -> "(lambda " ^ (match args with [] -> "() " | a1 :: args_rest -> (List.fold_left (fun arglist arg -> arglist ^ " " ^ arg) ("(" ^ a1) args_rest) ^ ") ") ^ (string_of_expr expr) ^ ")"
    | LambdaOpt (args, arg, expr) -> "(lambda " ^ (match args with [] -> (arg ^ " ") | a1 :: args_rest -> (List.fold_left (fun arglist arg -> arglist ^ " " ^ arg) ("(" ^ a1) args_rest) ^ " . " ^ arg ^ ") ") ^ (string_of_expr expr) ^ ")"
    | Applic (expr, exprs) -> (List.fold_left (fun seq expr -> seq ^ (if seq = "(" then "" else " ") ^ (string_of_expr expr)) "(" (expr :: exprs)) ^ ")"

let sexp_testable = Alcotest.testable (fun ppf (se: sexpr) -> (Format.fprintf ppf "%s" (string_of_sexpr se))) sexpr_eq;;

let expr_testable = Alcotest.testable (fun ppf expr -> (Format.fprintf ppf "%s" (string_of_expr expr))) expr_eq;;

let map_test_suite_to_alcotest_test_suite test_suite = List.map (fun (name, input, expected) -> 
  (name, fun () -> Alcotest.(check (list expr_testable)) "same expression?" input expected)) test_suite;;

let () =
  let open Alcotest in
  run "Tag_Parser" [
      "test const", (List.map (fun (desc, test) -> test_case desc `Quick test) (map_test_suite_to_alcotest_test_suite const_test_suite));
      "Test define", (List.map (fun (desc, test) -> test_case desc `Quick test) (map_test_suite_to_alcotest_test_suite define_test_suite));
      "Test lambda",  (List.map (fun (desc, test) -> test_case desc `Quick test) (map_test_suite_to_alcotest_test_suite lambda_test_suite));
      "Test let and let*",  (List.map (fun (desc, test) -> test_case desc `Quick test) (map_test_suite_to_alcotest_test_suite let_test_suite));
      "Test if, and, cond",  (List.map (fun (desc, test) -> test_case desc `Quick test) (map_test_suite_to_alcotest_test_suite if_test_suite));
      "Test quasiquote",  (List.map (fun (desc, test) -> test_case desc `Quick test) (map_test_suite_to_alcotest_test_suite quasiquote_test_suite));
      (* "Test sequence",  (List.map (fun (desc, test) -> test_case desc `Quick test) (map_test_suite_to_alcotest_test_suite sequence_test_suite)); *)
    ]