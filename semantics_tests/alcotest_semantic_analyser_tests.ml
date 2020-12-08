#use "topfind";;

#use "semantic_analyser_tests.ml";;
#require "alcotest";;


let rec string_of_sexpr se = 
    let string_of_number n = match n with
    | Fraction((a, b)) -> "Fraction(" ^ (string_of_int a) ^ "/" ^ (string_of_int b) ^ ")"
    | Float(f) -> "Float(" ^ (string_of_float f) ^ ")" in  
    match se with
    | Bool(b) -> "Bool(" ^ (string_of_bool b) ^ ")"
    | Nil -> "Nil"
    | Number(n) -> string_of_number n
    | Char(c) ->  "Char(" ^ (String.make 1 c) ^")"
    | String(s) -> "String(" ^ s ^ ")"
    | Symbol(s) -> "Symbol(" ^ s ^ ")"
    | Pair((se1, se2)) -> "Pair(" ^ (string_of_sexpr se1) ^ ", " ^ (string_of_sexpr se2) ^ ")"

let concrete_syntax_string_of_sexpr sexpr = 
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

let rec string_of_expr' e =
  let string_of_var var = match var with
    | VarFree var -> "VarFree (" ^ var ^ ")"
    | VarParam (var, mj_idx) -> "VarParam (" ^ var ^ ", " ^ (string_of_int mj_idx) ^ ")"
    | VarBound (var, mj_idx, mn_idx) -> "VarBound (" ^ var ^ ", " ^ (string_of_int mj_idx) ^ ", " ^ (string_of_int mn_idx) ^ ")" in
  let string_of_constant c = match c with Void -> "Void" | Sexpr s -> string_of_sexpr s 
  in match e with
  | Const' c -> "Const' (" ^ (string_of_constant c) ^ ")"
  | Var' v -> "Var' (" ^ (string_of_var v) ^ ")"
  | Box' v -> "Box' (" ^ (string_of_var v) ^ ")"
  | BoxGet' v -> "BoxGet' (" ^ (string_of_var v) ^ ")"
  | BoxSet' (v, e) -> "BoxSet' (" ^ (string_of_var v) ^ ", " ^ (string_of_expr' e) ^ ")"
  | If' (test, _then, _else) -> "If' (" ^ (string_of_expr' test) ^ ", " ^ (string_of_expr' _then) ^ ", " ^ (string_of_expr' _else) ^ ")"
  | Seq' [] -> "Seq' []"
  | Seq' (e :: es) -> List.fold_left (fun str curr -> str ^ "; " ^ (string_of_expr' curr)) ("Seq' [" ^ (string_of_expr' e)) es
  | Set' (v, e) -> "Set' (" ^ (string_of_var v) ^ ", " ^ (string_of_expr' e) ^ ")"
  | Def' (v, e) -> "Def' (" ^ (string_of_var v) ^ ", " ^ (string_of_expr' e) ^ ")"
  | Or' [] -> "Or []"
  | Or' (e :: es) -> List.fold_left (fun str curr -> str ^ "; " ^ (string_of_expr' curr)) ("Or' [" ^ (string_of_expr' e)) es
  | LambdaSimple' (params, body) -> "LambdaSimple' ([" ^ (List.fold_left (fun str curr -> str ^ curr ^ "; ") "" params) ^ "], " ^ (string_of_expr' body) ^ ")"
  | LambdaOpt' (params, opt_param, body) -> "LambdaOpt' ([" ^ (List.fold_left (fun str curr -> str ^ curr ^ "; ") "" params) ^ "], " ^ opt_param ^ ", " ^ (string_of_expr' body) ^ ")"
  | Applic' (rator, rands) -> "Applic' (" ^ (string_of_expr' rator) ^ ", " ^ (match rands with [] -> "[]" | e :: es -> (List.fold_left (fun str curr -> str ^ "; " ^ (string_of_expr' curr)) ("Seq' [" ^ (string_of_expr' e)) es)) ^ ")" 
  | ApplicTP' (rator, rands) -> "ApplicTP' (" ^ (string_of_expr' rator) ^ ", " ^ (match rands with [] -> "[]" | e :: es -> (List.fold_left (fun str curr -> str ^ "; " ^ (string_of_expr' curr)) ("Seq' [" ^ (string_of_expr' e)) es)) ^ ")";;

let sexp_testable = Alcotest.testable (fun ppf (se: sexpr) -> (Format.fprintf ppf "%s" (string_of_sexpr se))) sexpr_eq;;

let expr_testable = Alcotest.testable (fun ppf expr -> (Format.fprintf ppf "%s" (string_of_expr expr))) expr_eq;;

let expr'_testable = Alcotest.testable (fun ppf expr -> (Format.fprintf ppf "%s" (string_of_expr' expr))) expr'_eq;;

let map_test_suite_to_alcotest_test_suite test_suite = List.map (fun (name, input, expected) -> 
  (name, fun () -> Alcotest.(check (expr'_testable)) "same semantic expression?" expected (Semantics.annotate_tail_calls (Semantics.annotate_lexical_addresses input)))) test_suite;;

let () =
  let open Alcotest in
  run "Semantics" [
      "test semantic_analyzer", (List.map (fun (desc, test) -> test_case desc `Quick test) (map_test_suite_to_alcotest_test_suite semantic_analyzer_tests_suite));
    ]