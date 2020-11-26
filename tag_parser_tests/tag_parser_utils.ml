#use "topfind.ml";;

#directory "..";;
#use "tag-parser.ml";;
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
    | Symbol(s) -> s
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
    | Seq exprs -> (List.fold_left (fun seq expr -> seq ^ " " ^ (string_of_expr expr)) "(begin " exprs) ^ ")"
    | Set (var, _val) -> "(set! " ^ (string_of_expr var) ^ (string_of_expr _val) ^ ")"
    | Def (var, _val) -> "(define " ^ (string_of_expr var) ^ (string_of_expr _val) ^ ")"
    | Or exprs -> (List.fold_left (fun seq expr -> seq ^ " " ^ (string_of_expr expr)) "(or " exprs) ^ ")"
    | LambdaSimple (args, expr) -> "(lambda " ^ (List.fold_left (fun arglist arg -> arglist ^ " " ^ arg) "(" args) ^ ") " ^ (string_of_expr expr) ^ ")"
    | LambdaOpt (args, arg, expr) -> "(lambda " ^ (if args = [] then arg else (List.fold_left (fun arglist arg -> arglist ^ " " ^ arg) "(" args) ^ ". " ^ arg ^ ") ") ^ (string_of_expr expr) ^ ")"
    | Applic (expr, exprs) -> (List.fold_left (fun seq expr -> seq ^ " " ^ (string_of_expr expr)) "(" (expr :: exprs)) ^ ") "

let sexp_testable = Alcotest.testable (fun ppf (se: sexpr) -> (Format.fprintf ppf "%s" (string_of_sexpr se))) sexpr_eq;;

let expr_testable = Alcotest.testable (fun ppf expr -> (Format.fprintf ppf "%s" (string_of_expr expr))) expr_eq;;