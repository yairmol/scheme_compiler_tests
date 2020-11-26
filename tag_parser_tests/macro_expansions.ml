(* your macro expanders here *)
(* each expander should receive a sexpr and return the expanded sexpr *)
#use "tag_parser_utils.ml";;

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "pset!"; "unquote";
   "unquote-splicing"];;

(* helper functions *)
let is_reserved_word e = List.exists (fun e2 -> e = e2) reserved_word_list;;

let rec sexpr_list_to_list sexpr_list = match sexpr_list with
  | Nil -> [];
  | Pair (car, cdr) -> car :: (sexpr_list_to_list cdr);
  | _ -> raise X_syntax_error;;

let rec simple_params params = match params with
  | Nil -> true;
  | Pair (car, Nil) -> true
  | Pair (car, cdr) -> simple_params cdr
  | _ -> false ;;

let rec opt_params_to_list params = match params with
  | Pair (car, Nil) -> raise X_syntax_error
  | Pair (car, cdr) -> car :: (opt_params_to_list cdr) 
  | cdr -> [cdr] ;;

let rec last_in_list list = match list with
  | [] -> raise X_syntax_error
  | x :: [] -> x
  | x :: tail -> last_in_list tail;;

let rec remove_last list = match list with
  | [] -> raise X_syntax_error
  | x :: [] -> []
  | x :: tail -> x :: remove_last tail;;

let rec extract_bindings bindings = match bindings with
  | Nil -> (Nil, Nil)
  | Pair (Pair (param, Pair (arg, Nil)), rest) -> 
      let (params, args) = extract_bindings rest in
      (Pair (param, params), Pair(arg, args))
  | _ -> raise X_syntax_error;;

let let_expander sexpr =
  let app_of_let bindings body =
    let (params, args) = extract_bindings bindings in 
    Pair(Pair(Symbol "lambda", Pair(params, body)), args) in
  match sexpr with Pair (Symbol "let", Pair (bindings, body)) -> app_of_let bindings body | _ -> Nil;;

let letstar_expander sexpr =  
  let rec nested_let_of_letstar bindings body = match bindings with
    | Nil -> (Pair (Symbol "let", Pair(Nil, body)))
    | Pair (binding1, Nil) -> (Pair (Symbol "let", Pair(Pair(binding1, Nil), body)))
    | Pair(binding1, rest) -> (Pair (Symbol "let", Pair(Pair(binding1, Nil), Pair(nested_let_of_letstar rest body, Nil))))
    | _ -> raise X_syntax_error in 
  match sexpr with Pair (Symbol "let*", Pair (bindings, body)) -> nested_let_of_letstar bindings body | _ -> Nil;;

let letrec_expander sexpr =  
  let letrec_to_let bindings body = 
    let (params, args) = extract_bindings bindings in
    let whatever = Pair(Symbol "quote", Pair(Symbol "whatever", Nil)) in
    let dummy_bindings = 
      let rec create_dummy_bindings params = match params with
        | Nil -> Nil
        | Pair (param, rest) -> Pair (Pair (param, Pair (whatever, Nil)), create_dummy_bindings rest)
        | _ -> raise X_syntax_error in
      create_dummy_bindings params in
    let set_bangs = 
      let create_set_bang param arg = Pair (Symbol "set!", Pair (param, Pair (arg, Nil))) in
      let rec create_set_bangs params args = match (params, args) with
        | (Nil, Nil) -> body
        | (Pair (param, rest_p), Pair (arg, rest_a)) -> Pair (create_set_bang param arg, create_set_bangs rest_p rest_a)
        | _ -> raise X_syntax_error in
      create_set_bangs params args in
    Pair (Symbol "let", Pair (dummy_bindings, set_bangs)) in
  match sexpr with Pair (Symbol "letrec", Pair (bindings, body)) -> letrec_to_let bindings body | _ -> Nil;;

let and_expander sexpr = 
  let rec and_to_if exprs = match exprs with
    | Nil -> Bool (true)
    | Pair (first, Nil) -> first
    | Pair (first, rest) -> Pair (Symbol "if", Pair (first, Pair (and_to_if rest, Pair(Bool (false), Nil))))
    | _ -> raise X_syntax_error in
  match sexpr with Pair (Symbol "and", exprs) -> and_to_if exprs | _ -> Nil;;

let cond_expander sexpr =
  let rec cond_macro cond_ribs = 
    let make_let bindings body = Pair (Symbol "let", Pair (bindings, Pair(body, Nil))) in
    let make_let_binding var _val = Pair (var, Pair (_val, Nil)) in
    let make_let_bindings vars vals = List.fold_right2 (fun vr vl bndgs -> Pair (make_let_binding vr vl, bndgs)) vars vals Nil in
    let make_lambda args body = Pair (Symbol "lambda", Pair (args, Pair (body, Nil))) in
    let make_if test _then _else = Pair (Symbol "if", Pair (test, Pair (_then, Pair (_else, Nil)))) in
    match cond_ribs with
    | Pair (first_rib, rest) -> (match first_rib with 
      | Pair (expr, Pair (Symbol "=>", Pair(expr_f, Nil))) -> 
          make_let (make_let_bindings [Symbol "value"; Symbol "f"; Symbol "rest"] 
                                      [expr; make_lambda Nil expr_f; make_lambda Nil (cond_macro rest)])
                  (make_if (Symbol "value") (Pair (Pair (Symbol "f", Nil), Symbol "value")) (Pair (Symbol "rest", Nil)))
        | Pair (Symbol "else", exprs) -> exprs
        | Pair (test, sequence) -> Pair (Pair (Symbol "if", Pair (test, sequence)), cond_macro rest)
        | _ -> raise X_syntax_error)
    | Nil -> Pair (Symbol "begin", Nil)
    | _ -> raise X_syntax_error in 
  match sexpr with Pair (Symbol "cond", cond_ribs, Nil) -> cond_macro cond_ribs | _ -> Nil;;


let pset_expander sexpr = 
  let pset_to_let assignments =
    let (vars, vals) = extract_bindings assignments in
    let set_bangs = 
      let create_set_bang var _val = Pair (Symbol "set!", Pair (var, Pair (_val, Nil))) in
      let rec create_set_bangs vars vals = match (vars, vals) with
        | (Nil, Nil) -> Nil
        | (Pair(var, rest_vars), Pair(_val, rest_vals)) -> Pair (create_set_bang var _val, create_set_bangs rest_vars rest_vals)
        | _ -> raise X_syntax_error in
      create_set_bangs vars vals in
    (* ((lambda (v_1 ... v_n) (begin)) (set! v_1 e_1) ... (set! v_n e_n)) *)
    Pair (Pair (Symbol "lambda", Pair (vars, Pair (Pair (Symbol "begin", Nil), Nil))), set_bangs) in
  match sexpr with Pair (Symbol "pset!", assignments) -> pset_to_let assignments | _ -> Nil;;

let quasiquote_expander sexpr =
  let rec quasiquote_expander sexpr = 
    let uqsp_expander symbol sexpr1 sexpr2 = Pair (symbol, Pair (sexpr1, Pair (sexpr2, Nil))) in
    match sexpr with
      | Pair (Symbol "unquote", Pair (sexpr, Nil)) -> sexpr
      | Pair (Symbol "unquote-splicing", Pair (sexpr, Nil)) -> raise X_syntax_error
      | Nil -> Pair (Symbol "quote", Pair (Nil, Nil))
      | Symbol s -> Pair (Symbol "quote", Pair (Symbol s, Nil))
      | Pair (sexpr, rest) -> (match (sexpr, rest) with
        | (Pair (Symbol "unqoute-splicing", Pair(sexpr, Nil)), rest) -> uqsp_expander (Symbol "append") sexpr (quasiquote_expander rest)
        | (sexpr1, Pair (Symbol "unquote-splicing", Pair (sexpr2, Nil))) -> uqsp_expander (Symbol "cons") (quasiquote_expander sexpr1) sexpr2
        | (sexpr, rest) -> uqsp_expander (Symbol "cons") (quasiquote_expander sexpr) (quasiquote_expander rest))
      | _ -> raise X_syntax_error in 
  match sexpr with Pair (Symbol "quasiquote", Pair (sexpr, Nil)) -> quasiquote_expander sexpr | _ -> Nil;;


(* put the names of the macro expanders in a tuple here *)

let (and_expander, cond_expander, quasiquote_expander, let_expander,
    letstar_expander, letrec_expander) = (and_expander, cond_expander, quasiquote_expander, let_expander,
    letstar_expander, letrec_expander);; (* write function names in tuple according to the given order *)