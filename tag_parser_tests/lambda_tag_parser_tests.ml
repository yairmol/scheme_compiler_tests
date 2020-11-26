#use "topfind";;
#use "tag_parser_utils.ml";;
#require "alcotest";;

let testSimpleLambda () = 
    let parsed = Tag_Parser.tag_parse_expressions [
      Pair (Symbol "lambda", Pair (Pair (Symbol "arg1", Pair (Symbol "arg2", Pair (Symbol "arg3", Nil))),
            Pair (Pair (Symbol "expr1", Pair (Symbol "expr2", Nil)), Nil)))
    ] in
    let expected = [
      LambdaSimple ([Const (Sexpr (Symbol "arg1"); Const (Sexpr (Symbol "arg2"); Const (Sexpr (Symbol "arg3")],
      [])
    ]in
    Alcotest.(check (list expr_testable)) "same expr?" expected parsed;;r