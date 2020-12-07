#use "topfind";;
#directory "../";;
#directory "../..";;
#use "tag-parser.ml";;

let quasiquote_test_suite = [
  ("quasiquote expansion test 1", Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair (Pair (Pair (Symbol "unquote", Pair (Symbol "x", Nil)), Nil), Nil)) ], [Applic (Var "cons", [Var "x"; Const (Sexpr Nil)])]);
  ("quasiquote expansion test 2", Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Nil)) ], [Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])]);
  ("quasiquote expansion test 3", Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),Pair (Symbol "b", Nil)),Nil))], [Applic (Var "cons",[Var "a";Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])]);
  ("quasiquote expansion test 4", Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair (Symbol "a",Pair (Pair (Symbol "unquote", Pair (Symbol "b", Nil)), Nil)),Nil))], [Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "cons", [Var "b"; Const (Sexpr Nil)])])]);
  ("quasiquote expansion test 5", Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Symbol "b", Nil)),Nil))], [Applic (Var "append",[Var "a";Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])]);
  ("quasiquote expansion test 6", Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair (Symbol "a",Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))], [Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])]);
  ("quasiquote expansion test 7", Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))], [Applic (Var "cons",[Var "a"; Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])]);
  ("quasiquote expansion test 8", Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))], [Applic (Var "append",[Var "a"; Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])]);
  ("quasiquote expansion test 9", Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Symbol "unquote", Pair (Symbol "b", Nil))),Nil))], [Applic (Var "append", [Var "a"; Var "b"])]);
  ("quasiquote expansion test 10", Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil))),Nil))], [Applic (Var "cons", [Var "a"; Var "b"])]);
  ("quasiquote expansion test 11", Tag_Parser.tag_parse_expressions [Pair (Symbol "quasiquote", Pair(Pair(Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)), Nil),Nil),Nil),Nil))], [Applic (Var "cons",[Applic (Var "cons",[Applic (Var "append", [Var "a"; Const (Sexpr Nil)]); Const (Sexpr Nil)]);Const (Sexpr Nil)])]);
];;