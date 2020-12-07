#use "topfind";;
#directory "../";;
#directory "../..";;
#use "tag-parser.ml";;

(* tests for Define *)
let define_test_suite = [
  ("simple_define_test", Tag_Parser.tag_parse_expressions [Pair (Symbol "define", Pair (Symbol "var", Pair (Bool true, Nil)))], [Def (Var "var", Const (Sexpr (Bool true)))]);
  ("complex_define_test", Tag_Parser.tag_parse_expressions [
    Pair (Symbol "define", 
      Pair (Symbol "var",
      Pair (
        Pair (Symbol "if", 
          Pair (Pair (Symbol "or", Pair (Bool false, Pair (Bool true, Pair (Number (Fraction (1, 1)), Nil)))),
          Pair (Char 'c', 
          Pair (String "well", Nil)))),
    Nil)))
  ], [
    Def (Var "var", 
      If (
        Or [
          Const (Sexpr (Bool false));
          Const (Sexpr (Bool true));
          Const (Sexpr (Number (Fraction (1, 1))))
        ], 
        Const (Sexpr (Char 'c')), Const (Sexpr (String "well"))))]);
  ("mit define expansion 1", Tag_Parser.tag_parse_expressions [Pair (Symbol "define",Pair (Pair (Symbol "square", Pair (Symbol "x", Nil)),Pair (Pair (Symbol "*", Pair (Symbol "x", Pair (Symbol "x", Nil))), Nil)))], [Def (Var "square",LambdaSimple (["x"], Applic (Var "*", [Var "x"; Var "x"])))]);
  ("mit define expansion 2", Tag_Parser.tag_parse_expressions [Pair (Symbol "define",Pair (Pair (Symbol "cmp", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "if",Pair (Pair (Symbol ">", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (-1, 1)), Nil)))),Nil)))], [Def (Var "cmp",LambdaSimple (["a"; "b"],If (Applic (Var ">", [Var "a"; Var "b"]), Const (Sexpr (Number (Fraction (1, 1)))),Const (Sexpr (Number (Fraction (-1, 1)))))))]);
  ("mit define expansion 3", Tag_Parser.tag_parse_expressions [Pair (Symbol "define",Pair(Pair (Symbol "square",Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil)))),Pair(Pair (Symbol "*",Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil)))),Nil)))], [Def (Var "square",LambdaSimple (["a"; "b"; "c"],Applic (Var "*", [Var "a"; Var "b"; Var "c"])))]);
  ("mit define expansion 4", Tag_Parser.tag_parse_expressions [Pair (Symbol "define",Pair (Pair (Symbol "returnonly", Pair (Symbol "x", Nil)),Pair(Pair (Symbol "begin",Pair (String "return only", Pair (Symbol "x", Nil))),Nil)))], [Def (Var "returnonly",LambdaSimple (["x"], Seq [Const (Sexpr (String "return only")); Var "x"]))]);
  ("mit define expansion 5", Tag_Parser.tag_parse_expressions [Pair (Symbol "define",Pair(Pair (Symbol "applic",Pair (Symbol "fun",Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "e", Nil))))))),Pair(Pair (Symbol "fun",Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "e", Nil)))))),Nil)))], [Def (Var "applic",LambdaSimple (["fun"; "a"; "b"; "c"; "d"; "e"],Applic (Var "fun", [Var "a"; Var "b"; Var "c"; Var "d"; Var "e"])))]);
  ("mit define expansion 6", Tag_Parser.tag_parse_expressions [Pair (Symbol "define",Pair(Pair (Symbol "if_fun",Pair (Symbol "if_test",Pair (Symbol "if_then", Pair (Symbol "if_else", Nil)))),Pair(Pair (Symbol "if",Pair (Symbol "if_test",Pair (Symbol "if_then", Pair (Symbol "if_else", Nil)))),Nil)))], [Def (Var "if_fun",LambdaSimple (["if_test"; "if_then"; "if_else"],If (Var "if_test", Var "if_then", Var "if_else")))]);
  ("mit define expansion 7", Tag_Parser.tag_parse_expressions [Pair (Symbol "define",Pair (Pair (Symbol "pairing", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "quote",Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Nil)),Nil)))], [Def (Var "pairing",LambdaSimple (["a"; "b"],Const (Sexpr (Pair (Symbol "a", Pair (Symbol "b", Nil))))))]);
];;
