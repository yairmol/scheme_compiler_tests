#use "topfind";;
#directory "../";;
#directory "../..";;
#use "tag-parser.ml";;

(* tests for and, if, cond *)
let if_test_suite = [
  ("test_simple_if", Tag_Parser.tag_parse_expressions [
    Pair (Symbol "if", 
      Pair (Pair (Symbol "quote", Pair (Symbol "test", Nil)),
      Pair (Pair (Symbol "quote", Pair (Symbol "then", Nil)),
      Pair (Pair (Symbol "quote", Pair (Symbol "else", Nil)), Nil))))],
    [If (Const (Sexpr (Symbol "test")),
      Const (Sexpr (Symbol "then")),
      Const (Sexpr (Symbol "else")))
  ]);
  ("test_simple_if_no_else", 
    Tag_Parser.tag_parse_expressions [
      Pair (Symbol "if", 
        Pair (Pair (Symbol "quote", Pair (Symbol "well", Nil)),
        Pair (Pair (Symbol "quote", Pair (Symbol "you-know", Nil)), Nil)))],
    
    [If (Const (Sexpr (Symbol "well")),
        Const (Sexpr (Symbol "you-know")),
        Const Void)]);
  ("test_nested_if",
    Tag_Parser.tag_parse_expressions [
      Pair (Symbol "if", 
        Pair (Pair (Symbol "quote", Pair (Symbol "test", Nil)),
        Pair (Pair (Symbol "quote", Pair (Symbol "then", Nil)),
        Pair (Pair (Symbol "if",
          Pair (Number (Fraction (1, 1)),
          Pair (Number (Fraction (2, 1)),
          Pair (Number (Fraction (3, 1)), Nil)))), Nil))))],
    [If (Const (Sexpr (Symbol "test")),
        Const (Sexpr (Symbol "then")),
        If (Const (Sexpr (Number (Fraction (1,1)))),
            Const (Sexpr (Number (Fraction (2,1)))),
            Const (Sexpr (Number (Fraction (3,1))))))]);
  ("test_nested_if_no_else",
    Tag_Parser.tag_parse_expressions [
      Pair (Symbol "if", 
        Pair (Pair (Symbol "quote", Pair (Symbol "ocaml?", Nil)),
        Pair (Pair (Symbol "quote", Pair (Symbol "good", Nil)),
        Pair (Pair (Symbol "if",
          Pair (Pair (Symbol "quote", Pair (Symbol "java?", Nil)),
          Pair (Pair (Symbol "quote", Pair (Symbol "bad", Nil)), Nil))), Nil))))],
    [If (Const (Sexpr (Symbol "ocaml?")),
        Const (Sexpr (Symbol "good")),
        If (Const (Sexpr (Symbol "java?")),
            Const (Sexpr (Symbol "bad")),
            Const Void))]);
  ("and expansion base_case", Tag_Parser.tag_parse_expressions [Pair (Symbol "and", Nil)], [Const (Sexpr (Bool true))]);
  ("and expansion base_case_2", Tag_Parser.tag_parse_expressions [Pair (Symbol "and", Pair (Number (Fraction (1, 1)), Nil))], [Const (Sexpr (Number (Fraction (1, 1))))]);
  ("simple and expansion", Tag_Parser.tag_parse_expressions [Pair (Symbol "and", Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil))))], [If (Var "a", If (Var "b", Var "c", Const (Sexpr (Bool false))), Const (Sexpr (Bool false)))]);
  ("complex and expansion", Tag_Parser.tag_parse_expressions [
    Pair (Symbol "and",
      Pair (Pair (Symbol "quote", Pair (Nil, Nil)),
      Pair (Pair (Symbol "+", Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),
      Pair (Pair (Symbol "quote", Pair (Pair (Bool true, Pair (Char 'c', Pair (String "hello", Symbol ".sym?"))), Nil)),
     Pair (Bool false, Nil)))))], [
    If (
      Const (Sexpr Nil),
      If (
        Applic (Var "+", [Const (Sexpr (Number (Fraction (2, 1)))); Const (Sexpr (Number (Fraction (3, 1))))]),
        If (
          Const (Sexpr (Pair (Bool true, Pair (Char 'c', Pair (String "hello", Symbol ".sym?"))))),
          Const (Sexpr (Bool false)),
          Const (Sexpr (Bool false))
        ),
        Const (Sexpr (Bool false))  
      ),
      Const (Sexpr (Bool false))

    )
  ]);
  ("and expansion test 1", Tag_Parser.tag_parse_expressions [Pair (Symbol "and", Pair (Number (Fraction (1, 1)),Pair (Number (Fraction (2, 1)),Pair (Number (Fraction (3, 1)),Pair (Number (Fraction (4, 1)),Pair (Number (Fraction (5, 1)),Pair (Number (Fraction (6, 1)),Pair (Number (Fraction (7, 1)),Pair (Number (Fraction (8, 1)),Pair (Number (Fraction (9, 1)), Pair (Number (Fraction (10, 1)), Nil)))))))))))], [If (Const (Sexpr (Number (Fraction (1, 1)))),If (Const (Sexpr (Number (Fraction (2, 1)))),If (Const (Sexpr (Number (Fraction (3, 1)))),If (Const (Sexpr (Number (Fraction (4, 1)))),If (Const (Sexpr (Number (Fraction (5, 1)))),If (Const (Sexpr (Number (Fraction (6, 1)))),If (Const (Sexpr (Number (Fraction (7, 1)))),If (Const (Sexpr (Number (Fraction (8, 1)))),If (Const (Sexpr (Number (Fraction (9, 1)))), Const (Sexpr (Number (Fraction (10, 1)))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false)))]);
  ("cond expansion test 1", Tag_Parser.tag_parse_expressions [Pair (Symbol "cond", Pair(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Pair(Pair (Number (Fraction (4, 1)), Pair (Number (Fraction (5, 1)), Pair (Number (Fraction (6, 1)), Nil))),Nil)))], [If (Const (Sexpr (Number (Fraction (1, 1)))),Seq [Const (Sexpr (Number (Fraction (2, 1)))); Const (Sexpr (Number (Fraction (3, 1))))],If (Const (Sexpr (Number (Fraction (4, 1)))),Seq [Const (Sexpr (Number (Fraction (5, 1)))); Const (Sexpr (Number (Fraction (6, 1))))],Const Void))]);
  ("cond expansion test 2", Tag_Parser.tag_parse_expressions [Pair (Symbol "cond", Pair(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Pair(Pair (Number (Fraction (4, 1)), Pair (Number (Fraction (5, 1)), Pair (Number (Fraction (6, 1)), Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction (7, 1)), Pair (Number (Fraction (8, 1)), Pair (Number (Fraction (9, 1)), Nil)))),Nil))))], [If (Const (Sexpr (Number (Fraction (1, 1)))),Seq [Const (Sexpr (Number (Fraction (2, 1)))); Const (Sexpr (Number (Fraction (3, 1))))],If (Const (Sexpr (Number (Fraction (4, 1)))),Seq [Const (Sexpr (Number (Fraction (5, 1)))); Const (Sexpr (Number (Fraction (6, 1))))],Seq[Const (Sexpr (Number (Fraction (7, 1)))); Const (Sexpr (Number (Fraction (8, 1))));Const (Sexpr (Number (Fraction (9, 1))))]))]);
  ("cond expansion test 3", Tag_Parser.tag_parse_expressions [Pair (Symbol "cond", Pair(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction (4, 1)), Pair (Number (Fraction (5, 1)), Pair (Number (Fraction (6, 1)), Nil)))),Pair(Pair (Number (Fraction (7, 1)), Pair (Number (Fraction (8, 1)), Pair (Number (Fraction (9, 1)), Nil))),Nil))))], [If (Const (Sexpr (Number (Fraction (1, 1)))),Seq [Const (Sexpr (Number (Fraction (2, 1)))); Const (Sexpr (Number (Fraction (3, 1))))],Seq[Const (Sexpr (Number (Fraction (4, 1)))); Const (Sexpr (Number (Fraction (5, 1))));Const (Sexpr (Number (Fraction (6, 1))))])]);
  ("cond expansion test 4", Tag_Parser.tag_parse_expressions [Pair (Symbol "cond", Pair (Pair (Symbol "a", Pair (Symbol "=>", Pair (Symbol "b", Nil))), Nil)) ], [Applic(LambdaSimple (["value"; "f"],If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),Const Void)),[Var "a"; LambdaSimple ([], Var "b")])]);
  ("cond expansion test 5", Tag_Parser.tag_parse_expressions [Pair (Symbol "cond", Pair (Pair (Symbol "a", Pair (Symbol "=>", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil)))),Nil)))], [Applic(LambdaSimple (["value"; "f"; "rest"],If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),Applic (Var "rest", []))),[Var "a"; LambdaSimple ([], Var "b");LambdaSimple ([],Seq[Const (Sexpr (Number (Fraction (1, 1)))); Const (Sexpr (Number (Fraction (2, 1))));Const (Sexpr (Number (Fraction (3, 1))))])])]);
];;
