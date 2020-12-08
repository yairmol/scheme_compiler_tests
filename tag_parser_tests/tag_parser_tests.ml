#use "topfind";;
#directory "../";;
#directory "../..";;
#use "tag-parser.ml";;

(* tests for Const *)
let const_test_suite = [
  ("test_bool", [Bool true], [(Const(Sexpr (Bool (true))))]);
  ("test_number", [(Number (Float(1.23)))], [(Const(Sexpr (Number (Float(1.23)))))]);
  ("test_char", [Char ('a')], [(Const(Sexpr (Char ('a'))))]);
  ("test_string", [String ("hello")], [(Const(Sexpr (String ("hello"))))]);
  ("test_simple_quote", [Pair (Symbol "quote", Pair (Symbol "a", Nil))], [Const (Sexpr(Symbol "a"))]);
  ("test_complex_quote",
  [Pair (Symbol "quote", Pair (Pair (Symbol "a",
    Pair (Number (Fraction (2, 1)),
      Pair (Pair (String "well", Pair (Nil, Nil)), Nil))) , Nil))],
  [Const (Sexpr 
    (Pair (Symbol "a",
      Pair (Number (Fraction (2, 1)),
        Pair (Pair (String "well", Pair (Nil, Nil)), Nil)))))]
    );
  ("const_test_1", [Pair (Symbol "quote",Pair(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Nil))], [Const(Sexpr(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil)))))]);
  ("const_test_2", [Pair (Symbol "quote",Pair (Pair (Number (Float 1.2), Number (Float 3.4)), Nil))], [Const (Sexpr (Pair (Number (Float 1.2), Number (Float 3.4))))]);
  ("const_test_3", [Pair (Symbol "quote",Pair(Pair(Pair (Number (Float (-0.5)),Pair (Number (Float 999.99), Pair (Number (Float (-12.1)), Nil))),Pair (Number (Float 10.),Pair (Pair (Number (Fraction (1, 1)), Number (Fraction (2, 1))), Number (Float 1.)))),Nil))], [Const(Sexpr(Pair(Pair (Number (Float (-0.5)),Pair (Number (Float 999.99), Pair (Number (Float (-12.1)), Nil))),Pair (Number (Float 10.),Pair (Pair (Number (Fraction (1, 1)), Number (Fraction (2, 1))), Number (Float 1.))))))]);
  ("const_test_4", [Pair (Symbol "quote",Pair(Pair (Number (Float (-0.)),Pair (Number (Float 999.123),Pair (Pair (Number (Float 501.1), Number (Float 0.5)),Number (Float (-0.555))))),Nil))], [Const(Sexpr(Pair (Number (Float (-0.)),Pair (Number (Float 999.123),Pair (Pair (Number (Float 501.1), Number (Float 0.5)),Number (Float (-0.555)))))))]);
  ("const_test_5", [Pair (Symbol "quote",Pair(Pair (Number (Float (-0.52)),Pair (Pair (Number (Float 0.5234), Number (Float (-123.4))),Number (Float (-0.535)))),Nil))], [Const(Sexpr(Pair (Number (Float (-0.52)),Pair (Pair (Number (Float 0.5234), Number (Float (-123.4))),Number (Float (-0.535))))))]);
  ("const_test_6", [Pair (Symbol "quote", Pair (Pair (Number (Float 0.123), Nil), Nil))], [Const (Sexpr (Pair (Number (Float 0.123), Nil)))]);
  ("const_test_7", [Pair (Symbol "quote", Pair (Pair (Char '\000', Nil), Nil))], [Const (Sexpr (Pair (Char '\000', Nil)))]);
  ("const_test_8", [Char '\n'], [Const (Sexpr (Char '\n'))]);
  ("const_test_9", [Pair (Symbol "quote",Pair(Pair (Number (Fraction (1, 1)),Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Pair (Char '\012', Nil)))),Nil))], [Const(Sexpr(Pair (Number (Fraction (1, 1)),Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Pair (Char '\012', Nil))))))]);
  ("const_test_10", [Pair (Symbol "quote",Pair(Pair (Pair (Char '\t', Nil),Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Nil))),Nil))], [Const(Sexpr(Pair (Pair (Char '\t', Nil),Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Nil)))))]);
  ("const_test_11", [Pair (Symbol "quote",Pair(Pair (String "a",Pair (String "b", Pair (Pair (String "c", String "d"), String "e"))),Nil))], [Const(Sexpr(Pair (String "a",Pair (String "b", Pair (Pair (String "c", String "d"), String "e")))))]);
  ("const_test_12", [Pair (Symbol "quote",Pair(Pair (String "hello",Pair (String "world",Pair (Pair (Number (Float 1.2), Number (Fraction (3, 1))), Char '\000'))),Nil))], [Const(Sexpr(Pair (String "hello",Pair (String "world",Pair (Pair (Number (Float 1.2), Number (Fraction (3, 1))), Char '\000')))))]);
  ("const_test_13", [Pair (Symbol "quote",Pair (Pair (String "should", Pair (String "be", String "list")), Nil))], [Const (Sexpr (Pair (String "should", Pair (String "be", String "list"))))]);
  ("const_test_14", [Pair (Symbol "quote",Pair(Pair (String "vary",Pair (String "long",Pair (Number (Fraction (0, 1)),Pair (Number (Float 2.1),Pair (Number (Float (-4.2)),Pair (String "complex", Pair (Char '\r', Pair (String "list", Nil)))))))),Nil))], [Const(Sexpr(Pair (String "vary",Pair (String "long",Pair (Number (Fraction (0, 1)),Pair (Number (Float 2.1),Pair (Number (Float (-4.2)),Pair (String "complex",Pair (Char '\r', Pair (String "list", Nil))))))))))]);
  ("const_test_15", [Pair (Symbol "quote",Pair(Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "f", Symbol "g"))))),Nil))], [Const(Sexpr(Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "f", Symbol "g")))))))]);
  ("const_test_16", [Pair (Symbol "quote",Pair(Pair (String "a long long string",Pair (String "another long one",Pair(Pair (String "some numbers",Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Number (Fraction (3, 1))))),Pair (String "Named Char", Char ' ')))),Nil))], [Const(Sexpr(Pair (String "a long long string",Pair (String "another long one",Pair(Pair (String "some numbers",Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Number (Fraction (3, 1))))),Pair (String "Named Char", Char ' '))))))]);
  ("const_test_17", [Pair (Symbol "quote",Pair(Pair (Number (Float (-0.2)),Pair (Number (Float 1.5),Pair(Pair (Number (Fraction (1, 1)),Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Pair (Pair (Number (Fraction (4, 1)), Number (Fraction (5, 1))),Pair (Pair (Symbol "a", Pair (Symbol "b", Symbol "c")),Pair (Symbol "d", Symbol "f")))))),Nil))], [Const(Sexpr(Pair (Number (Float (-0.2)),Pair (Number (Float 1.5),Pair(Pair (Number (Fraction (1, 1)),Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Pair (Pair (Number (Fraction (4, 1)), Number (Fraction (5, 1))),Pair (Pair (Symbol "a", Pair (Symbol "b", Symbol "c")),Pair (Symbol "d", Symbol "f"))))))))]);
  ("const_test_18", [Pair (Symbol "quote",Pair(Pair (Number (Float 0.1),Pair (Number (Float (-0.2)),Pair (Number (Float 3.4),Pair (Number (Fraction (5, 1)),Pair (Number (Float 6.7), Pair (Number (Fraction (8, 1)), Number (Fraction (9, 1)))))))),Nil))], [Const(Sexpr(Pair (Number (Float 0.1),Pair (Number (Float (-0.2)),Pair (Number (Float 3.4),Pair (Number (Fraction (5, 1)),Pair (Number (Float 6.7), Pair (Number (Fraction (8, 1)), Number (Fraction (9, 1))))))))))]);
  ("const_test_19", [Pair (Symbol "quote",Pair(Pair (Number (Fraction (1, 1)),Pair (Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil)),Pair (Pair (Number (Fraction (4, 1)), Pair (Number (Float 0.5), Nil)),Pair(Pair (Symbol "c",Pair (Symbol "d",Pair (Pair (Symbol "f", Symbol "g"), String "Hello World"))),Symbol "z")))),Nil))], [Const(Sexpr(Pair (Number (Fraction (1, 1)),Pair (Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil)),Pair (Pair (Number (Fraction (4, 1)), Pair (Number (Float 0.5), Nil)),Pair(Pair (Symbol "c",Pair (Symbol "d",Pair (Pair (Symbol "f", Symbol "g"), String "Hello World"))),Symbol "z"))))))]);
  ("const_test_20", [String ""], [Const (Sexpr (String ""))]);
  ("const_test_21", [Pair (Symbol "quote",Pair (Pair (Number (Float 1.2), Pair (Number (Float 3.4), Nil)), Nil))], [Const (Sexpr (Pair (Number (Float 1.2), Pair (Number (Float 3.4), Nil))))]);
  ("const_test_22", [Pair (Symbol "quote", Pair(Pair (Pair (Char '\t', Nil), Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Char '\000'))),Nil))], [Const(Sexpr(Pair (Pair (Char '\t', Nil), Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Char '\000')))))]);
];;

(* tests for Define *)
let define_test_suite = [
  ("simple_define_test", [Pair (Symbol "define", Pair (Symbol "var", Pair (Bool true, Nil)))], [Def (Var "var", Const (Sexpr (Bool true)))]);
  ("complex_define_test", [
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
  ("mit define expansion 1", [Pair (Symbol "define",Pair (Pair (Symbol "square", Pair (Symbol "x", Nil)),Pair (Pair (Symbol "*", Pair (Symbol "x", Pair (Symbol "x", Nil))), Nil)))], [Def (Var "square",LambdaSimple (["x"], Applic (Var "*", [Var "x"; Var "x"])))]);
  ("mit define expansion 2", [Pair (Symbol "define",Pair (Pair (Symbol "cmp", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "if",Pair (Pair (Symbol ">", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (-1, 1)), Nil)))),Nil)))], [Def (Var "cmp",LambdaSimple (["a"; "b"],If (Applic (Var ">", [Var "a"; Var "b"]), Const (Sexpr (Number (Fraction (1, 1)))),Const (Sexpr (Number (Fraction (-1, 1)))))))]);
  ("mit define expansion 3", [Pair (Symbol "define",Pair(Pair (Symbol "square",Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil)))),Pair(Pair (Symbol "*",Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil)))),Nil)))], [Def (Var "square",LambdaSimple (["a"; "b"; "c"],Applic (Var "*", [Var "a"; Var "b"; Var "c"])))]);
  ("mit define expansion 4", [Pair (Symbol "define",Pair (Pair (Symbol "returnonly", Pair (Symbol "x", Nil)),Pair(Pair (Symbol "begin",Pair (String "return only", Pair (Symbol "x", Nil))),Nil)))], [Def (Var "returnonly",LambdaSimple (["x"], Seq [Const (Sexpr (String "return only")); Var "x"]))]);
  ("mit define expansion 5", [Pair (Symbol "define",Pair(Pair (Symbol "applic",Pair (Symbol "fun",Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "e", Nil))))))),Pair(Pair (Symbol "fun",Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "e", Nil)))))),Nil)))], [Def (Var "applic",LambdaSimple (["fun"; "a"; "b"; "c"; "d"; "e"],Applic (Var "fun", [Var "a"; Var "b"; Var "c"; Var "d"; Var "e"])))]);
  ("mit define expansion 6", [Pair (Symbol "define",Pair(Pair (Symbol "if_fun",Pair (Symbol "if_test",Pair (Symbol "if_then", Pair (Symbol "if_else", Nil)))),Pair(Pair (Symbol "if",Pair (Symbol "if_test",Pair (Symbol "if_then", Pair (Symbol "if_else", Nil)))),Nil)))], [Def (Var "if_fun",LambdaSimple (["if_test"; "if_then"; "if_else"],If (Var "if_test", Var "if_then", Var "if_else")))]);
  ("mit define expansion 7", [Pair (Symbol "define",Pair (Pair (Symbol "pairing", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "quote",Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Nil)),Nil)))], [Def (Var "pairing",LambdaSimple (["a"; "b"],Const (Sexpr (Pair (Symbol "a", Pair (Symbol "b", Nil))))))]);
];;

(* tests for and, if, cond *)
let if_test_suite = [
  ("test_simple_if", [
    Pair (Symbol "if", 
      Pair (Pair (Symbol "quote", Pair (Symbol "test", Nil)),
      Pair (Pair (Symbol "quote", Pair (Symbol "then", Nil)),
      Pair (Pair (Symbol "quote", Pair (Symbol "else", Nil)), Nil))))],
    [If (Const (Sexpr (Symbol "test")),
      Const (Sexpr (Symbol "then")),
      Const (Sexpr (Symbol "else")))
  ]);
  ("test_simple_if_no_else", 
    [
      Pair (Symbol "if", 
        Pair (Pair (Symbol "quote", Pair (Symbol "well", Nil)),
        Pair (Pair (Symbol "quote", Pair (Symbol "you-know", Nil)), Nil)))],
    
    [If (Const (Sexpr (Symbol "well")),
        Const (Sexpr (Symbol "you-know")),
        Const Void)]);
  ("test_nested_if",
    [
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
    [
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
  ("and expansion base_case", [Pair (Symbol "and", Nil)], [Const (Sexpr (Bool true))]);
  ("and expansion base_case_2", [Pair (Symbol "and", Pair (Number (Fraction (1, 1)), Nil))], [Const (Sexpr (Number (Fraction (1, 1))))]);
  ("simple and expansion", [Pair (Symbol "and", Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil))))], [If (Var "a", If (Var "b", Var "c", Const (Sexpr (Bool false))), Const (Sexpr (Bool false)))]);
  ("complex and expansion", [
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
  ("and expansion test 1", [Pair (Symbol "and", Pair (Number (Fraction (1, 1)),Pair (Number (Fraction (2, 1)),Pair (Number (Fraction (3, 1)),Pair (Number (Fraction (4, 1)),Pair (Number (Fraction (5, 1)),Pair (Number (Fraction (6, 1)),Pair (Number (Fraction (7, 1)),Pair (Number (Fraction (8, 1)),Pair (Number (Fraction (9, 1)), Pair (Number (Fraction (10, 1)), Nil)))))))))))], [If (Const (Sexpr (Number (Fraction (1, 1)))),If (Const (Sexpr (Number (Fraction (2, 1)))),If (Const (Sexpr (Number (Fraction (3, 1)))),If (Const (Sexpr (Number (Fraction (4, 1)))),If (Const (Sexpr (Number (Fraction (5, 1)))),If (Const (Sexpr (Number (Fraction (6, 1)))),If (Const (Sexpr (Number (Fraction (7, 1)))),If (Const (Sexpr (Number (Fraction (8, 1)))),If (Const (Sexpr (Number (Fraction (9, 1)))), Const (Sexpr (Number (Fraction (10, 1)))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false)))]);
  ("cond expansion test 1", [Pair (Symbol "cond", Pair(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Pair(Pair (Number (Fraction (4, 1)), Pair (Number (Fraction (5, 1)), Pair (Number (Fraction (6, 1)), Nil))),Nil)))], [If (Const (Sexpr (Number (Fraction (1, 1)))),Seq [Const (Sexpr (Number (Fraction (2, 1)))); Const (Sexpr (Number (Fraction (3, 1))))],If (Const (Sexpr (Number (Fraction (4, 1)))),Seq [Const (Sexpr (Number (Fraction (5, 1)))); Const (Sexpr (Number (Fraction (6, 1))))],Const Void))]);
  ("cond expansion test 2", [Pair (Symbol "cond", Pair(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Pair(Pair (Number (Fraction (4, 1)), Pair (Number (Fraction (5, 1)), Pair (Number (Fraction (6, 1)), Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction (7, 1)), Pair (Number (Fraction (8, 1)), Pair (Number (Fraction (9, 1)), Nil)))),Nil))))], [If (Const (Sexpr (Number (Fraction (1, 1)))),Seq [Const (Sexpr (Number (Fraction (2, 1)))); Const (Sexpr (Number (Fraction (3, 1))))],If (Const (Sexpr (Number (Fraction (4, 1)))),Seq [Const (Sexpr (Number (Fraction (5, 1)))); Const (Sexpr (Number (Fraction (6, 1))))],Seq[Const (Sexpr (Number (Fraction (7, 1)))); Const (Sexpr (Number (Fraction (8, 1))));Const (Sexpr (Number (Fraction (9, 1))))]))]);
  ("cond expansion test 3", [Pair (Symbol "cond", Pair(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction (4, 1)), Pair (Number (Fraction (5, 1)), Pair (Number (Fraction (6, 1)), Nil)))),Pair(Pair (Number (Fraction (7, 1)), Pair (Number (Fraction (8, 1)), Pair (Number (Fraction (9, 1)), Nil))),Nil))))], [If (Const (Sexpr (Number (Fraction (1, 1)))),Seq [Const (Sexpr (Number (Fraction (2, 1)))); Const (Sexpr (Number (Fraction (3, 1))))],Seq[Const (Sexpr (Number (Fraction (4, 1)))); Const (Sexpr (Number (Fraction (5, 1))));Const (Sexpr (Number (Fraction (6, 1))))])]);
  ("cond expansion test 4", [Pair (Symbol "cond", Pair (Pair (Symbol "a", Pair (Symbol "=>", Pair (Symbol "b", Nil))), Nil)) ], [Applic(LambdaSimple (["value"; "f"],If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),Const Void)),[Var "a"; LambdaSimple ([], Var "b")])]);
  ("cond expansion test 5", [Pair (Symbol "cond", Pair (Pair (Symbol "a", Pair (Symbol "=>", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil)))),Nil)))], [Applic(LambdaSimple (["value"; "f"; "rest"],If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),Applic (Var "rest", []))),[Var "a"; LambdaSimple ([], Var "b");LambdaSimple ([],Seq[Const (Sexpr (Number (Fraction (1, 1)))); Const (Sexpr (Number (Fraction (2, 1))));Const (Sexpr (Number (Fraction (3, 1))))])])]);
];;

(* tests for Lambda *)
let lambda_test_suite = [
  ("lambda_test_1", [Pair (Symbol "lambda", Pair (Nil, Pair (Number (Fraction (10, 1)), Nil))) ], [LambdaSimple ([], Const (Sexpr (Number (Fraction (10, 1)))))]);
  ("lambda_test_2", [Pair (Symbol "lambda", Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Pair (Symbol "a", Nil))) ], [LambdaSimple (["a"; "b"], Var "a")]);
  ("lambda_test_3", [Pair (Symbol "lambda", Pair (Pair (Symbol "a", Nil), Pair (Symbol "a", Nil))) ], [LambdaSimple (["a"], Var "a")]);
  ("lambda_test_4", [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),Pair (Pair (Symbol "+", Pair (Symbol "x", Pair (Symbol "y", Nil))), Nil)))], [LambdaSimple (["x"; "y"], Applic (Var "+", [Var "x"; Var "y"]))]);
  ("lambda_test_5", [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil))),Pair(Pair (Symbol "if",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil)))),Nil)))], [LambdaSimple (["x"; "y"; "z"], If (Var "x", Var "y", Var "z"))]);
  ("lambda_test_6", [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil))),Pair(Pair (Symbol "begin",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil)))),Nil)))], [LambdaSimple (["x"; "y"; "z"], Seq [Var "x"; Var "y"; Var "z"])]);
  ("lambda_test_7", [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),Pair (Pair (Symbol "set", Pair (Symbol "x", Pair (Symbol "y", Nil))), Nil)))], [LambdaSimple (["x"; "y"], Applic (Var "set", [Var "x"; Var "y"]))]);
  ("lambda_test_8", [Pair (Symbol "lambda", Pair(Pair (Symbol "x",Pair (Symbol "y", Pair (Symbol "z", Pair (Symbol "w", Nil)))),Pair(Pair (Symbol "if",Pair (Symbol "x",Pair (Pair (Symbol "+", Pair (Symbol "y", Pair (Symbol "z", Nil))),Pair (Pair (Symbol "+", Pair (Symbol "z", Pair (Symbol "w", Nil))), Nil)))),Nil)))], [LambdaSimple (["x"; "y"; "z"; "w"],If (Var "x", Applic (Var "+", [Var "y"; Var "z"]),Applic (Var "+", [Var "z"; Var "w"])))]);
  ("lambda_test_9", [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),Pair (Pair (Symbol "or", Pair (Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil))), Nil)), Nil))) ], [LambdaSimple (["x"; "y"], Applic (Var "x", [Var "y"; Var "z"]))]);
  ("lambda_test_10", [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "vs")),Pair(Pair (Symbol "begin",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "vs", Nil)))),Nil)))], [LambdaOpt (["x"; "y"], "vs", Seq [Var "x"; Var "y"; Var "vs"])]);
  ("lambda_test_11", [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Symbol "vs"),Pair (Pair (Symbol "if", Pair (Symbol "x", Pair (Symbol "vs", Nil))), Nil)))], [LambdaOpt (["x"], "vs", If (Var "x", Var "vs", Const Void))]);
  ("lambda_test_12", [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "vs")),Pair(Pair (Symbol "and",Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil)))),Nil)))], [LambdaOpt (["x"; "y"], "vs",If (Const (Sexpr (Number (Fraction (1, 1)))),If (Const (Sexpr (Number (Fraction (2, 1)))), Const (Sexpr (Number (Fraction (3, 1)))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))))]);
  ("lambda_test_13", [Pair (Symbol "lambda", Pair(Pair (Symbol "a",Pair (Symbol "b", Pair (Symbol "c", Pair (Symbol "d", Symbol "vs")))),Pair(Pair (Symbol "if",Pair (Pair (Symbol ">", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair (Pair (Symbol "+", Pair (Symbol "c", Pair (Symbol "d", Nil))),Pair (Pair (Symbol "list", Pair (Symbol "vs", Nil)), Nil)))),Nil)))], [LambdaOpt (["a"; "b"; "c"; "d"], "vs",If (Applic (Var ">", [Var "a"; Var "b"]),Applic (Var "+", [Var "c"; Var "d"]), Applic (Var "list", [Var "vs"])))]);
  ("lambda_test_14", [Pair (Symbol "lambda", Pair (Pair (Symbol "b", Symbol "vs"),Pair(Pair (Symbol "begin",Pair (Symbol "b",Pair(Pair (Symbol "define", Pair (Symbol "x", Pair (Number (Fraction (10, 1)), Nil))),Pair(Pair (Symbol "set",Pair (Symbol "b",Pair(Pair (Symbol "+", Pair (Symbol "x", Pair (Number (Fraction (15, 1)), Nil))),Nil))),Nil)))),Nil)))], [LambdaOpt (["b"], "vs",Seq[Var "b"; Def (Var "x", Const (Sexpr (Number (Fraction (10, 1)))));Applic (Var "set",[Var "b"; Applic (Var "+", [Var "x"; Const (Sexpr (Number (Fraction (15, 1))))])])])]);
  ("lambda_test_15", [Pair (Symbol "lambda", Pair (Pair (Symbol "a", Pair (Symbol "b", Symbol "vs")),Pair(Pair (Symbol "cond",Pair (Pair (Symbol "a", Pair (Number (Fraction (1, 1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction (2, 1)), Nil)),Pair(Pair (Symbol "else",Pair (Pair (Symbol "+", Pair (Symbol "a", Pair (Symbol "b", Nil))),Nil)),Nil)))),Nil)))], [LambdaOpt (["a"; "b"], "vs",If (Var "a", Const (Sexpr (Number (Fraction (1, 1)))),If (Var "b", Const (Sexpr (Number (Fraction (2, 1)))),Applic (Var "+", [Var "a"; Var "b"]))))]);
  ("lambda_test_16", [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Symbol "vs"), Pair (Symbol "vs", Nil))) ], [LambdaOpt (["x"], "vs", Var "vs")]);
  ("lambda_test_17", [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "vs")),Pair(Pair(Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "x", Nil)),Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "y", Nil)), Nil)),Nil)),Nil),Nil)))], [LambdaOpt (["x"; "y"], "vs",Applic(Applic (Var "cons",[Var "x"; Applic (Var "append", [Var "y"; Const (Sexpr Nil)])]),[]))]);
  ("lambda_test_18", [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "vs")),Pair(Pair (Symbol "and",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "vs", Nil)))),Nil)))], [LambdaOpt (["x"; "y"], "vs",If (Var "x", If (Var "y", Var "vs", Const (Sexpr (Bool false))),Const (Sexpr (Bool false))))]);
  ("lambda_test_19", [Pair (Symbol "lambda",Pair (Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Symbol "d"))),Pair(Pair (Symbol "quasiquote",Pair(Pair(Pair (Symbol "a",Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)), Nil)),Pair(Pair (Symbol "b",Pair (Pair (Symbol "unquote", Pair (Symbol "b", Nil)), Nil)),Pair(Pair (Symbol "c",Pair (Pair (Symbol "unquote", Pair (Symbol "c", Nil)), Nil)),Pair(Pair (Symbol "d",Pair (Pair (Symbol "unquote", Pair (Symbol "d", Nil)), Nil)),Nil)))),Nil)),Nil)))], [LambdaOpt (["a"; "b"; "c"], "d",Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "cons", [Var "a"; Const (Sexpr Nil)])]);Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "b"));Applic (Var "cons", [Var "b"; Const (Sexpr Nil)])]);Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "c"));Applic (Var "cons", [Var "c"; Const (Sexpr Nil)])]);Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "d"));Applic (Var "cons", [Var "d"; Const (Sexpr Nil)])]);Const (Sexpr Nil)])])])]))]);
];;

let let_test_suite = [
  ("let expansion test 1", [Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Fraction (1, 1)), Nil)), Pair (Pair (Symbol "y", Pair (Number (Fraction (2, 1)), Nil)), Nil)), Pair (Symbol "y", Nil)))], [Applic (LambdaSimple (["x"; "y"], Var "y"),[Const (Sexpr (Number (Fraction (1, 1)))); Const (Sexpr (Number (Fraction (2, 1))))])]);
  ("let expansion test 2", [Pair (Symbol "let", Pair (Nil, Pair (Number (Fraction (10, 1)), Nil)))], [Applic (LambdaSimple ([], Const (Sexpr (Number (Fraction (10, 1))))), [])]);
  ("let expansion test 3", [Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Fraction (1, 1)), Nil)), Nil), Pair (Symbol "x", Nil)))], [Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Number (Fraction (1, 1))))])]);
  ("let expansion test 4", [Pair (Symbol "let", Pair (Nil, Pair (Pair (Symbol "begin", Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil)))), Nil)))], [Applic(LambdaSimple ([],Seq[Const (Sexpr (Number (Fraction (1, 1)))); Const (Sexpr (Number (Fraction (2, 1))));Const (Sexpr (Number (Fraction (3, 1))))]),[])]);
  ("let expansion test 5", [Pair (Symbol "let", Pair (Pair (Pair (Symbol "a", Pair (Number (Fraction (3, 1)), Nil)), Pair (Pair (Symbol "b", Pair (Number (Fraction (10, 1)), Nil)), Nil)), Pair (Pair (Symbol "+", Pair (Symbol "a", Pair (Symbol "b", Nil))), Nil)))], [Applic (LambdaSimple (["a"; "b"], Applic (Var "+", [Var "a"; Var "b"])),[Const (Sexpr (Number (Fraction (3, 1)))); Const (Sexpr (Number (Fraction (10, 1))))])]);
  ("let expansion test 6", [Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Char 'a', Nil)), Nil), Pair (Symbol "x", Nil)))], [Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Char 'a'))])]);
  ("let expansion test 7", [Pair (Symbol "let", Pair (Pair (Pair (Symbol "t", Pair (Bool true, Nil)), Pair (Pair (Symbol "th", Pair (Number (Fraction (3, 1)), Nil)), Pair (Pair (Symbol "el", Pair (Number (Fraction (4, 1)), Nil)), Nil))), Pair (Pair (Symbol "if", Pair (Symbol "t", Pair (Symbol "th", Pair (Symbol "el", Nil)))), Nil)))], [Applic (LambdaSimple (["t"; "th"; "el"], If (Var "t", Var "th", Var "el")),[Const (Sexpr (Bool true)); Const (Sexpr (Number (Fraction (3, 1))));Const (Sexpr (Number (Fraction (4, 1))))])]);
  ("let expansion test 8", [Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil), Pair (Pair (Symbol "define", Pair (Symbol "y", Pair (Number (Float 1.23), Nil))), Nil)))], [Applic(LambdaSimple (["x"], Def (Var "y", Const (Sexpr (Number (Float 1.23))))),[Const (Sexpr (String "asd"))])]);
  ("let expansion test 9", [Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil), Pair (Pair (Symbol "begin", Pair (Pair (Symbol "define", Pair (Symbol "y", Pair (Number (Float 1.23), Nil))), Pair (Pair (Symbol "set", Pair (Symbol "y", Pair (Number (Fraction (-1, 1)), Nil))), Nil))), Nil)))], [Applic(LambdaSimple (["x"],Seq[Def (Var "y", Const (Sexpr (Number (Float 1.23))));Applic (Var "set", [Var "y"; Const (Sexpr (Number (Fraction (-1, 1))))])]),[Const (Sexpr (String "asd"))])]);
  ("let expansion test 10", [Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil), Pair (Pair (Symbol "begin", Pair (Symbol "x", Nil)), Nil)))], [Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (String "asd"))])]);
  ("let* expansion test 1", [Pair (Symbol "let*", Pair(Pair (Pair (Symbol "x", Pair (Number (Fraction (1, 1)), Nil)),Pair (Pair (Symbol "y", Pair (Number (Fraction (2, 1)), Nil)), Nil)),Pair (Symbol "y", Nil)))], [Applic(LambdaSimple (["x"],Applic (LambdaSimple (["y"], Var "y"), [Const (Sexpr (Number (Fraction (2, 1))))])),[Const (Sexpr (Number (Fraction (1, 1))))])]);
  ("let* expansion test 2", [Pair (Symbol "let*", Pair(Pair (Pair (Symbol "x", Pair (Number (Fraction (1, 1)), Nil)),Pair (Pair (Symbol "y", Pair (Number (Fraction (2, 1)), Nil)),Pair (Pair (Symbol "z", Pair (Number (Fraction (3, 1)), Nil)),Pair (Pair (Symbol "a", Pair (Number (Fraction (4, 1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction (5, 1)), Nil)),Pair (Pair (Symbol "c", Pair (Number (Fraction (6, 1)), Nil)), Nil)))))),Pair(Pair (Symbol "begin",Pair (Symbol "x",Pair (Symbol "y",Pair (Symbol "z",Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil))))))),Nil)))], [Applic(LambdaSimple (["x"],Applic(LambdaSimple (["y"],Applic(LambdaSimple (["z"],Applic(LambdaSimple (["a"],Applic(LambdaSimple (["b"],Applic(LambdaSimple (["c"],Seq [Var "x"; Var "y"; Var "z"; Var "a"; Var "b"; Var "c"]),[Const (Sexpr (Number (Fraction (6, 1))))])),[Const (Sexpr (Number (Fraction (5, 1))))])),[Const (Sexpr (Number (Fraction (4, 1))))])),[Const (Sexpr (Number (Fraction (3, 1))))])),[Const (Sexpr (Number (Fraction (2, 1))))])),[Const (Sexpr (Number (Fraction (1, 1))))])]);
  ("let* expansion test 3", [Pair (Symbol "let*", Pair(Pair (Pair (Symbol "a", Pair (Number (Fraction (1, 1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction (2, 1)), Nil)),Pair (Pair (Symbol "c", Pair (Number (Fraction (3, 1)), Nil)),Pair (Pair (Symbol "d", Pair (Number (Fraction (4, 1)), Nil)),Pair (Pair (Symbol "e", Pair (Number (Fraction (5, 1)), Nil)),Pair (Pair (Symbol "f", Pair (Number (Fraction (5, 1)), Nil)),Pair (Pair (Symbol "g", Pair (Number (Fraction (6, 1)), Nil)), Nil))))))),Pair(Pair (Symbol "and",Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c",Pair (Symbol "d",Pair (Symbol "e", Pair (Symbol "f", Pair (Symbol "g", Nil)))))))),Nil)))], [Applic(LambdaSimple (["a"],Applic(LambdaSimple (["b"],Applic(LambdaSimple (["c"],Applic(LambdaSimple (["d"],Applic(LambdaSimple (["e"],Applic(LambdaSimple (["f"],Applic(LambdaSimple (["g"],If (Var "a",If (Var "b",If (Var "c",If (Var "d",If (Var "e",If (Var "f", Var "g", Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false)))),[Const (Sexpr (Number (Fraction (6, 1))))])),[Const (Sexpr (Number (Fraction (5, 1))))])),[Const (Sexpr (Number (Fraction (5, 1))))])),[Const (Sexpr (Number (Fraction (4, 1))))])),[Const (Sexpr (Number (Fraction (3, 1))))])),[Const (Sexpr (Number (Fraction (2, 1))))])),[Const (Sexpr (Number (Fraction (1, 1))))])]);
  ("let* expansion test 4", [Pair (Symbol "let*", Pair (Nil,Pair(Pair (Symbol "begin",Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil)))),Nil)))], [Applic(LambdaSimple ([],Seq[Const (Sexpr (Number (Fraction (1, 1)))); Const (Sexpr (Number (Fraction (2, 1))));Const (Sexpr (Number (Fraction (3, 1))))]),[])]);
  ("let* expansion test 5", [Pair (Symbol "let*", Pair(Pair (Pair (Symbol "a", Pair (Number (Fraction (3, 1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction (10, 1)), Nil)), Nil)),Pair (Pair (Symbol "+", Pair (Symbol "a", Pair (Symbol "b", Nil))), Nil)))], [Applic(LambdaSimple (["a"],Applic (LambdaSimple (["b"], Applic (Var "+", [Var "a"; Var "b"])),[Const (Sexpr (Number (Fraction (10, 1))))])),[Const (Sexpr (Number (Fraction (3, 1))))])]);
  ("let* expansion test 6", [Pair (Symbol "let*", Pair (Pair (Pair (Symbol "x", Pair (Char 'a', Nil)), Nil),Pair (Symbol "x", Nil)))], [Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Char 'a'))])]);
  ("let* expansion test 7", [Pair (Symbol "let*", Pair(Pair (Pair (Symbol "t", Pair (Bool true, Nil)),Pair (Pair (Symbol "th", Pair (Number (Fraction (3, 1)), Nil)),Pair (Pair (Symbol "el", Pair (Number (Fraction (4, 1)), Nil)), Nil))),Pair(Pair (Symbol "if",Pair (Bool true, Pair (Number (Fraction (3, 1)), Pair (Number (Fraction (4, 1)), Nil)))),Nil)))], [Applic(LambdaSimple (["t"],Applic(LambdaSimple (["th"],Applic(LambdaSimple (["el"],If (Const (Sexpr (Bool true)), Const (Sexpr (Number (Fraction (3, 1)))),Const (Sexpr (Number (Fraction (4, 1)))))),[Const (Sexpr (Number (Fraction (4, 1))))])),[Const (Sexpr (Number (Fraction (3, 1))))])),[Const (Sexpr (Bool true))])]);
  ("let* expansion test 8", [Pair (Symbol "let*", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil),Pair(Pair (Symbol "define", Pair (Symbol "y", Pair (Number (Float 12.3), Nil))),Nil)))], [Applic(LambdaSimple (["x"], Def (Var "y", Const (Sexpr (Number (Float 12.3))))),[Const (Sexpr (String "asd"))])]);
  ("let* expansion test 9", [Pair (Symbol "let*", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil),Pair(Pair (Symbol "begin",Pair(Pair (Symbol "define",Pair (Symbol "y", Pair (Number (Float 1.23), Nil))),Pair(Pair (Symbol "set", Pair (Symbol "y", Pair (Number (Fraction (-1, 1)), Nil))),Nil))),Nil)))], [Applic(LambdaSimple (["x"],Seq[Def (Var "y", Const (Sexpr (Number (Float 1.23))));Applic (Var "set", [Var "y"; Const (Sexpr (Number (Fraction (-1, 1))))])]),[Const (Sexpr (String "asd"))])]);
  ("let* expansion test 10", [Pair (Symbol "let*", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil),Pair (Pair (Symbol "begin", Pair (Symbol "x", Nil)), Nil)))], [Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (String "asd"))])]);
];;

let quasiquote_test_suite = [
  ("quasiquote expansion test 1", [Pair (Symbol "quasiquote", Pair (Pair (Pair (Symbol "unquote", Pair (Symbol "x", Nil)), Nil), Nil)) ], [Applic (Var "cons", [Var "x"; Const (Sexpr Nil)])]);
  ("quasiquote expansion test 2", [Pair (Symbol "quasiquote", Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Nil)) ], [Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])]);
  ("quasiquote expansion test 3", [Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),Pair (Symbol "b", Nil)),Nil))], [Applic (Var "cons",[Var "a";Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])]);
  ("quasiquote expansion test 4", [Pair (Symbol "quasiquote", Pair(Pair (Symbol "a",Pair (Pair (Symbol "unquote", Pair (Symbol "b", Nil)), Nil)),Nil))], [Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "cons", [Var "b"; Const (Sexpr Nil)])])]);
  ("quasiquote expansion test 5", [Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Symbol "b", Nil)),Nil))], [Applic (Var "append",[Var "a";Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])]);
  ("quasiquote expansion test 6", [Pair (Symbol "quasiquote", Pair(Pair (Symbol "a",Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))], [Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])]);
  ("quasiquote expansion test 7", [Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))], [Applic (Var "cons",[Var "a"; Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])]);
  ("quasiquote expansion test 8", [Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))], [Applic (Var "append",[Var "a"; Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])]);
  ("quasiquote expansion test 9", [Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Symbol "unquote", Pair (Symbol "b", Nil))),Nil))], [Applic (Var "append", [Var "a"; Var "b"])]);
  ("quasiquote expansion test 10", [Pair (Symbol "quasiquote", Pair(Pair(Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)), Nil),Nil),Nil),Nil))], [Applic (Var "cons",[Applic (Var "cons",[Applic (Var "append", [Var "a"; Const (Sexpr Nil)]); Const (Sexpr Nil)]);Const (Sexpr Nil)])]);
];;

let sequence_test_suite = [
  ("test_empty_sequence", [Pair (Symbol "begin", Nil)], [Const Void]);
  ("test_single_expr_sequence", [Pair (Symbol "begin", Pair (Number (Float 1.2), Nil))], [Const (Sexpr (Number (Float 1.2)))]);
  ("test_single_expr_sequence_2", [Pair (Symbol "begin", Pair (Pair (Symbol "if", Pair (Symbol "?", Pair (Symbol "!", Nil))), Nil))], [If (Var "?", Var "!", Const Void)]);
  (let expr1 = Number (Float 1.234) in 
  let expr2 = Char ('c') in 
  let expr3 = String ("on the fly") in
    ("test_regular_sequence", 
      [
        Pair (Symbol "begin",
          Pair (expr1,
          Pair (expr2,
          Pair (expr3, Nil))))], 
      [Seq ([Const (Sexpr expr1); Const (Sexpr expr2); Const (Sexpr expr3)])]));
  ("test_nested_sequence_depth_one",
    (let seq_inner =
      let expr1 = Pair (Symbol "quote", Pair (Symbol "inner_seq", Nil)) in
      let expr2 = Bool (true) in 
        Pair (Symbol "begin", Pair (expr1, Pair (expr2, Nil))) in
      let expr1 = Pair (Symbol "set!", Pair (Symbol "var", Pair (Bool (true), Nil))) in
      let expr2 = seq_inner in
      let expr3 = Pair (Symbol "display", Pair (Symbol "var", Nil)) in
      let expr4 = Symbol "var" in
    [
      Pair (Symbol "begin", 
        Pair (expr1,
        Pair (expr2,
        Pair (expr3,
        Pair (expr4, Nil)))))]),
    [Seq [
      Set (Var "var", Const (Sexpr (Bool true)));
      Const (Sexpr (Symbol "inner_seq"));
      Const (Sexpr (Bool true));
      Applic (Var "display", [Var "var"]);
      Var ("var")]]);
  ("test_deep_nested_sequence",
      (let seq1 = Pair (Symbol "begin", Pair (Number (Float (512.23)), Pair (Char ('v'), Nil))) in
      let seq2 = Pair (Symbol "begin", Pair (String "just a string", Pair (Bool (true), Nil))) in
      let seq3 = Pair (Symbol "begin", Pair (Pair (Symbol "set!", Pair (Symbol "var", Pair (String "well", Nil))), Pair (Pair (Symbol "display", Pair (Symbol "var", Nil)), Nil))) in
      let seq4 = Pair (Symbol "begin", Pair (Pair (Symbol "quote", Pair (Symbol "last_sequence!", Nil)), Nil)) in
      [
      Pair (Symbol "begin", 
        Pair (seq1,
        Pair (seq2,
        Pair (seq3,
        Pair (seq4, Nil)))))]),
    [Seq [
      Const(Sexpr(Number(Float(512.23)))) ; Const(Sexpr(Char('v'))) ; 
      Const(Sexpr(String "just a string")) ; Const(Sexpr(Bool true)) ; 
      Set(Var "var", Const(Sexpr(String "well"))) ; Applic (Var "display", [Var "var"]) ;
      Const(Sexpr(Symbol "last_sequence!"))
    ]]);
  ("test_simple_implicit_sequence", 
    [
      Pair (Symbol "lambda", Pair (Nil, 
        Pair (Symbol "expr1",
        Pair (Symbol "expr2",
        Pair (Bool true,
        Pair (Char 't', Nil))))))
    ],
    [
      LambdaSimple ([], 
        Seq [
          Var "expr1";
          Var "expr2";
          Const (Sexpr (Bool true));
          Const (Sexpr (Char 't')) 
        ])
    ]);
  ("test_nested_implicit_sequence_d1", 
    [
      Pair (Symbol "lambda", Pair (Nil, 
        Pair (Symbol "expr1",
        Pair (Symbol "expr2",
        Pair (Pair (Symbol "begin",
          Pair (Symbol "inner",
          Pair (Number (Float 3.1), Nil))),
        Pair (Bool true,
        Pair (Char 't', Nil)))))))
    ],
    [
      LambdaSimple ([], 
        Seq [
          Var "expr1";
          Var "expr2";
          Var "inner";
          Const (Sexpr (Number (Float 3.1)));
          Const (Sexpr (Bool true));
          Const (Sexpr (Char 't')); 
        ])
    ]);
];;
