#use "topfind";;
#directory "../";;
#directory "../../";;
#use "semantic-analyser.ml";;

let semantic_analyzer_tests_suite = [
  ("semantic test 1",
    LambdaSimple ([], Const (Sexpr (Number (Fraction (1, 1))))),
    LambdaSimple' ([], Const' (Sexpr (Number (Fraction (1, 1))))));
  ("semantic test 2",
    Const
      (Sexpr
        (Pair
          (Pair (Symbol "lambda",
            Pair (Nil,
             Pair
              (Pair (Symbol "lambda",
                Pair (Pair (Symbol "x", Nil),
                 Pair (Symbol "x",
                  Pair
                   (Pair (Symbol "lambda",
                     Pair (Nil,
                      Pair
                       (Pair (Symbol "set!",
                         Pair (Symbol "x", Pair (Number (Fraction (1, 1)), Nil))),
                       Nil))),
                   Nil)))),
              Nil))),
          Nil))),
    
    Const'
     (Sexpr
       (Pair
         (Pair (Symbol "lambda",
           Pair (Nil,
            Pair
             (Pair (Symbol "lambda",
               Pair (Pair (Symbol "x", Nil),
                Pair (Symbol "x",
                 Pair
                  (Pair (Symbol "lambda",
                    Pair (Nil,
                     Pair
                      (Pair (Symbol "set!",
                        Pair (Symbol "x", Pair (Number (Fraction (1, 1)), Nil))),
                      Nil))),
                  Nil)))),
             Nil))),
         Nil))));
  ("semantic test 3",
    Applic
      (LambdaSimple (["x"],
        If (Applic (Var "x", [Const (Sexpr (Number (Fraction (1, 1))))]),
         Applic (Var "x", [Const (Sexpr (Number (Fraction (2, 1))))]),
         Applic
          (LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Number (Fraction (0, 1)))))),
          [Const (Sexpr (Number (Fraction (3, 1))))]))),
      [LambdaSimple (["x"], Var "x")]),
    
    Applic'
     (LambdaSimple' (["x"],
       If'
        (Applic' (Var' (VarParam ("x", 0)), [Const' (Sexpr (Number (Fraction (1, 1))))]),
        ApplicTP' (Var' (VarParam ("x", 0)), [Const' (Sexpr (Number (Fraction (2, 1))))]),
        ApplicTP'
         (LambdaSimple' (["x"],
           Set' ((VarParam ("x", 0)), Const' (Sexpr (Number (Fraction (0, 1)))))),
         [Const' (Sexpr (Number (Fraction (3, 1))))]))),
     [LambdaSimple' (["x"], Var' (VarParam ("x", 0)))]));
  ("semantic test 4",
    LambdaSimple (["x"],
      Or
       [Applic
         (LambdaOpt (["y"], "z",
           Applic
            (LambdaSimple ([],
              Applic (LambdaSimple ([], Applic (Var "+", [Var "x"; Var "z"])), [])),
            [])),
         [Var "x"; Const (Sexpr (Number (Fraction (1, 1))))]);
        LambdaSimple ([], Set (Var "x", Var "w")); Applic (Var "w", [Var "w"])]),
    
    LambdaSimple' (["x"],
     Seq'
      [Set' ((VarParam ("x", 0)), Box' (VarParam ("x", 0)));
       Or'
        [Applic'
          (LambdaOpt' (["y"], "z",
            ApplicTP'
             (LambdaSimple' ([],
               ApplicTP'
                (LambdaSimple' ([],
                  ApplicTP' (Var' (VarFree "+"),
                   [BoxGet' (VarBound ("x", 2, 0)); Var' (VarBound ("z", 1, 1))])),
                [])),
             [])),
          [BoxGet' (VarParam ("x", 0)); Const' (Sexpr (Number (Fraction (1, 1))))]);
         LambdaSimple' ([], BoxSet' (VarBound ("x", 0, 0), Var' (VarFree "w")));
         ApplicTP' (Var' (VarFree "w"), [Var' (VarFree "w")])]]));
  ("semantic test 5",
    If (Applic (LambdaSimple (["y"], Var "x"), []),
      Applic
       (LambdaSimple (["x"],
         Seq
          [Set (Var "x", Var "y");
           LambdaSimple ([], Set (Var "x", Const (Sexpr (Number (Fraction (1, 1))))))]),
       [Const (Sexpr (Symbol "a"))]),
      LambdaSimple (["x"], Set (Var "x", Var "y"))),
    
    If' (Applic' (LambdaSimple' (["y"], Var' (VarFree "x")), []),
     Applic'
      (LambdaSimple' (["x"],
        Seq'
         [Set' ((VarParam ("x", 0)), Var' (VarFree "y"));
          LambdaSimple' ([],
           Set' ((VarBound ("x", 0, 0)), Const' (Sexpr (Number (Fraction (1, 1))))))]),
      [Const' (Sexpr (Symbol "a"))]),
     LambdaSimple' (["x"], Set' ((VarParam ("x", 0)), Var' (VarFree "y")))));
  ("semantic test 6",
    LambdaOpt (["x"; "y"; "z"], "w",
      Seq
       [Var "z";
        Applic
         (LambdaSimple ([],
           Seq [Set (Var "w", Var "w"); Applic (Applic (Var "y", [Var "x"]), [])]),
         [])]),
    
    LambdaOpt' (["x"; "y"; "z"], "w",
     Seq'
      [Var' (VarParam ("z", 2));
       ApplicTP'
        (LambdaSimple' ([],
          Seq'
           [Set' ((VarBound ("w", 0, 3)), Var' (VarBound ("w", 0, 3)));
            ApplicTP'
             (Applic' (Var' (VarBound ("y", 0, 1)),
               [Var' (VarBound ("x", 0, 0))]),
             [])]),
        [])]));
  ("semantic test 7",
    Def (Var "a",
      Applic
       (LambdaSimple ([],
         LambdaOpt ([], "x",
          Seq
           [Var "x";
            LambdaOpt ([], "y", Set (Var "y", Const (Sexpr (Number (Fraction (1, 1))))))])),
       [])),
    
    Def' ((VarFree "a"),
     Applic'
      (LambdaSimple' ([],
        LambdaOpt' ([], "x",
         Seq'
          [Var' (VarParam ("x", 0));
           LambdaOpt' ([], "y",
            Set' ((VarParam ("y", 0)), Const' (Sexpr (Number (Fraction (1, 1))))))])),
      [])));
  ("semantic test 8",
    LambdaSimple (["x"; "y"],
      Seq
       [Applic (Var "x", [Var "y"]);
        LambdaSimple ([],
         LambdaSimple ([],
          LambdaSimple ([],
           Set (Var "x",
            Applic (LambdaSimple (["z"], Set (Var "y", Var "x")), [Var "y"])))))]),
    
    LambdaSimple' (["x"; "y"],
     Seq'
      [Set' ((VarParam ("x", 0)), Box' (VarParam ("x", 0)));
       Set' ((VarParam ("y", 1)), Box' (VarParam ("y", 1)));
       Applic' (BoxGet' (VarParam ("x", 0)), [BoxGet' (VarParam ("y", 1))]);
         LambdaSimple' ([],
          LambdaSimple' ([],
           LambdaSimple' ([],
            BoxSet' (VarBound ("x", 2, 0),
             Applic'
              (LambdaSimple' (["z"],
                BoxSet' (VarBound ("y", 3, 1), BoxGet' (VarBound ("x", 3, 0)))),
              [BoxGet' (VarBound ("y", 2, 1))])))))]));
  ("semantic test 9",
    LambdaSimple ([],
      Seq
       [Applic (LambdaSimple ([], Var "x"), []);
        Applic
         (LambdaSimple (["x"],
           Seq
            [Set (Var "x", Const (Sexpr (Number (Fraction (1, 1)))));
             LambdaSimple ([], Var "x")]),
         [Const (Sexpr (Number (Fraction (2, 1))))]);
        Applic (LambdaOpt ([], "x", Var "x"), [Const (Sexpr (Number (Fraction (3, 1))))])]),
    
    LambdaSimple' ([],
     Seq'
      [Applic' (LambdaSimple' ([], Var' (VarFree "x")), []);
       Applic'
        (LambdaSimple' (["x"],
          Seq'
           [Set' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction (1, 1)))));
            LambdaSimple' ([], Var' (VarBound ("x", 0, 0)))]),
        [Const' (Sexpr (Number (Fraction (2, 1))))]);
       ApplicTP' (LambdaOpt' ([], "x", Var' (VarParam ("x", 0))),
        [Const' (Sexpr (Number (Fraction (3, 1))))])]));
  ("semantic test 10",
    LambdaSimple (["x"; "y"; "z"],
      Seq
       [LambdaSimple (["y"],
         Seq
          [Set (Var "x", Const (Sexpr (Number (Fraction (5, 1)))));
           Applic (Var "+", [Var "x"; Var "y"])]);
        Applic (Var "+", [Var "x"; Var "y"; Var "z"])]),
    
    LambdaSimple' (["x"; "y"; "z"],
     Seq'
      [Set' ((VarParam ("x", 0)), Box' (VarParam ("x", 0)));
       LambdaSimple' (["y"],
          Seq'
           [BoxSet' (VarBound ("x", 0, 0), Const' (Sexpr (Number (Fraction (5, 1)))));
            ApplicTP' (Var' (VarFree "+"),
             [BoxGet' (VarBound ("x", 0, 0)); Var' (VarParam ("y", 0))])]);
         ApplicTP' (Var' (VarFree "+"),
          [BoxGet' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
           Var' (VarParam ("z", 2))])]));
  ("semantic test 11",
    LambdaSimple (["x"], Set (Var "x", Applic (LambdaSimple ([], Var "x"), []))),
    
    LambdaSimple' (["x"],
     Seq'
      [Set' ((VarParam ("x", 0)), Box' (VarParam ("x", 0)));
       BoxSet' (VarParam ("x", 0),
        Applic' (LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0))), []))]));
  ("semantic test 12",
    Applic (Var "y",
      [LambdaSimple (["y"],
        Seq
         [Set (Var "a", LambdaSimple (["b"], Applic (Var "a", [Var "b"])));
          Set (Var "t",
           LambdaSimple (["x"],
            Seq
             [Set (Var "y",
               LambdaSimple (["j"], Applic (Var "x", [Var "j"; Var "x"])));
              Var "h"]));
          Applic (Var "y", [Var "a"])])]),
    
    Applic' (Var' (VarFree "y"),
     [LambdaSimple' (["y"],
       Seq'
        [Set' ((VarParam ("y", 0)), Box' (VarParam ("y", 0)));
         Set' ((VarFree "a"),
            LambdaSimple' (["b"],
             ApplicTP' (Var' (VarFree "a"), [Var' (VarParam ("b", 0))])));
           Set' ((VarFree "t"),
            LambdaSimple' (["x"],
             Seq'
              [BoxSet' (VarBound ("y", 0, 0),
                LambdaSimple' (["j"],
                 ApplicTP' (Var' (VarBound ("x", 0, 0)),
                  [Var' (VarParam ("j", 0)); Var' (VarBound ("x", 0, 0))])));
               Var' (VarFree "h")]));
           ApplicTP' (BoxGet' (VarParam ("y", 0)), [Var' (VarFree "a")])])]));
  ("semantic test 13",
    LambdaSimple (["x"],
      Seq
       [LambdaSimple (["x"], Set (Var "x", Var "x"));
        LambdaSimple (["x"], Set (Var "x", Var "x"))]),
    
    LambdaSimple' (["x"],
     Seq'
      [LambdaSimple' (["x"],
        Set' ((VarParam ("x", 0)), Var' (VarParam ("x", 0))));
       LambdaSimple' (["x"],
        Set' ((VarParam ("x", 0)), Var' (VarParam ("x", 0))))]));
  ("semantic test 14",
    LambdaSimple (["x"; "y"],
      Seq
       [LambdaSimple ([], Set (Var "x", Var "y"));
        LambdaSimple ([], Set (Var "y", Var "x"))]),
    
    LambdaSimple' (["x"; "y"],
     Seq'
      [Set' ((VarParam ("x", 0)), Box' (VarParam ("x", 0)));
       Set' ((VarParam ("y", 1)), Box' (VarParam ("y", 1)));
       LambdaSimple' ([],
          BoxSet' (VarBound ("x", 0, 0), BoxGet' (VarBound ("y", 0, 1))));
         LambdaSimple' ([],
          BoxSet' (VarBound ("y", 0, 1), BoxGet' (VarBound ("x", 0, 0))))]));
  ("semantic test 15",
    LambdaOpt ([], "x",
      Seq
       [LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Number (Fraction (1, 1))))));
        Applic (Var "car", [Var "x"])]),
    
    LambdaOpt' ([], "x",
     Seq'
      [LambdaSimple' (["x"],
        Set' ((VarParam ("x", 0)), Const' (Sexpr (Number (Fraction (1, 1))))));
       ApplicTP' (Var' (VarFree "car"), [Var' (VarParam ("x", 0))])]));
  ("semantic test 16",
    If (Var "x", Applic (Var "x", []), Var "x"),
    
    If' (Var' (VarFree "x"), Applic' (Var' (VarFree "x"), []),
     Var' (VarFree "x")));
  ("semantic test 17",
    LambdaSimple ([],
      If (Var "x", Applic (Var "x", []), Applic (Var "not", [Var "x"]))),
    
    LambdaSimple' ([],
     If' (Var' (VarFree "x"), ApplicTP' (Var' (VarFree "x"), []),
      ApplicTP' (Var' (VarFree "not"), [Var' (VarFree "x")]))));
  ("semantic test 18",
    LambdaSimple (["a"; "b"; "c"; "d"; "e"],
      Applic (Var "a",
       [Applic (Var "b", [Var "c"]); Applic (Var "c", [Var "b"; Var "d"]);
        Applic (Var "a",
         [Applic (Var "b", [Applic (Var "c", [Applic (Var "d", [Var "e"])])])])])),
    
    LambdaSimple' (["a"; "b"; "c"; "d"; "e"],
     ApplicTP' (Var' (VarParam ("a", 0)),
      [Applic' (Var' (VarParam ("b", 1)), [Var' (VarParam ("c", 2))]);
       Applic' (Var' (VarParam ("c", 2)),
        [Var' (VarParam ("b", 1)); Var' (VarParam ("d", 3))]);
       Applic' (Var' (VarParam ("a", 0)),
        [Applic' (Var' (VarParam ("b", 1)),
          [Applic' (Var' (VarParam ("c", 2)),
            [Applic' (Var' (VarParam ("d", 3)), [Var' (VarParam ("e", 4))])])])])])));
  ("semantic test 19",
    LambdaSimple (["x"],
      Seq [Applic (Var "x", []); Set (Var "x", Applic (Var "x", []))]),
    
    LambdaSimple' (["x"],
     Seq'
      [Applic' (Var' (VarParam ("x", 0)), []);
       Set' ((VarParam ("x", 0)), Applic' (Var' (VarParam ("x", 0)), []))]));
  ("semantic test 20",
    LambdaSimple (["x"],
      Applic
       (LambdaSimple (["y"],
         Seq [Set (Var "x", Applic (Var "y", [])); Const (Sexpr (Number (Fraction (2, 1))))]),
       [])),
    
    LambdaSimple' (["x"],
     ApplicTP'
      (LambdaSimple' (["y"],
        Seq'
         [Set' ((VarBound ("x", 0, 0)),
           Applic' (Var' (VarParam ("y", 0)), []));
          Const' (Sexpr (Number (Fraction (2, 1))))]),
      [])));
  ("semantic test 21",
    Const(Void),
     Const' Void);
  ("semantic test 22",
    LambdaSimple (["x"],
      Seq
       [Var "x";
        LambdaSimple (["x"],
         Seq
          [Set (Var "x", Const (Sexpr (Number (Fraction (1, 1)))));
           LambdaSimple ([], Var "x")]);
        LambdaSimple ([], Set (Var "x", Var "x"))]),
    
    LambdaSimple' (["x"],
     Seq'
     [Var' (VarParam ("x", 0));
      LambdaSimple' (["x"],
        Seq'
         [Set' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction (1, 1)))));
          LambdaSimple' ([],
            Var' (VarBound ("x", 0, 0)))]);
          LambdaSimple' ([],
            Set' (VarBound ("x", 0, 0), Var' (VarBound ("x", 0, 0))))]));
  ("semantic test 23",
    LambdaSimple (["x"],
      Seq
       [Var "x";
        LambdaSimple (["x"],
         Seq
          [Set (Var "y", Var "x");
           LambdaSimple ([], Var "x")]);
        LambdaSimple ([], Set (Var "x", Var "x"))]),
    
    LambdaSimple' (["x"],
     Seq'
      [Var' (VarParam ("x", 0));
       LambdaSimple' (["x"],
        Seq'
         [Set' ((VarFree "y"), Var' (VarParam ("x", 0)));
          LambdaSimple' ([],
            Var' (VarBound ("x", 0, 0)))]);
          LambdaSimple' ([],
            Set' (VarBound ("x", 0, 0), Var' (VarBound ("x", 0, 0))))]));
  ("semantic test 24",
    LambdaSimple (["x"; "y"; "z"],
     Applic (Var "f", [Applic (Var "g", [Applic (Var "g", [Var "x"])])])),
    
    LambdaSimple' (["x"; "y"; "z"],
     ApplicTP' (Var' (VarFree "f"),
      [Applic' (Var' (VarFree "g"),
        [Applic' (Var' (VarFree "g"), [Var' (VarParam ("x", 0))])])])));
  ("semantic test 25",
    LambdaSimple (["x"],
     Applic (Var "f",
      [LambdaSimple (["y"], Applic (Var "g", [Var "x"; Var "y"]))])),
    
    LambdaSimple' (["x"],
     ApplicTP' (Var' (VarFree "f"),
      [LambdaSimple' (["y"],
        ApplicTP' (Var' (VarFree "g"),
         [Var' (VarBound ("x", 0, 0)); Var' (VarParam ("y", 0))]))])));
  ("semantic test 26",
    LambdaSimple (["x"; "y"; "z"; "w"],
     If (Applic (Var "even?", [Var "x"]), Applic (Var "y", [Var "w"]),
      Applic (Var "z", [Var "w"]))),
    
    LambdaSimple' (["x"; "y"; "z"; "w"],
     If' (Applic' (Var' (VarFree "even?"), [Var' (VarParam ("x", 0))]),
      ApplicTP' (Var' (VarParam ("y", 1)), [Var' (VarParam ("w", 3))]),
      ApplicTP' (Var' (VarParam ("z", 2)), [Var' (VarParam ("w", 3))]))));
  ("semantic test 27",
    LambdaSimple (["x"; "y"; "z"],
     Applic (Var "f",
      [If (Applic (Var "odd?", [Var "x"]), Applic (Var "h", [Var "y"]),
        Applic (Var "w", [Var "z"]))])),
    
    LambdaSimple' (["x"; "y"; "z"],
     ApplicTP' (Var' (VarFree "f"),
      [If' (Applic' (Var' (VarFree "odd?"), [Var' (VarParam ("x", 0))]),
        Applic' (Var' (VarFree "h"), [Var' (VarParam ("y", 1))]),
        Applic' (Var' (VarFree "w"), [Var' (VarParam ("z", 2))]))])));
  ("semantic test 28",
    LambdaSimple (["a"; "b"],
     Seq
      [Applic (Var "f", [Var "a"]); Applic (Var "g", [Var "a"; Var "b"]);
       Applic (Var "display", [Const (Sexpr (String "done!\n"))])]),
    
    LambdaSimple' (["a"; "b"],
     Seq'
      [Applic' (Var' (VarFree "f"), [Var' (VarParam ("a", 0))]);
       Applic' (Var' (VarFree "g"),
        [Var' (VarParam ("a", 0)); Var' (VarParam ("b", 1))]);
       ApplicTP' (Var' (VarFree "display"), [Const' (Sexpr (String "done!\n"))])]));
  ("semantic test 29",
    LambdaSimple ([],
     If (Applic (Var "f", [Var "x"]),
      If (Applic (Var "g", [Var "y"]), Applic (Var "h", [Var "z"]),
       Const (Sexpr (Bool false))),
      Const (Sexpr (Bool false)))),
    
    LambdaSimple' ([],
     If' (Applic' (Var' (VarFree "f"), [Var' (VarFree "x")]),
      If' (Applic' (Var' (VarFree "g"), [Var' (VarFree "y")]),
       ApplicTP' (Var' (VarFree "h"), [Var' (VarFree "z")]),
       Const' (Sexpr (Bool false))),
      Const' (Sexpr (Bool false)))));
  ("semantic test 30",
    LambdaSimple (["x"; "y"],
     Or [Applic (Var "f", [Applic (Var "g", [Var "x"])]); Var "y"]),
    
    LambdaSimple' (["x"; "y"],
     Or'
      [Applic' (Var' (VarFree "f"),
        [Applic' (Var' (VarFree "g"), [Var' (VarParam ("x", 0))])]);
       Var' (VarParam ("y", 1))]));
  ("semantic test 31",
    LambdaSimple (["x"], Set (Var "x", Applic (Var "f", [Var "y"]))),
    
    LambdaSimple' (["x"],
     Set' ((VarParam ("x", 0)),
      Applic' (Var' (VarFree "f"), [Var' (VarFree "y")]))));
  ("semantic test 32",
    LambdaSimple ([],
     Set (Var "x",
      Applic (Var "f",
       [LambdaSimple (["y"], Applic (Var "g", [Var "x"; Var "y"]))]))),
    
    LambdaSimple' ([],
     Set' ((VarFree "x"),
      Applic' (Var' (VarFree "f"),
       [LambdaSimple' (["y"],
         ApplicTP' (Var' (VarFree "g"),
          [Var' (VarFree "x"); Var' (VarParam ("y", 0))]))]))));
  ("semantic test 33",
    LambdaSimple (["x"; "y"; "z"],
     If (Applic (Var "f?", [Var "x"]), Applic (Var "g", [Var "y"]),
      If (Applic (Var "g?", [Var "x"]),
       Seq [Applic (Var "f", [Var "x"]); Applic (Var "f", [Var "y"])],
       Seq
        [Applic (Var "h", [Var "x"]); Applic (Var "f", [Var "y"]);
         Applic (Var "g", [Applic (Var "f", [Var "x"])])]))),
    
    LambdaSimple' (["x"; "y"; "z"],
     If' (Applic' (Var' (VarFree "f?"), [Var' (VarParam ("x", 0))]),
      ApplicTP' (Var' (VarFree "g"), [Var' (VarParam ("y", 1))]),
      If' (Applic' (Var' (VarFree "g?"), [Var' (VarParam ("x", 0))]),
       Seq'
        [Applic' (Var' (VarFree "f"), [Var' (VarParam ("x", 0))]);
         ApplicTP' (Var' (VarFree "f"), [Var' (VarParam ("y", 1))])],
       Seq'
        [Applic' (Var' (VarFree "h"), [Var' (VarParam ("x", 0))]);
         Applic' (Var' (VarFree "f"), [Var' (VarParam ("y", 1))]);
         ApplicTP' (Var' (VarFree "g"),
          [Applic' (Var' (VarFree "f"), [Var' (VarParam ("x", 0))])])]))));
  ("semantic test 34",
    Applic (LambdaSimple (["x"; "y"], Applic (Var "+", [Var "x"; Var "y"])),
     [Applic (Var "f", [Var "y"]); Applic (Var "g", [Var "x"])]),
    
    Applic'
     (LambdaSimple' (["x"; "y"],
       ApplicTP' (Var' (VarFree "+"),
        [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1))])),
     [Applic' (Var' (VarFree "f"), [Var' (VarFree "y")]);
      Applic' (Var' (VarFree "g"), [Var' (VarFree "x")])]));
  ("semantic test 35",
    LambdaSimple (["x"],
     Applic (Var "list",
      [LambdaSimple ([], Var "x"); LambdaSimple (["y"], Set (Var "x", Var "y"))])),
    
    LambdaSimple' (["x"],
     Seq'
      [Set' ((VarParam ("x", 0)), Box' (VarParam ("x", 0)));
       ApplicTP' (Var' (VarFree "list"),
        [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
         LambdaSimple' (["y"],
          BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("y", 0))))])]));
  ("semantic test 36",
    LambdaSimple (["x"; "y"; "z"], Applic (Var "+", [Var "x"; Var "y"; Var "z"])),
    
    LambdaSimple' (["x"; "y"; "z"],
     ApplicTP' (Var' (VarFree "+"),
      [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
       Var' (VarParam ("z", 2))])));
  ("semantic test 37",
    LambdaSimple (["x"; "y"; "z"],
     Applic (Var "+",
      [Var "x"; Var "y";
       LambdaSimple (["z"],
        Applic (Var "+", [Var "z"; Var "x"; Const (Sexpr (Number (Fraction (1, 1))))]))])),
    
    LambdaSimple' (["x"; "y"; "z"],
     ApplicTP' (Var' (VarFree "+"),
      [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
       LambdaSimple' (["z"],
        ApplicTP' (Var' (VarFree "+"),
         [Var' (VarParam ("z", 0)); Var' (VarBound ("x", 0, 0));
          Const' (Sexpr (Number (Fraction (1, 1))))]))])));
  ("semantic test 38",
    LambdaSimple (["x"; "y"; "z"],
     Applic (Var "+",
      [Var "x"; Var "y";
       LambdaSimple (["z"],
        Applic (Var "+", [Var "z"; Const (Sexpr (Number (Fraction (2, 1))))]))])),
    
    LambdaSimple' (["x"; "y"; "z"],
     ApplicTP' (Var' (VarFree "+"),
      [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
       LambdaSimple' (["z"],
        ApplicTP' (Var' (VarFree "+"),
         [Var' (VarParam ("z", 0)); Const' (Sexpr (Number (Fraction (2, 1))))]))])));
  ("semantic test 39",
    LambdaOpt (["x"; "y"], "z",
     Applic (Var "+",
      [Var "x"; Var "y";
       LambdaSimple (["z"],
        Applic (Var "+",
         [Var "z"; LambdaSimple (["z"], Applic (Var "+", [Var "z"; Var "y"]))]))])),
    
    LambdaOpt' (["x"; "y"], "z",
     ApplicTP' (Var' (VarFree "+"),
      [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
       LambdaSimple' (["z"],
        ApplicTP' (Var' (VarFree "+"),
         [Var' (VarParam ("z", 0));
          LambdaSimple' (["z"],
           ApplicTP' (Var' (VarFree "+"),
            [Var' (VarParam ("z", 0)); Var' (VarBound ("y", 1, 1))]))]))])));
  ("semantic test 40",
    LambdaSimple (["x"; "y"; "z"],
     Applic (Var "+",
      [Var "x"; Var "y";
       LambdaSimple (["z"],
        Applic (Var "+",
         [Var "z";
          LambdaSimple (["x"], Applic (Var "+", [Var "x"; Var "y"; Var "z"]))]))])),
    
    LambdaSimple' (["x"; "y"; "z"],
     ApplicTP' (Var' (VarFree "+"),
      [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
       LambdaSimple' (["z"],
        ApplicTP' (Var' (VarFree "+"),
         [Var' (VarParam ("z", 0));
          LambdaSimple' (["x"],
           ApplicTP' (Var' (VarFree "+"),
            [Var' (VarParam ("x", 0)); Var' (VarBound ("y", 1, 1));
             Var' (VarBound ("z", 0, 0))]))]))])));
  ("semantic test 41",
    LambdaOpt (["x"; "y"], "z",
     Applic (Var "+",
      [Var "x"; Var "y";
       LambdaSimple (["z"],
        Applic (Var "+",
         [Var "z"; LambdaSimple ([], Applic (Var "+", [Var "z"; Var "y"]))]))])),
    
    LambdaOpt' (["x"; "y"], "z",
     ApplicTP' (Var' (VarFree "+"),
      [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
       LambdaSimple' (["z"],
        ApplicTP' (Var' (VarFree "+"),
         [Var' (VarParam ("z", 0));
          LambdaSimple' ([],
           ApplicTP' (Var' (VarFree "+"),
            [Var' (VarBound ("z", 0, 0)); Var' (VarBound ("y", 1, 1))]))]))])));
  ("semantic test 42",
    LambdaSimple (["x"; "y"; "z"],
     Applic (Var "+",
      [Var "x"; Var "y";
       LambdaSimple (["z"],
        Applic (Var "+",
         [Var "x"; Var "y"; Var "z";
          LambdaSimple ([], Applic (Var "+", [Var "x"; Var "y"; Var "z"]))]))])),
    
    LambdaSimple' (["x"; "y"; "z"],
     ApplicTP' (Var' (VarFree "+"),
      [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
       LambdaSimple' (["z"],
        ApplicTP' (Var' (VarFree "+"),
         [Var' (VarBound ("x", 0, 0)); Var' (VarBound ("y", 0, 1));
          Var' (VarParam ("z", 0));
          LambdaSimple' ([],
           ApplicTP' (Var' (VarFree "+"),
            [Var' (VarBound ("x", 1, 0)); Var' (VarBound ("y", 1, 1));
             Var' (VarBound ("z", 0, 0))]))]))])));
  ("semantic test 43",
    Def (Var "foo3",
     LambdaOpt (["x"], "y",
      Seq
       [LambdaSimple ([], Var "x"); LambdaSimple ([], Var "y");
        LambdaSimple ([], Set (Var "x", Var "y"))])),
    
    Def' ((VarFree "foo3"),
     LambdaOpt' (["x"], "y",
      Seq'
       [Set' ((VarParam ("x", 0)), Box' (VarParam ("x", 0)));
        LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
          LambdaSimple' ([], Var' (VarBound ("y", 0, 1)));
          LambdaSimple' ([],
           BoxSet' (VarBound ("x", 0, 0), Var' (VarBound ("y", 0, 1))))])));
  ("semantic test 44",
    Def (Var "test",
     LambdaSimple (["x"],
      Applic (Var "list",
       [LambdaSimple ([], Var "x"); LambdaSimple (["y"], Set (Var "x", Var "y"))]))),
    
    Def' ((VarFree "test"),
     LambdaSimple' (["x"],
      Seq'
       [Set' ((VarParam ("x", 0)), Box' (VarParam ("x", 0)));
        ApplicTP' (Var' (VarFree "list"),
         [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
          LambdaSimple' (["y"],
           BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("y", 0))))])])));
  ("semantic test 45",
    Def (Var "test",
     LambdaSimple (["x"; "y"],
      Set (Var "x", Applic (Var "*", [Var "x"; Var "y"])))),
    
    Def' ((VarFree "test"),
     LambdaSimple' (["x"; "y"],
      Set' ((VarParam ("x", 0)),
       Applic' (Var' (VarFree "*"),
        [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1))])))));
  ("semantic test 46",
    Def (Var "test",
     LambdaSimple (["x"; "y"],
      If (Var "x", LambdaSimple ([], Set (Var "y", Var "x")),
       LambdaSimple (["z"], Set (Var "x", Var "z"))))),
    
    Def' ((VarFree "test"),
     LambdaSimple' (["x"; "y"],
      Seq'
       [Set' ((VarParam ("x", 0)), Box' (VarParam ("x", 0)));
        If' (BoxGet' (VarParam ("x", 0)),
         LambdaSimple' ([],
          Set' ((VarBound ("y", 0, 1)), BoxGet' (VarBound ("x", 0, 0)))),
         LambdaSimple' (["z"],
          BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0)))))])));
  ("semantic test 47",
    Def (Var "test",
     LambdaSimple (["x"; "y"],
      Applic (Var "list",
       [LambdaSimple ([],
         Set (Var "x",
          Applic (Var "+", [Var "x"; Const (Sexpr (Number (Fraction (1, 1))))])));
        LambdaSimple ([], Var "y")]))),
    
    Def' ((VarFree "test"),
     LambdaSimple' (["x"; "y"],
      ApplicTP' (Var' (VarFree "list"),
       [LambdaSimple' ([],
         Set' ((VarBound ("x", 0, 0)),
          Applic' (Var' (VarFree "+"),
           [Var' (VarBound ("x", 0, 0)); Const' (Sexpr (Number (Fraction (1, 1))))])));
        LambdaSimple' ([], Var' (VarBound ("y", 0, 1)))]))));
  ("semantic test 48",
    Def (Var "test",
     LambdaSimple (["x"],
      LambdaSimple (["op"],
       If (Applic (Var "eq?", [Var "op"; Const (Sexpr (Symbol "read"))]),
        LambdaSimple ([], Var "x"),
        If (Applic (Var "eq?", [Var "op"; Const (Sexpr (Symbol "write"))]),
         LambdaSimple (["val"], Set (Var "x", Var "val")), Const Void))))),
    
    Def' ((VarFree "test"),
     LambdaSimple' (["x"],
      LambdaSimple' (["op"],
       If'
        (Applic' (Var' (VarFree "eq?"),
          [Var' (VarParam ("op", 0)); Const' (Sexpr (Symbol "read"))]),
        LambdaSimple' ([], Var' (VarBound ("x", 1, 0))),
        If'
         (Applic' (Var' (VarFree "eq?"),
           [Var' (VarParam ("op", 0)); Const' (Sexpr (Symbol "write"))]),
         LambdaSimple' (["val"],
          Set' ((VarBound ("x", 1, 0)), Var' (VarParam ("val", 0)))),
         Const' Void))))));
  ("semantic test 49",
    Def (Var "test",
     LambdaSimple (["x"],
      Applic
       (LambdaSimple (["y"],
         Applic (Var "cons",
          [LambdaSimple ([], Var "x");
           Applic (Var "cons", [Set (Var "x", Var "y"); Const (Sexpr Nil)])])),
       [Const (Sexpr (Number (Fraction (1, 1))))]))),
    
    Def' ((VarFree "test"),
     LambdaSimple' (["x"],
      ApplicTP'
       (LambdaSimple' (["y"],
         ApplicTP' (Var' (VarFree "cons"),
          [LambdaSimple' ([], Var' (VarBound ("x", 1, 0)));
           Applic' (Var' (VarFree "cons"),
            [Set' ((VarBound ("x", 0, 0)), Var' (VarParam ("y", 0)));
             Const' (Sexpr Nil)])])),
       [Const' (Sexpr (Number (Fraction (1, 1))))]))));
  ("semantic test 50",
    Def (Var "test",
     LambdaOpt (["x"], "y",
      Applic (Var "cons", [Var "x"; LambdaSimple ([], Set (Var "x", Var "y"))]))),
    
    Def' ((VarFree "test"),
     LambdaOpt' (["x"], "y",
      Seq'
       [Set' ((VarParam ("x", 0)), Box' (VarParam ("x", 0)));
        ApplicTP' (Var' (VarFree "cons"),
         [BoxGet' (VarParam ("x", 0));
          LambdaSimple' ([],
           BoxSet' (VarBound ("x", 0, 0), Var' (VarBound ("y", 0, 1))))])])));
  ("semantic test 51",
    Def (Var "test",
     LambdaSimple (["x"; "y"; "z"],
      Applic (Var "list",
       [LambdaSimple ([],
         Applic (Var "list",
          [LambdaSimple (["x"], Set (Var "x", Var "z"));
           LambdaSimple ([], Set (Var "x", Var "z")); Var "x"]));
        LambdaSimple (["y"], Set (Var "x", Var "y"))]))),
    
    Def' ((VarFree "test"),
     LambdaSimple' (["x"; "y"; "z"],
      Seq'
       [Set' ((VarParam ("x", 0)), Box' (VarParam ("x", 0)));
        ApplicTP' (Var' (VarFree "list"),
         [LambdaSimple' ([],
           ApplicTP' (Var' (VarFree "list"),
            [LambdaSimple' (["x"],
              Set' ((VarParam ("x", 0)), Var' (VarBound ("z", 1, 2))));
             LambdaSimple' ([],
              BoxSet' (VarBound ("x", 1, 0), Var' (VarBound ("z", 1, 2))));
             BoxGet' (VarBound ("x", 0, 0))]));
          LambdaSimple' (["y"],
           BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("y", 0))))])])));
  ("semantic test 52",
    LambdaSimple (["x"; "y"],
     Applic (Var "list",
      [LambdaSimple ([], Var "x"); LambdaSimple ([], Var "y");
       LambdaSimple (["z"], Set (Var "x", Var "z"))])),
    
    LambdaSimple' (["x"; "y"],
     Seq'
      [Set' ((VarParam ("x", 0)), Box' (VarParam ("x", 0)));
       ApplicTP' (Var' (VarFree "list"),
        [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
         LambdaSimple' ([], Var' (VarBound ("y", 0, 1)));
         LambdaSimple' (["z"],
          BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0))))])]));
  ("semantic test 53",
    LambdaSimple (["x"; "y"],
     Applic (Var "list",
      [LambdaSimple ([], Var "x"); LambdaSimple (["z"], Set (Var "y", Var "z"));
       LambdaSimple (["z"], Set (Var "x", Var "z"))])),
    
    LambdaSimple' (["x"; "y"],
     Seq'
      [Set' ((VarParam ("x", 0)), Box' (VarParam ("x", 0)));
       ApplicTP' (Var' (VarFree "list"),
        [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
         LambdaSimple' (["z"],
          Set' ((VarBound ("y", 0, 1)), Var' (VarParam ("z", 0))));
         LambdaSimple' (["z"],
          BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0))))])]));
  ("semantic test 54",
    LambdaSimple (["x"; "y"],
     Applic (Var "list",
      [LambdaSimple ([], Var "x"); LambdaSimple ([], Var "y");
       LambdaSimple (["z"], Set (Var "y", Var "z"));
       LambdaSimple (["z"], Set (Var "x", Var "z"))])),
    
    LambdaSimple' (["x"; "y"],
     Seq'
      [Set' ((VarParam ("x", 0)), Box' (VarParam ("x", 0)));
       Set' ((VarParam ("y", 1)), Box' (VarParam ("y", 1)));
       ApplicTP' (Var' (VarFree "list"),
        [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
         LambdaSimple' ([], BoxGet' (VarBound ("y", 0, 1)));
         LambdaSimple' (["z"],
          BoxSet' (VarBound ("y", 0, 1), Var' (VarParam ("z", 0))));
         LambdaSimple' (["z"],
          BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0))))])]));
  ("semantic test 55",
    Def (Var "func",
     LambdaOpt ([], "x",
      Applic (Var "list",
       [LambdaSimple ([], Var "x"); LambdaSimple (["z"], Set (Var "x", Var "z"));
        LambdaSimple (["z"], Set (Var "x", Var "z"))]))),
    
    Def' ((VarFree "func"),
     LambdaOpt' ([], "x",
      Seq'
       [Set' ((VarParam ("x", 0)), Box' (VarParam ("x", 0)));
        ApplicTP' (Var' (VarFree "list"),
         [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
          LambdaSimple' (["z"],
           BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0))));
          LambdaSimple' (["z"],
           BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0))))])])));
  ("semantic test 56",
    Def (Var "func",
     LambdaOpt ([], "x",
      LambdaSimple (["samerib"],
       Applic (Var "list",
        [LambdaSimple ([], Var "x");
         LambdaSimple (["z"], Set (Var "x", Var "z"))])))),
    
    Def' ((VarFree "func"),
     LambdaOpt' ([], "x",
      LambdaSimple' (["samerib"],
       ApplicTP' (Var' (VarFree "list"),
        [LambdaSimple' ([], Var' (VarBound ("x", 1, 0)));
         LambdaSimple' (["z"],
          Set' ((VarBound ("x", 1, 0)), Var' (VarParam ("z", 0))))])))));
  ("semantic test 57",
    Def (Var "func",
     LambdaSimple (["x"; "y"; "z"; "w"],
      Applic (Var "list",
       [LambdaSimple ([], Var "x"); LambdaSimple ([], Var "y");
        LambdaSimple ([], Var "z"); LambdaSimple ([], Var "w");
        LambdaSimple ([], Set (Var "x", Const (Sexpr (Number (Fraction (0, 1))))));
        LambdaSimple ([], Set (Var "y", Const (Sexpr (Number (Fraction (1, 1))))));
        LambdaSimple ([], Set (Var "z", Const (Sexpr (Number (Fraction (2, 1))))));
        LambdaSimple ([], Set (Var "w", Const (Sexpr (Number (Fraction (3, 1))))))]))),
    
    Def' ((VarFree "func"),
     LambdaSimple' (["x"; "y"; "z"; "w"],
      Seq'
       [Set' ((VarParam ("x", 0)), Box' (VarParam ("x", 0)));
        Set' ((VarParam ("y", 1)), Box' (VarParam ("y", 1)));
        Set' ((VarParam ("z", 2)), Box' (VarParam ("z", 2)));
        Set' ((VarParam ("w", 3)), Box' (VarParam ("w", 3)));
        ApplicTP' (Var' (VarFree "list"),
         [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
          LambdaSimple' ([], BoxGet' (VarBound ("y", 0, 1)));
          LambdaSimple' ([], BoxGet' (VarBound ("z", 0, 2)));
          LambdaSimple' ([], BoxGet' (VarBound ("w", 0, 3)));
          LambdaSimple' ([],
           BoxSet' (VarBound ("x", 0, 0), Const' (Sexpr (Number (Fraction (0, 1))))));
          LambdaSimple' ([],
           BoxSet' (VarBound ("y", 0, 1), Const' (Sexpr (Number (Fraction (1, 1))))));
          LambdaSimple' ([],
           BoxSet' (VarBound ("z", 0, 2), Const' (Sexpr (Number (Fraction (2, 1))))));
          LambdaSimple' ([],
           BoxSet' (VarBound ("w", 0, 3), Const' (Sexpr (Number (Fraction (3, 1))))))])])));
  ("semantic test 58",
    Def (Var "x",
     Applic (Var "+",
      [Const (Sexpr (Number (Fraction (1, 1)))); Const (Sexpr (Number (Fraction (2, 1))))])),
    
    Def' ((VarFree "x"),
     Applic' (Var' (VarFree "+"),
      [Const' (Sexpr (Number (Fraction (1, 1)))); Const' (Sexpr (Number (Fraction (2, 1))))])));
  ("semantic test 59",
    LambdaOpt (["x"], "y", Var "x"),
     LambdaOpt' (["x"], "y", Var' (VarParam ("x", 0))));
  ("semantic test 60",
    Applic
     (LambdaSimple (["x"],
       Applic (LambdaSimple (["y"], Applic (Var "+", [Var "x"; Var "y"])),
        [Applic (Var "g", [Var "x"])])),
     [Applic (Var "f", [Var "y"])]),
    
    Applic'
     (LambdaSimple' (["x"],
       ApplicTP'
        (LambdaSimple' (["y"],
          ApplicTP' (Var' (VarFree "+"),
           [Var' (VarBound ("x", 0, 0)); Var' (VarParam ("y", 0))])),
        [Applic' (Var' (VarFree "g"), [Var' (VarParam ("x", 0))])])),
     [Applic' (Var' (VarFree "f"), [Var' (VarFree "y")])]));
  ("semantic test 61",
    LambdaSimple (["x"; "y"; "z"; "w"],
      If (Applic (Var "foo?", [Var "x"]), Applic (Var "goo", [Var "y"]),
       Applic (Var "boo", [Applic (Var "doo", [Var "z"])]))),
    
    LambdaSimple' (["x"; "y"; "z"; "w"],
     If' (Applic' (Var' (VarFree "foo?"), [Var' (VarParam ("x", 0))]),
      ApplicTP' (Var' (VarFree "goo"), [Var' (VarParam ("y", 1))]),
      ApplicTP' (Var' (VarFree "boo"),
       [Applic' (Var' (VarFree "doo"), [Var' (VarParam ("z", 2))])]))));
  ("semantic test 62",
    LambdaSimple (["x"; "y"; "z"],
      Applic (Var "f",
       [If (Applic (Var "g?", [Var "x"]), Applic (Var "h", [Var "y"]),
         Applic (Var "w", [Var "z"]))])),
    
    LambdaSimple' (["x"; "y"; "z"],
     ApplicTP' (Var' (VarFree "f"),
      [If' (Applic' (Var' (VarFree "g?"), [Var' (VarParam ("x", 0))]),
        Applic' (Var' (VarFree "h"), [Var' (VarParam ("y", 1))]),
        Applic' (Var' (VarFree "w"), [Var' (VarParam ("z", 2))]))])));
  ("semantic test 63",
    LambdaSimple ([],
      If (Applic (Var "f", [Var "x"]),
       If (Applic (Var "g", [Var "y"]), Applic (Var "h", [Var "z"]),
        Const (Sexpr (Bool false))),
       Const (Sexpr (Bool false)))),
    
    LambdaSimple' ([],
     If' (Applic' (Var' (VarFree "f"), [Var' (VarFree "x")]),
      If' (Applic' (Var' (VarFree "g"), [Var' (VarFree "y")]),
       ApplicTP' (Var' (VarFree "h"), [Var' (VarFree "z")]),
       Const' (Sexpr (Bool false))),
      Const' (Sexpr (Bool false)))));
  ("semantic test 64",
    LambdaSimple (["x"; "y"; "z"; "a"; "b"],
      Applic (Var "f",
       [If (Applic (Var "g?", [Var "x"]),
         If (Applic (Var ">", [Const (Sexpr (Number (Fraction (1, 1)))); Var "a"]),
          Applic (Var "h", [Var "y"]), Var "b"),
         Applic (Var "w", [Var "z"]))])),
    
    LambdaSimple' (["x"; "y"; "z"; "a"; "b"],
     ApplicTP' (Var' (VarFree "f"),
      [If' (Applic' (Var' (VarFree "g?"), [Var' (VarParam ("x", 0))]),
        If'
         (Applic' (Var' (VarFree ">"),
           [Const' (Sexpr (Number (Fraction (1, 1)))); Var' (VarParam ("a", 3))]),
         Applic' (Var' (VarFree "h"), [Var' (VarParam ("y", 1))]),
         Var' (VarParam ("b", 4))),
        Applic' (Var' (VarFree "w"), [Var' (VarParam ("z", 2))]))])));
  ("semantic test 65",
    LambdaSimple (["x"; "y"; "z"; "a"; "b"],
      If (Applic (Var "g?", [Var "x"]),
       If (Applic (Var ">", [Const (Sexpr (Number (Fraction (1, 1)))); Var "a"]),
        Applic (Var "h", [Var "y"]), Applic (Var "w", [Var "b"])),
       If (Applic (Var "<", [Const (Sexpr (Number (Fraction (1, 1)))); Var "a"]),
        Applic (Var "w", [Var "b"]), Applic (Var "w", [Var "z"])))),
    
    LambdaSimple' (["x"; "y"; "z"; "a"; "b"],
     If' (Applic' (Var' (VarFree "g?"), [Var' (VarParam ("x", 0))]),
      If'
       (Applic' (Var' (VarFree ">"),
         [Const' (Sexpr (Number (Fraction (1, 1)))); Var' (VarParam ("a", 3))]),
       ApplicTP' (Var' (VarFree "h"), [Var' (VarParam ("y", 1))]),
       ApplicTP' (Var' (VarFree "w"), [Var' (VarParam ("b", 4))])),
      If'
       (Applic' (Var' (VarFree "<"),
         [Const' (Sexpr (Number (Fraction (1, 1)))); Var' (VarParam ("a", 3))]),
       ApplicTP' (Var' (VarFree "w"), [Var' (VarParam ("b", 4))]),
       ApplicTP' (Var' (VarFree "w"), [Var' (VarParam ("z", 2))])))));
  ("semantic test 66",
    LambdaSimple (["x"; "y"; "z"; "a"; "b"],
      If (Or [Applic (Var "f", [Applic (Var "g", [Var "x"])]); Var "y"],
       If (Applic (Var "f", [Var "x"]),
        If (Applic (Var "g", [Var "y"]), Applic (Var "h", [Var "z"]),
         Const (Sexpr (Bool false))),
        Const (Sexpr (Bool false))),
       Applic (Var "h", [Var "z"]))),
    
    LambdaSimple' (["x"; "y"; "z"; "a"; "b"],
     If'
      (Or'
        [Applic' (Var' (VarFree "f"),
          [Applic' (Var' (VarFree "g"), [Var' (VarParam ("x", 0))])]);
         Var' (VarParam ("y", 1))],
      If' (Applic' (Var' (VarFree "f"), [Var' (VarParam ("x", 0))]),
       If' (Applic' (Var' (VarFree "g"), [Var' (VarParam ("y", 1))]),
        ApplicTP' (Var' (VarFree "h"), [Var' (VarParam ("z", 2))]),
        Const' (Sexpr (Bool false))),
       Const' (Sexpr (Bool false))),
      ApplicTP' (Var' (VarFree "h"), [Var' (VarParam ("z", 2))]))));
  ("semantic test 67",
    LambdaSimple (["s"], Applic (Var "apply", [Var "f"; Var "s"])),
    
    LambdaSimple' (["s"],
     ApplicTP' (Var' (VarFree "apply"),
      [Var' (VarFree "f"); Var' (VarParam ("s", 0))])));
  ("semantic test 68",
    Applic
      (LambdaSimple (["a"; "b"; "c"; "e"],
        If (Applic (Var "eq?", [Var "a"; Const (Sexpr (Number (Fraction (5, 1))))]),
         Applic (Var "e", [Var "a"; Var "b"]),
         Applic (Var "c", [Var "a"; Var "b"]))),
      [Const (Sexpr (Number (Fraction (5, 1)))); Const (Sexpr (Number (Fraction (7, 1)))); Var "d";
       Var "f"]),
    
    Applic'
     (LambdaSimple' (["a"; "b"; "c"; "e"],
       If'
        (Applic' (Var' (VarFree "eq?"),
          [Var' (VarParam ("a", 0)); Const' (Sexpr (Number (Fraction (5, 1))))]),
        ApplicTP' (Var' (VarParam ("e", 3)),
         [Var' (VarParam ("a", 0)); Var' (VarParam ("b", 1))]),
        ApplicTP' (Var' (VarParam ("c", 2)),
         [Var' (VarParam ("a", 0)); Var' (VarParam ("b", 1))]))),
     [Const' (Sexpr (Number (Fraction (5, 1)))); Const' (Sexpr (Number (Fraction (7, 1))));
      Var' (VarFree "d"); Var' (VarFree "f")]));
  ("semantic test 69",
    Applic
      (LambdaSimple (["a"],
        Applic
         (LambdaSimple (["b"],
           Applic
            (LambdaSimple (["c"],
              Applic
               (LambdaSimple (["e"],
                 If
                  (Applic (Var "eq?", [Var "a"; Const (Sexpr (Number (Fraction (5, 1))))]),
                  Applic (Var "e", [Var "a"; Var "b"]),
                  Applic (Var "c", [Var "a"; Var "b"]))),
               [Var "f"])),
            [Var "d"])),
         [Const (Sexpr (Number (Fraction (7, 1))))])),
      [Const (Sexpr (Number (Fraction (5, 1))))]),
    
    Applic'
     (LambdaSimple' (["a"],
       ApplicTP'
        (LambdaSimple' (["b"],
          ApplicTP'
           (LambdaSimple' (["c"],
             ApplicTP'
              (LambdaSimple' (["e"],
                If'
                 (Applic' (Var' (VarFree "eq?"),
                   [Var' (VarBound ("a", 2, 0)); Const' (Sexpr (Number (Fraction (5, 1))))]),
                 ApplicTP' (Var' (VarParam ("e", 0)),
                  [Var' (VarBound ("a", 2, 0)); Var' (VarBound ("b", 1, 0))]),
                 ApplicTP' (Var' (VarBound ("c", 0, 0)),
                  [Var' (VarBound ("a", 2, 0)); Var' (VarBound ("b", 1, 0))]))),
              [Var' (VarFree "f")])),
           [Var' (VarFree "d")])),
        [Const' (Sexpr (Number (Fraction (7, 1))))])),
     [Const' (Sexpr (Number (Fraction (5, 1))))]));
  ("semantic test 70",
    Def (Var "while",
      LambdaSimple (["test"; "body"],
       If (Applic (Var "test", []),
        Seq
         [Applic (Var "body", []);
          Applic (Var "while", [Var "test"; Var "body"])],
        Const Void))),
    
    Def' ((VarFree "while"),
     LambdaSimple' (["test"; "body"],
      If' (Applic' (Var' (VarParam ("test", 0)), []),
       Seq'
        [Applic' (Var' (VarParam ("body", 1)), []);
         ApplicTP' (Var' (VarFree "while"),
          [Var' (VarParam ("test", 0)); Var' (VarParam ("body", 1))])],
       Const' Void))));
  ("semantic test 71",
    Applic
      (LambdaSimple (["a"],
        Applic
         (LambdaSimple (["c"],
           Applic
            (LambdaSimple (["e"],
              If (Applic (Var "x", [Var ">"; Const (Sexpr (Number (Fraction (5, 1))))]),
               LambdaSimple (["x"], Applic (Var "a", [Var "x"])),
               LambdaSimple (["x"], Applic (Var "c", [Var "x"])))),
            [Var "f"])),
         [Var "d"])),
      [Var "b"]),
    
    Applic'
     (LambdaSimple' (["a"],
       ApplicTP'
        (LambdaSimple' (["c"],
          ApplicTP'
           (LambdaSimple' (["e"],
             If'
              (Applic' (Var' (VarFree "x"),
                [Var' (VarFree ">"); Const' (Sexpr (Number (Fraction (5, 1))))]),
              LambdaSimple' (["x"],
               ApplicTP' (Var' (VarBound ("a", 2, 0)),
                [Var' (VarParam ("x", 0))])),
              LambdaSimple' (["x"],
               ApplicTP' (Var' (VarBound ("c", 1, 0)),
                [Var' (VarParam ("x", 0))])))),
           [Var' (VarFree "f")])),
        [Var' (VarFree "d")])),
     [Var' (VarFree "b")]));
  ("semantic test 72",
    LambdaSimple (["x"],
      LambdaOpt (["x"], "y",
       If (Applic (Var "x", [Var ">"; Const (Sexpr (Number (Fraction (5, 1))))]),
        LambdaSimple (["x"], Applic (Var "a", [Var "x"])),
        LambdaSimple (["x"], Applic (Var "c", [Var "x"]))))),
    
    LambdaSimple' (["x"],
     LambdaOpt' (["x"], "y",
      If'
       (Applic' (Var' (VarParam ("x", 0)),
         [Var' (VarFree ">"); Const' (Sexpr (Number (Fraction (5, 1))))]),
       LambdaSimple' (["x"],
        ApplicTP' (Var' (VarFree "a"), [Var' (VarParam ("x", 0))])),
       LambdaSimple' (["x"],
        ApplicTP' (Var' (VarFree "c"), [Var' (VarParam ("x", 0))]))))));
  ("semantic test 73",
    LambdaSimple (["x"],
      LambdaOpt (["a"], "y",
       If (Applic (Var "x", [Var ">"; Const (Sexpr (Number (Fraction (5, 1))))]),
        LambdaSimple (["x"], Applic (Var "a", [Var "x"])),
        LambdaSimple (["x"], Applic (Var "c", [Var "x"]))))),
    
    LambdaSimple' (["x"],
     LambdaOpt' (["a"], "y",
      If'
       (Applic' (Var' (VarBound ("x", 0, 0)),
         [Var' (VarFree ">"); Const' (Sexpr (Number (Fraction (5, 1))))]),
       LambdaSimple' (["x"],
        ApplicTP' (Var' (VarBound ("a", 0, 0)), [Var' (VarParam ("x", 0))])),
       LambdaSimple' (["x"],
        ApplicTP' (Var' (VarFree "c"), [Var' (VarParam ("x", 0))]))))));
  ("semantic test 74",
    LambdaSimple (["a"],
      Seq
       [LambdaSimple ([],
         LambdaSimple (["x"; "y"; "z"],
          Or [Applic (Var "x", [Var "y"]); Applic (Var "a", [Var "z"])]));
        LambdaSimple (["x"], Applic (Var "a", [Var "x"]))]),
    
    LambdaSimple' (["a"],
     Seq'
      [LambdaSimple' ([],
        LambdaSimple' (["x"; "y"; "z"],
         Or'
          [Applic' (Var' (VarParam ("x", 0)), [Var' (VarParam ("y", 1))]);
           ApplicTP' (Var' (VarBound ("a", 1, 0)), [Var' (VarParam ("z", 2))])]));
       LambdaSimple' (["x"],
        ApplicTP' (Var' (VarBound ("a", 0, 0)), [Var' (VarParam ("x", 0))]))]));
  ("semantic test 75",
    Seq
      [LambdaSimple (["x"; "y"],
        LambdaSimple (["y"; "z"], Applic (Var "x", [Var "y"; Var "z"])));
       LambdaSimple (["z"], Applic (Var "x", [Var "y"; Var "z"]))],
    
    Seq'
     [LambdaSimple' (["x"; "y"],
       LambdaSimple' (["y"; "z"],
        ApplicTP' (Var' (VarBound ("x", 0, 0)),
         [Var' (VarParam ("y", 0)); Var' (VarParam ("z", 1))])));
      LambdaSimple' (["z"],
       ApplicTP' (Var' (VarFree "x"),
        [Var' (VarFree "y"); Var' (VarParam ("z", 0))]))]);
  ("semantic test 76",
    LambdaSimple (["x"; "y"; "z"],
      Applic
       (If (Applic (Var "x", [Var "y"; Var "z"]),
         Seq
          [LambdaSimple (["x"; "y"],
            LambdaSimple (["y"; "z"], Applic (Var "x", [Var "y"; Var "z"])));
           LambdaSimple (["z"], Applic (Var "x", [Var "y"; Var "z"]))],
         Const Void),
       [LambdaSimple ([], Applic (Var "x", [Var "y"; Var "z"]))])),
    
    LambdaSimple' (["x"; "y"; "z"],
     ApplicTP'
      (If'
        (Applic' (Var' (VarParam ("x", 0)),
          [Var' (VarParam ("y", 1)); Var' (VarParam ("z", 2))]),
        Seq'
         [LambdaSimple' (["x"; "y"],
           LambdaSimple' (["y"; "z"],
            ApplicTP' (Var' (VarBound ("x", 0, 0)),
             [Var' (VarParam ("y", 0)); Var' (VarParam ("z", 1))])));
          LambdaSimple' (["z"],
           ApplicTP' (Var' (VarBound ("x", 0, 0)),
            [Var' (VarBound ("y", 0, 1)); Var' (VarParam ("z", 0))]))],
        Const' Void),
      [LambdaSimple' ([],
        ApplicTP' (Var' (VarBound ("x", 0, 0)),
         [Var' (VarBound ("y", 0, 1)); Var' (VarBound ("z", 0, 2))]))])));
  ("semantic test 77",
    LambdaSimple (["x"],
      Applic (Var "x",
       [LambdaSimple (["y"; "z"],
         Applic (Var "x",
          [Var "y"; Var "z";
           LambdaSimple ([],
            Applic (Var "x",
             [Var "y"; Var "z";
              LambdaSimple (["z"], Applic (Var "x", [Var "y"; Var "z"]))]))]))])),
    
    LambdaSimple' (["x"],
     ApplicTP' (Var' (VarParam ("x", 0)),
      [LambdaSimple' (["y"; "z"],
        ApplicTP' (Var' (VarBound ("x", 0, 0)),
         [Var' (VarParam ("y", 0)); Var' (VarParam ("z", 1));
          LambdaSimple' ([],
           ApplicTP' (Var' (VarBound ("x", 1, 0)),
            [Var' (VarBound ("y", 0, 0)); Var' (VarBound ("z", 0, 1));
             LambdaSimple' (["z"],
              ApplicTP' (Var' (VarBound ("x", 2, 0)),
               [Var' (VarBound ("y", 1, 0)); Var' (VarParam ("z", 0))]))]))]))])));
  ("semantic test 78",
    LambdaSimple (["x"],
      Applic (Var "x",
       [LambdaSimple (["y"; "z"],
         Applic (Var "x",
          [Var "y"; Var "z";
           LambdaSimple ([],
            Applic (Var "x",
             [Var "y"; Var "z";
              LambdaSimple (["z"], Applic (Var "x", [Var "y"; Var "z"]))]))]));
        LambdaSimple (["x"],
         LambdaSimple (["y"; "z"], Applic (Var "x", [Var "y"; Var "z"])))])),
    
    LambdaSimple' (["x"],
     ApplicTP' (Var' (VarParam ("x", 0)),
      [LambdaSimple' (["y"; "z"],
        ApplicTP' (Var' (VarBound ("x", 0, 0)),
         [Var' (VarParam ("y", 0)); Var' (VarParam ("z", 1));
          LambdaSimple' ([],
           ApplicTP' (Var' (VarBound ("x", 1, 0)),
            [Var' (VarBound ("y", 0, 0)); Var' (VarBound ("z", 0, 1));
             LambdaSimple' (["z"],
              ApplicTP' (Var' (VarBound ("x", 2, 0)),
               [Var' (VarBound ("y", 1, 0)); Var' (VarParam ("z", 0))]))]))]));
       LambdaSimple' (["x"],
        LambdaSimple' (["y"; "z"],
         ApplicTP' (Var' (VarBound ("x", 0, 0)),
          [Var' (VarParam ("y", 0)); Var' (VarParam ("z", 1))])))])));
  ("semantic test 79",
    LambdaSimple (["x"; "y"; "z"],
      Seq
       [Applic (Var "x",
         [LambdaSimple (["x"],
           LambdaSimple (["y"],
            LambdaSimple (["z"], Applic (Var "x", [Var "y"; Var "z"]))))]);
        LambdaSimple ([], Applic (Var "z", []))]),
    
    LambdaSimple' (["x"; "y"; "z"],
     Seq'
      [Applic' (Var' (VarParam ("x", 0)),
        [LambdaSimple' (["x"],
          LambdaSimple' (["y"],
           LambdaSimple' (["z"],
            ApplicTP' (Var' (VarBound ("x", 1, 0)),
             [Var' (VarBound ("y", 0, 0)); Var' (VarParam ("z", 0))]))))]);
       LambdaSimple' ([], ApplicTP' (Var' (VarBound ("z", 0, 2)), []))]));
  ("semantic test 80",
    LambdaSimple (["x"],
      Applic (Var "eq?",
       [LambdaSimple (["x"], Applic (Var "x", []));
        LambdaSimple ([], LambdaSimple (["x"], Applic (Var "x", [])))])),
    
    LambdaSimple' (["x"],
     ApplicTP' (Var' (VarFree "eq?"),
      [LambdaSimple' (["x"], ApplicTP' (Var' (VarParam ("x", 0)), []));
       LambdaSimple' ([],
        LambdaSimple' (["x"], ApplicTP' (Var' (VarParam ("x", 0)), [])))])));
  ("semantic test 81",
    LambdaSimple (["x"],
      LambdaSimple ([],
       Seq
        [Applic (LambdaSimple (["x"], Applic (Var "x", [])), [Var "x"]);
         LambdaSimple ([], LambdaSimple ([], Applic (Var "x", [])))])),
    
    LambdaSimple' (["x"],
     LambdaSimple' ([],
      Seq'
       [Applic' (LambdaSimple' (["x"], ApplicTP' (Var' (VarParam ("x", 0)), [])),
         [Var' (VarBound ("x", 0, 0))]);
        LambdaSimple' ([],
         LambdaSimple' ([], ApplicTP' (Var' (VarBound ("x", 2, 0)), [])))])));
  ("semantic test 82",
    LambdaSimple (["x"; "y"; "z"; "w"],
      Applic (Var "w",
       [LambdaSimple (["x"; "y"; "z"],
         Seq
          [Applic (Var "z",
            [LambdaSimple (["x"; "y"],
              Applic (Var "y", [LambdaSimple (["x"], Applic (Var "x", []))]));
             Var "x"]);
           Var "x"]);
        Var "x"; Var "y"; Var "z"])),
    
    LambdaSimple' (["x"; "y"; "z"; "w"],
     ApplicTP' (Var' (VarParam ("w", 3)),
      [LambdaSimple' (["x"; "y"; "z"],
        Seq'
         [Applic' (Var' (VarParam ("z", 2)),
           [LambdaSimple' (["x"; "y"],
             ApplicTP' (Var' (VarParam ("y", 1)),
              [LambdaSimple' (["x"], ApplicTP' (Var' (VarParam ("x", 0)), []))]));
            Var' (VarParam ("x", 0))]);
          Var' (VarParam ("x", 0))]);
       Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
       Var' (VarParam ("z", 2))])));
];;
