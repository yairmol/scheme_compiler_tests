#use "topfind";;
#use "tag_parser_utils.ml";;

let sequnce_test_suite = [
  ("test_empty_sequence", Tag_Parser.tag_parse_expressions [Pair (Symbol "begin", Nil)], [Const Void]);
  ("test_single_expr_sequence", Tag_Parser.tag_parse_expressions [Pair (Symbol "begin", Pair (Number (Float 1.2), Nil))], [Const (Sexpr (Number (Float 1.2)))]);
  ("test_single_expr_sequence_2", Tag_Parser.tag_parse_expressions [Pair (Symbol "begin", Pair (Pair (Symbol "if", Pair (Symbol "?", Pair (Symbol "!", Nil))), Nil))], [If (Var "?", Var "!", Const Void)]);
  (let expr1 = Number (Float 1.234) in 
  let expr2 = Char ('c') in 
  let expr3 = String ("on the fly") in
    ("test_regular_sequence", 
      Tag_Parser.tag_parse_expressions [
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
    Tag_Parser.tag_parse_expressions [
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
      Tag_Parser.tag_parse_expressions [
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
    Tag_Parser.tag_parse_expressions [
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
    Tag_Parser.tag_parse_expressions [
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
