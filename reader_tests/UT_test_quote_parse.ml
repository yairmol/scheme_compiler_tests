#use "topfind";;

#use "reader.ml";;

let quote_parser_tester_suite = [
  ("simple_quote_test", [Pair ((Symbol "quote"), Pair ((Bool true), Nil))], (Reader.read_sexprs "'#t"));
  ("simple_quasiquote_test", [Pair ((Symbol "quasiquote"), Pair ((Bool true), Nil))], (Reader.read_sexprs "`#t"));
  ("simple_unquote_test", [Pair ((Symbol "unquote"), Pair ((Bool true), Nil))], (Reader.read_sexprs ",#t"));
  ("simple_unquotesplice_test", [Pair ((Symbol "unquote-splicing"), Pair ((Bool true), Nil))], (Reader.read_sexprs ",@#t"));

  ("simple_quote_test_with_spaces", [Pair ((Symbol "quote"), Pair ((Bool true), Nil))], (Reader.read_sexprs "   '  #t  "));
  ("simple_quasiquote_test_with_spaces", [Pair ((Symbol "quasiquote"), Pair ((Bool true), Nil))], (Reader.read_sexprs "  `    #t  "));
  ("simple_unquote_test_with_spaces", [Pair ((Symbol "unquote"), Pair ((Bool true), Nil))], (Reader.read_sexprs "   ,   #t   "));
  ("simple_unquotesplice_test_with_spaces", [Pair ((Symbol "unquote-splicing"), Pair ((Bool true), Nil))], (Reader.read_sexprs "  ,@   #t"));

  ("nested_quotes_1", [
    Pair (Symbol "quasiquote",
        Pair
        (Pair (Bool true,
            Pair
            (Pair (Symbol "unquote",
                Pair
                (Pair (Symbol "+",
                Pair (Number (Fraction (1, 1)),
                    Pair (Number (Fraction (2, 1)),
                    Pair (Number (Fraction (3, 1)), Nil)))),
                Nil)),
            Pair (Bool false, Nil))),
        Nil))
    ], (Reader.read_sexprs "`(#t ,(+ 1 2 3) #f)"));

  ("nested_quotes_2", [
    Pair (Symbol "quasiquote",
        Pair
        (Pair (Symbol "a",
            Pair
            (Pair (Symbol "unquote-splicing",
                Pair
                (Pair (Symbol "append",
                Pair
                    (Pair (Symbol "quote",
                    Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)), Nil)),
                    Pair
                    (Pair (Symbol "quote",
                    Pair (Pair (Symbol "z", Pair (Symbol "w", Nil)), Nil)),
                    Nil))),
                Nil)),
            Pair (Symbol "b", Nil))),
        Nil))
    ], (Reader.read_sexprs "`(a ,@(append '(x y) '(z w)) b)"));
];;
