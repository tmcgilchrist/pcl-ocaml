(* XXX sepByArr, sepEndByArr (harder chainr1,chainl1) *)

module M : Test.Tests =
struct

  type result = TestOK | TestFail of string
  type test = string * (unit -> result) (* label, test *)

  open CharParse.CharPrim
  open CharParse.M
  open CharParse.CharComb
  open Error.M

  module U = Test.Util (CharParse.CharPrim)
  open U

  let check p i r = 
    (fun () -> check TestOK (fun s -> TestFail s) p (LazyList.M.ofString i) r)


  let description = "Combinator"

  let tests =
    [
      ("app", check (app (fun x -> Some x) (return "foo")) "" 
        (Succeed (Some "foo")));

      ("choice 1", check (choice [char 'a'; char 'b'; char 'c']) "abc"
        (Succeed 'a'));

      ("choice 2", check (choice [char 'a'; char 'b'; char 'c']) "bca"
        (Succeed 'b'));

      ("coice 3", check (choice [char 'a'; char 'b'; char 'c']) "cab"
        (Succeed 'c'));

      ("choice 4", check (choice [string "abc"; string "foo"; string "abd"])
        "abd" (Fail "\"test\" (line 1, column 0):\nunexpected \"d\"\nexpecting \"abc\"\n")
      );

      ("option 1", check (option "" (string "abc")) "abcd" (Succeed "abc"));

      ("option 2", check (option "" (string "abc")) "beef" (Succeed ""));

      ("option 3", check (option "" (string "abd")) "abc"
        (Fail "\"test\" (line 1, column 0):\nunexpected \"c\"\nexpecting \"abd\"\n")
      );

      ("optional 1", check (optional (string "op") >> char '+') "op+"
        (Succeed '+'));

      ("optional 2", check (optional (string "op") >> char '+') "+"
        (Succeed '+'));

      ("optional 3", check (optional (string "op") >> char '+') "op"
        (Fail "\"test\" (line 1, column 2):\nunexpected end of input\nexpecting \"+\"\n")
      );

      ("optionRet 1", check (    optionRet (string "op")
                             >>= fun o -> app (fun c -> (o,c)) (char '+')) 
        "op+" (Succeed (Some "op",'+')));

      ("optionRet 2", check (    optionRet (string "op")
                             >>= fun o -> app (fun c -> (o,c)) (char '+'))
        "+" (Succeed (None,'+')));

      ("optionRet 3", check (    optionRet (string "op")
                             >>= fun o -> app (fun c -> (o,c)) (char '+')) 
        "op" (Fail "\"test\" (line 1, column 2):\nunexpected end of input\nexpecting \"+\"\n")
      );     
      
      ("between 1", check (between (string "foo") (char ',') (many alphaNum))
        "fooe6e6e6e6," (Succeed (explode "e6e6e6e6")));

      ("between 2", check (between (string "foo") (char ',') (many alphaNum))
        "fooe6e6e6e6foo"
        (Fail "\"test\" (line 1, column 14):\nunexpected end of input\nexpecting \",\", or letter or digit\n")
      );

      ("skipMany1 1", check (skipMany1 space) "  \t\t " (Succeed ()));

      ("skipMany1 2", check (skipMany1 space) "ab  "
        (Fail "\"test\" (line 1, column 0):\nunexpected \"a\"\nexpecting space\n")
      );

      ("many1 1", check (many1 alphaNum) "elephant6;;"
        (Succeed (explode "elephant6")));

      ("many1 2", check (many1 alphaNum) ";elephant6"
        (Fail "\"test\" (line 1, column 0):\nunexpected \";\"\nexpecting letter or digit\n")
      );

      ("sepBy1 1", check (sepBy1 (app implode (many alphaNum))
                                 (char ','))
        "foo,bar,foo" (Succeed ["foo";"bar";"foo"]));

      ("sepBy1 2", check (sepBy1 (many1 alphaNum) (char ','))
        "" (Fail "\"test\" (line 1, column 0):\nunexpected end of input\nexpecting letter or digit\n")
      );

      ("sepBy 1", check (sepBy (app implode (many1 alphaNum))
                                 (char ','))
        "" (Succeed []));

      ("sepBy 2", check (sepBy (app implode (many alphaNum))
                                 (char ','))
        "foo,bar,foo" (Succeed ["foo";"bar";"foo"]));

      ("sepEndBy1 1", check (sepEndBy1 (app implode (many1 alphaNum))
                                       (char ','))
        "foo,bar,foo," (Succeed ["foo";"bar";"foo"]));

      ("sepEndBy1 2", check (sepEndBy1 (app implode (many alphaNum))
                                       (char ','))
        "foo,bar,foo"  (Succeed ["foo";"bar";"foo"]));
     
      ("sepEndBy1 3", check (sepEndBy1 (app implode (many1 alphaNum))
                                       (char ','))
        ""  (Fail "\"test\" (line 1, column 0):\nunexpected end of input\nexpecting letter or digit\n")
      );

      ("sepEndBy 1", check (sepEndBy (app implode (many1 alphaNum))
                                       (char ','))
        "foo,bar,foo," (Succeed ["foo";"bar";"foo"]));

      ("sepEndBy 2", check (sepEndBy (app implode (many alphaNum))
                                       (char ','))
        "foo,bar,foo"  (Succeed ["foo";"bar";"foo"]));
     
      ("sepEndBy 3", check (sepEndBy (app implode (many1 alphaNum))
                                       (char ','))
        ""  (Succeed []));

      ("endBy1 1", check (endBy1 (many1 alphaNum 
                                  >>= fun l -> return (implode l))
                             (char ','))
        "foo,bar,foo," (Succeed ["foo";"bar";"foo"]));

      ("endBy1 2", check (endBy1 (many alphaNum 
                                         >>= fun l -> return (implode l))
                                       (char ','))
        "foo,bar,foo"
        (Fail "\"test\" (line 1, column 11):\nunexpected end of input\nexpecting \",\", or letter or digit\n")
      );
     
      ("endBy1 3", check (endBy1 (many1 alphaNum 
                                         >>= fun l -> return (implode l))
                                       (char ','))
        "" (Fail "\"test\" (line 1, column 0):\nunexpected end of input\nexpecting letter or digit\n")
      );

      ("endBy 1", check (endBy (many1 alphaNum 
                                  >>= fun l -> return (implode l))
                             (char ','))
        "foo,bar,foo," (Succeed ["foo";"bar";"foo"]));

      ("endBy 2", check (endBy (many alphaNum 
                                  >>= fun l -> return (implode l))
                            (char ','))
        "foo,bar,foo"
        (Fail "\"test\" (line 1, column 11):\nunexpected end of input\nexpecting \",\", or letter or digit\n")
      );
     
      ("endBy 3", check (endBy (many1 alphaNum 
                                         >>= fun l -> return (implode l))
                                       (char ','))
        "" (Succeed []));

      ("count 1", check (count 3 (string "foo")) "foofoofoofoo"
        (Succeed ["foo";"foo";"foo"]));

      ("count 2", check (count 3 (string "foo")) "foofoo"
        (Fail "\"test\" (line 1, column 6):\nunexpected end of input\nexpecting \"foo\"\n")
      );

      ("count 3", check (count 0 (string "foo")) "afdsafda"
        (Succeed []));

      ("foldl 1", check (    many1 alphaNum 
                         >>= fun l -> 
                              foldl (    char '^' 
                                     >>  many1 alphaNum 
                                     >>= fun l -> 
                                          return (fun r -> r ^ (implode l)))
                                (implode l))
        "abc^def^ghi^jkl" (Succeed "abcdefghijkl"));

      ("foldl 2", check (    many1 alphaNum 
                         >>= fun l -> 
                              foldl (    char '^' 
                                     >>  many1 alphaNum 
                                     >>= fun l -> 
                                          return (fun r -> r ^ (implode l)))
                                (implode l))
        "abc^def^ghi^" 
        (Fail "\"test\" (line 1, column 12):\nunexpected end of input\nexpecting letter or digit\n")
      );

      ("chainr1 1", 
       check (chainr1 (app implode (many1 alphaNum))
                      (char '^' >> return (^))) "a^b^c"
         (Succeed "abc"));

      ("chainr1 2", 
       check (chainr1 (app implode (many1 alphaNum))
                      (char '^' >> return (^))) "a^b^c^"
         (Fail "\"test\" (line 1, column 6):\nunexpected end of input\nexpecting letter or digit\n")
      );

      ("chainl1 1", 
       check (chainl1 (app implode (many1 alphaNum))
                      (char '^' >> return (^))) "a^b^c"
         (Succeed "abc"));

      ("chainl1 2", 
       check (chainl1 (app implode (many1 alphaNum))
                      (char '^' >> return (^))) "a^b^c^"
         (Fail "\"test\" (line 1, column 6):\nunexpected end of input\nexpecting letter or digit\n")
      );

      ("chainr 1", 
       check (chainr (app implode (many1 alphaNum))
                      (char '^' >> return (^)) "foo") "a^b^c"
         (Succeed "abc"));

      ("chainr 2", 
       check (chainr (app implode (many1 alphaNum))
                      (char '^' >> return (^)) "foo") "a^b^c^"
         (Fail "\"test\" (line 1, column 6):\nunexpected end of input\nexpecting letter or digit\n")
      );

      ("chainr 3", 
       check (chainr (app implode (many1 alphaNum))
                      (char '^' >> return (^)) "foo") ""
         (Succeed "foo")
      );

      ("chainl 1", 
       check (chainl (app implode (many1 alphaNum))
                      (char '^' >> return (^)) "foo") "a^b^c"
         (Succeed "abc"));

      ("chainl 2", 
       check (chainl (app implode (many1 alphaNum))
                      (char '^' >> return (^)) "foo") "a^b^c^"
         (Fail "\"test\" (line 1, column 6):\nunexpected end of input\nexpecting letter or digit\n")
      );

      ("chainl 3", 
       check (chainl (app implode (many1 alphaNum))
                      (char '^' >> return (^)) "foo") ""
         (Succeed "foo")
      );

     ("anyToken", check (app implode (many anyToken)) "abcdef1;" 
       (Succeed "abcdef1;"));

     ("notFollowedBy 1", check (    app implode (many alphaNum) 
                                >>= fun s -> notFollowedBy (char '.')
                                >>  return s)
       "abcd" (Succeed "abcd"));
     
     ("notFollowedBy 2", check (    app implode (many alphaNum) 
                                >>= fun s -> notFollowedBy (char '.')
                                >>  return s)
       "abcd." 
       (Fail "\"test\" (line 1, column 4):\nunexpected .\nexpecting letter or digit\n")
     );

     ("eof 1", check eof "" (Succeed ()));

     ("eof 2", check (string "foo" >>= fun s -> eof >> return s) "foo"
       (Succeed "foo"));

     ("eof 3", check (string "foo" >>= fun s -> eof >> return s) "foodfdd"
       (Fail "\"test\" (line 1, column 3):\nunexpected d\nexpecting end of input\n")
     );

     ("manyTill 1", check (    (string "(*")
                           >>  manyTill anyToken (attempt (string "*)"))
                           >>= fun l -> return (implode l))
       "(* Hi dude *)" (Succeed " Hi dude "));

     ("manyTill 2", check (    (string "(*")
                           >>  manyTill anyToken (attempt (string "*)"))
                           >>= fun l -> return (implode l))
       "(* Hi dude" 
       (Fail "\"test\" (line 1, column 10):\nunexpected end of input\nexpecting \"*)\"\n")
     );
    ]

end
