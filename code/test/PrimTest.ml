(* XXX needs: position, state, manyArr tests *)

module M : Test.Tests =
struct

  type result = TestOK | TestFail of string
  type test = string * (unit -> result) (* label, test *)

  open CharParse.CharPrim
  open Error.M

  module U = Test.Util (CharParse.CharPrim)
  open U

  let check p i r = 
    (fun () -> check TestOK (fun s -> TestFail s) p (LazyList.M.ofString i) r)


  let description = "Prim"

  let tests =
    [("simple return", check (return true) "asdfadsf" (Succeed true));

     ("bind 1", check (return "a" >>= fun s -> return s) "" (Succeed "a"));

     ("bind 2",
      check (return "a" >>= fun s1 -> return "b" >>= fun s2 -> return (s1^s2))
      "" (Succeed "ab"));

     ("mzero", check mzero "" (Fail "unknown parse error"));

     ("bind fail", check (mzero >>= fun _ -> return ()) ""
                         (Fail "unknown parse error"));

     ("seq 1", check (return "a" >> return "b") "" (Succeed "b"));

     ("label 1", check (mzero <?> "foo") ""
                       (Fail "\"test\" (line 1, column 0):\nexpecting foo\n"));

     ("<|> 1", check (mzero <|> return "a") "" (Succeed "a"));

     ("<|> 2", check (return "a" <|> mzero) "" (Succeed "a"));

     ("<|> label", check ( (mzero <?> "foo") <|> (mzero <?> "bar") ) ""
       (Fail "\"test\" (line 1, column 0):\nexpecting bar, or foo\n"));

     ("token 1", check (token (fun x -> Some x)) "abc" (Succeed 'a'));

     ("token 2", check (token (fun x -> if x = 'a' then Some x else None))
                 "abc" (Succeed 'a'));

     ("token 3", check (token (fun x -> if x = 'a' then Some x else None))
       "bc" (Fail "\"test\" (line 1, column 0):\nunexpected \"b\"\n"));

     ("tokens 1", check (tokens ['a';'b';'c']) "abc" (Succeed ['a';'b';'c']));

     ("tokens 2", check (tokens ['a';'b';'c']) "ac" 
       (Fail "\"test\" (line 1, column 0):\nunexpected \"c\"\nexpecting \"abc\"\n")
     );

     ("tokens >>=", check (tokens ['a';'b'] >>= fun ts -> tokens ['c';'d']
                                            >>= fun ts2 -> return (ts@ts2))
       "abcd" (Succeed ['a';'b';'c';'d']));
     
     ("tokens <|> 1", check ((tokens ['a';'b';'c']) <|> (tokens ['b';'c'])) "bc"
       (Succeed ['b';'c']));

     ("tokens <|> 2", check ((tokens ['a';'b';'c']) <|> (tokens ['a';'c'])) "ac"
       (Fail "\"test\" (line 1, column 0):\nunexpected \"c\"\nexpecting \"abc\"\n")
     );

     ("attempt 1", check (    (attempt (tokens ['a';'b'])) 
                          >>= fun ts -> (tokens ['c';'d'])
                          >>= fun ts2 -> return (ts@ts2))
      "abcd" (Succeed ['a';'b';'c';'d']));

     ("attempt 2", check (    (attempt (tokens ['a';'b';'c'])) 
                          <|> (tokens ['a';'c']))
      "ac" (Succeed ['a';'c']));

     ("attempt 3", check (    (attempt (tokens ['a';'b';'c'])) 
                          <|> (tokens ['a';'c'])) "bc" 
       (Fail "\"test\" (line 1, column 0):\nunexpected \"b\"\nexpecting \"ac\", or \"abc\"\n")
     );

     ("attempt 4", check (    (attempt (tokens ['a';'b';'c'])) 
                          <|> (tokens ['a';'c'])) "ad"
       (Fail "\"test\" (line 1, column 0):\nunexpected \"d\"\nexpecting \"ac\"\n")
     );

     ("peek 1", check (    tokens ['a';'b'] 
                       >>= fun ts1 -> peek (tokens ['c'])
                       >>  tokens ['c';'d'] >>= fun ts2 -> return (ts1@ts2))
       "abcd" (Succeed ['a';'b';'c';'d']));

     ("peek 2", check (    tokens ['a';'b'] 
                       >>= fun ts1 -> peek (tokens ['d'])
                       >>  tokens ['c';'d'] >>= fun ts2 -> return (ts1@ts2))
       "abcd" (Fail "\"test\" (line 1, column 2):\nunexpected \"c\"\nexpecting \"d\"\n")
     );

     ("peek 3", check (    tokens ['a';'b'] 
                       >>= fun ts1 -> peek (tokens ['d'])
                       >>  tokens ['c';'d'] >>= fun ts2 -> return (ts1@ts2))
       "abd" (Fail "\"test\" (line 1, column 2):\nunexpected \"d\"\nexpecting \"cd\"\n")
     );

     ("many 1", check (many (token (fun x -> Some x))) "abcd"
       (Succeed ['a';'b';'c';'d']));

     ("many 2", check (many (token (fun x -> if x = 'a' || x = 'b' then Some x
                                             else None))) "abbcd"
       (Succeed ['a';'b';'b']));

     ("many 3", check (many (token (fun x -> if x = 'a' || x = 'b' then Some x
                                             else None))) "cdabb"
       (Succeed []));

     ("many 4", check (many (tokens ['a';'b'])) "abaccd"
       (Fail "\"test\" (line 1, column 2):\nunexpected \"c\"\nexpecting \"ab\"\n")
     );

     ("many 5", check (    (many (attempt (tokens ['a';'b']))) 
                       >>= fun ts -> tokens ['a';'c']
                       >>= fun ts2 -> return (ts,ts2)) "abaccd"
       (Succeed ([['a';'b']],['a';'c'])));

     ("skipmany", check (skipMany (tokens ['a']) >> tokens ['b';'c';'d'])
       "aaaaaabcd" (Succeed ['b';'c';'d']));

    ]

end
