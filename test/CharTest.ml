(* XXX alphanum, letter *)

module M : Test.Tests =
struct

  type result = TestOK | TestFail of string
  type test = string * (unit -> result) (* label, test *)

  open CharParse.CharPrim
  open CharParse.M
  open Error.M

  module U = Test.Util (CharParse.CharPrim)
  open U

  let check p i r = 
    (fun () -> check TestOK (fun s -> TestFail s) p (LazyList.M.ofString i) r)


  let description = "CharParse"

  let tests =
    [("satisfy 1", check (satisfy (fun c -> c < 'p')) "a" (Succeed 'a'));

     ("satisfy 2", check (satisfy (fun c -> c < 'p')) "za"
       (Fail "\"test\" (line 1, column 0):\nunexpected \"z\"\n"));

     ("string 1", check (string "foo") "fool" (Succeed "foo"));
     
     ("string 2", check (string "foo") "nofool"
       (Fail "\"test\" (line 1, column 0):\nunexpected \"n\"\nexpecting \"foo\"\n")
     );

     ("string 3", check (string "foo" <|> string "nof") "nofool"
       (Succeed "nof"));

     ("string 4", check (string "nope" <|> string "nof") "nofool"
       (Fail "\"test\" (line 1, column 0):\nunexpected \"f\"\nexpecting \"nope\"\n")
     );
        
     ("chars 1", check (chars ['f';'o';'o']) "fool" (Succeed ['f';'o';'o']));

     ("chars 2", check (chars ['f';'o';'o']) "nofool" 
       (Fail "\"test\" (line 1, column 0):\nunexpected \"n\"\nexpecting \"foo\"\n")
     );

     ("oneOf 1", check (many (oneOf (explode "abcd")) 
                         >>= fun l -> return (implode l))
       "abcadauuub" (Succeed "abcada"));

     ("oneOf 2", check (oneOf (explode "abcd")) "za"
       (Fail "\"test\" (line 1, column 0):\nunexpected \"z\"\n"));

     ("noneOf 1", check (many (noneOf (explode "xyz")) 
                          >>= fun l -> return (implode l))
       "deadbeefxyzaaa" (Succeed "deadbeef"));

     ("noneOf 2", check (noneOf (explode "zxy")) "xyzzy"
       (Fail "\"test\" (line 1, column 0):\nunexpected \"x\"\n"));

     ("char 1", check (many (char 'a')) "aa" (Succeed ['a';'a']));

     ("char 2", check (char 'a') "za" 
       (Fail "\"test\" (line 1, column 0):\nunexpected \"z\"\nexpecting \"a\"\n")
     );

     ("space 1", check (many space >>= fun l -> return (implode l)) 
       "  \t  \n\n aa  a a a   \n" (Succeed "  \t  \n\n "));

     ("space 2", check space "a b c d"
       (Fail "\"test\" (line 1, column 0):\nunexpected \"a\"\nexpecting space\n")
     );

     ("newline 1", check newline "\na\n" (Succeed '\n'));

     ("newline 2", check newline "ab\ncd" 
       (Fail "\"test\" (line 1, column 0):\nunexpected \"a\"\nexpecting newline\n")
     );

     ("tab 1", check tab "\t" (Succeed '\t'));
     
     ("tab 2", check tab "    "
         (Fail "\"test\" (line 1, column 0):\nunexpected \" \"\nexpecting tab\n")
     );

     ("spaces 1", check spaces "\t  \n \t\t aaa \t\t" (Succeed ()));

     ("spaces 2", check spaces "asddd" (Succeed ()));

     ("upper 1", check (many upper >>= fun l -> return (implode l))
       "ASJKDHHWE3ak" (Succeed "ASJKDHHWE"));

     ("upper 2", check upper "aA"
       (Fail "\"test\" (line 1, column 0):\nunexpected \"a\"\nexpecting uppercase letter\n")
     );

     ("upper 3", check upper ";;"
         (Fail "\"test\" (line 1, column 0):\nunexpected \";\"\nexpecting uppercase letter\n")
     );
     ("lower 1", check (many lower >>= fun l -> return (implode l))
       "asdfadsf3AK" (Succeed "asdfadsf"));

     ("lower 2", check lower "Aa"
       (Fail "\"test\" (line 1, column 0):\nunexpected \"A\"\nexpecting lowercase letter\n")
     );

     ("lower 3", check lower ";;"
       (Fail "\"test\" (line 1, column 0):\nunexpected \";\"\nexpecting lowercase letter\n")
     );
     
     ("digit 1", check ((many digit) >>= fun l -> return (implode l)) "123123"
       (Succeed "123123"));

     ("digit 2", check digit "A"
       (Fail "\"test\" (line 1, column 0):\nunexpected \"A\"\nexpecting digit\n")
     );

     ("hexDigit 1", check ((many hexDigit) >>= fun l -> return (implode l)) 
       "123AbCdEfG" (Succeed "123AbCdEf"));

     ("hexDigit 2", check hexDigit ";"
       (Fail "\"test\" (line 1, column 0):\nunexpected \";\"\nexpecting hexadecimal digit\n")
     );

     ("octDigit 1", check ((many octDigit) >>= fun l -> return (implode l)) 
       "0123456789" (Succeed "01234567"));

     ("octDigit 2", check octDigit ";"
       (Fail "\"test\" (line 1, column 0):\nunexpected \";\"\nexpecting octal digit\n")
     );

     ("anyChar", check ((many anyChar) >>= fun l -> return (implode l)) 
       "asdfASDF1234[]\'" (Succeed "asdfASDF1234[]\'"));

    ]

end
