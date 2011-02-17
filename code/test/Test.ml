module type Tests =
sig

  type result = TestOK | TestFail of string
  type test = string * (unit -> result) (* label, test *)

  val description : string  (* what is this test module *)
  val tests : test list

end

module Util (P : Prim.S) =
struct
  
  open P

  type 'a rescheck = Succeed of 'a | Fail of string | FailAny


  let check ok fail p i r =
    let result = parse "test" i p in
      match (result,r) with
        (Success a, Succeed b) ->
          if a = b then ok else
            fail "parser succeeded with wrong result"
      | (Success a, FailAny) -> fail "test succeeded, but should have failed"
      | (Success a, Fail s) ->
          fail ("test succeeded, but should have failed with error:\n" ^ s
                ^ "\n")
      | (Failure _, FailAny) -> ok
      | (Failure e, Fail s) ->
          let actualerror = Error.M.errorToString e in
          if actualerror = s then ok
          else fail ("test failed with error\n" ^ actualerror ^ "    but "
                     ^ "should have failed with\n" ^ s )
      | (Failure e, Succeed _) -> 
          fail ("test failed with error:\n" ^ (Error.M.errorToString e) ^
                   "    but should have succeeded\n")

end

module RunTests (T : Tests) : 
  sig 
    exception Stop
    val run_tests : bool*bool -> bool
  end =
struct

  open T

  exception Stop

  (* run_tests verb stop - run the tests in the Tests module,
   * if verb, then print the name of every test.  If stop, then
   * stop at the first error *)
  let run_tests (verb,stop) =
    let test_app (n,f) =
      if verb then
        (print_string ("  running " ^ n ^ ":");
         match f () with
           TestOK -> print_string " Success!\n"
         | TestFail s -> (print_string (" Failure: \n    " ^ s ^ "\n");
                          if stop then raise Stop else ()))
      else 
        match f () with
          TestOK -> ()
        | TestFail s -> 
            (print_string ("  Failed test " ^ n ^ ":\n    " ^ s ^ "\n");
             if stop then raise Stop else ())
    in
      try 
        (print_string ("\nBeginning " ^ description ^ " tests\n");
         List.iter test_app tests;
         true)
      with Stop -> false

end
