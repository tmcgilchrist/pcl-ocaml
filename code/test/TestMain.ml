
module PrimRun = Test.RunTests (PrimTest.M)
module CharRun = Test.RunTests (CharTest.M)
module CombRun = Test.RunTests (CombTest.M)

let _ = List.for_all (fun f -> f (true,false))
[  PrimRun.run_tests;
   CharRun.run_tests;
   CombRun.run_tests;
]

