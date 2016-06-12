
(* MUnit 1.4, distributed with Mathematica 10, renamed
   EquivalenceFunction as SameTest and introduced a new
   function VerificationTest in place of Test.
*)
If[$VersionNumber < 10,
  SetAttributes[VerificationTest, HoldAll];
  VerificationTest[test_, result_, opts___] :=
    Test[test, result, opts /. SameTest -> EquivalenceFunction]
];

withinRoundoff[a_, b_] :=
  (* Ignore the last N machine digits *)
  Block[{Internal`$SameQTolerance = 4},
    SameQ[a,b]];
