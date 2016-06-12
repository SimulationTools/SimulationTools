
(* MUnit 1.4, distributed with Mathematica 10, renamed
   EquivalenceFunction as SameTest and introduced a new
   function VerificationTest in place of Test.
*)
If[$VersionNumber < 10,
  SetAttributes[VerificationTest, HoldAllComplete];
  VerificationTest[test_, result_, Shortest[expectedMsgs_:{}], opts___] :=
    Test[test, result, expectedMsgs,opts];
  VerificationTest[test_, result_, Shortest[expectedMsgs_:{}], opts1___, SameTest -> fn_, opts2___] :=
    Test[test, result, expectedMsgs,opts1, EquivalenceFunction -> fn, opts2];
];

withinRoundoff[a_, b_] :=
  (* Ignore the last N machine digits *)
  Block[{Internal`$SameQTolerance = 4},
    SameQ[a,b]];
