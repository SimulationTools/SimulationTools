
(* MUnit 1.4, distributed with Mathematica 10, renamed
   EquivalenceFunction as SameTest *)
If[MUnit`Information`$VersionNumber >= 1.4,
  MUnit`EquivalenceFunction = SameTest];

withinRoundoff[a_, b_] :=
  (* Ignore the last N machine digits *)
  Block[{Internal`$SameQTolerance = 4},
    SameQ[a,b]];
