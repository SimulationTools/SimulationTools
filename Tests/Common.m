
(* MUnit 1.4, distributed with Mathematica 10, renamed
   EquivalenceFunction as SameTest *)
If[MUnit`Information`$VersionNumber >= 1.4,
  MUnit`EquivalenceFunction = SameTest];

withinRoundoff[a_, b_] := 2 Abs[(a-b)/(a+b)] < 10^-12;
