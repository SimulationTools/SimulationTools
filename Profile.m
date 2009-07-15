
BeginPackage["Profile`"];

Profile;
ProfileTime;

Begin["`Private`"];

SetAttributes[Profile, HoldAll];

Profile[name_, code_] :=
  Module[{time, result, name2},
    name2 = Evaluate[name];
    {time, result} = AbsoluteTiming[ReleaseHold[code]];
    If[Head[ProfileTime[name2]] === ProfileTime, ProfileTime[name2] = 0.0];
    ProfileTime[name2] += time;
    result];

End[];

EndPackage[];
