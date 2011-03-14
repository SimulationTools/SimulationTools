(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["Profile`"];

Profile;
ProfileTime;
ProfileCount;
ClearProfile;
Timer;

Begin["`Private`"];

ClearProfile[] :=
  Module[{},
    Clear[ProfileTime];
    Clear[ProfileCount]];

SetAttributes[Profile, HoldAll];

Profile[name_, code_] :=
  Module[{time, result, name2, result2, subTimers},
    name2 = Evaluate[name];
    {result2,subTimers} = Reap[{time, result} = AbsoluteTiming[ReleaseHold[code]]];
    If[Head[ProfileTime[name2]] === ProfileTime, ProfileTime[name2] = 0.0];
    If[Head[ProfileCount[name2]] === ProfileCount, ProfileCount[name2] = 0];
    ProfileTime[name2] += time;
    ProfileCount[name2] += 1;
    Sow[Timer[name2,time,If[subTimers === {}, {}, subTimers[[1]]]]];
    result];

(* The code below is a prototype for handling a tree of timer
   information from Profile.  It needs to be tidied up and a good
   interface defined.  Example of use:

     {result, timers} = Reap@MyProfile["Total", <code>];

     treeView@addUntimed@
       Timer[timers[[1, 1, 1]], timers[[1, 1, 2]],
         addTimers[timers[[1, 1, 3]], sameq, plus]]
*)

treeView[t : Timer[n_, v_, c_]] :=
 Module[{},
  If[c === {},
   Row[{"  ", v, " ", n}],
   OpenerView[{Row[{v, " ", n}],
     Column[treeView /@ c]}, True]]]

(* Take a list and, for all the members which are "sameq" as each \
other, replace them with the application of "plus" to them *)
add[l_, sameq_, plus_] :=
 Map[plus, Gather[l, sameq]];

addTimers[ts_List, sameq_, plus_] :=
 add[Map[Timer[#[[1]], #[[2]], addTimers[#[[3]], sameq, plus]] &, ts],
   sameq, plus];

sameq[Timer[n1_, v1_, s1_], Timer[n2_, v2_, s2_]] :=
 n1 === n2 && Length[s1] == Length[s2] &&
  And @@ MapThread[sameq, {s1, s2}];

plus[ts_List] :=
 Timer[ts[[1, 1]], Plus @@ Map[#[[2]] &, ts],
  MapThread[plus[{##}] &, Map[#[[3]] &, ts]]];

addUntimed[Timer[n_, v_, s_List]] :=
 If[Length[s] === 0, Timer[n, v, s],
  Module[{total},
   total = Plus @@ Map[#[[2]] &, s];
   Timer[n, v,
    Append[Map[addUntimed, s], Timer["untimed", v - total, {}]]]]];

End[];

EndPackage[];
