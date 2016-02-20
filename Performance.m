(* Copyright 2010-2012 Ian Hinder and Barry Wardell

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

BeginPackage["SimulationTools`Performance`",
 {
  "SimulationTools`RunFiles`"
 }];

ReadTimerTree;
TimerTreeView;
RemoveSmallTimers;
timer;
TimerTreeString;
ReadTimerTrees;
CollectedTimers;
CombinedTimerTrees;
ReadSimulationEvolutionTime;
TimerSubtree;
ReadSimulationTimer;

Begin["`Private`"];

(* Timer exists in ProfileCall as well.  Probably with the same format.
   Should merge these. *)
Timer = timer;

ReadTimerTrees[run_] :=
  Module[
    {files,procs},
    files = FindSimulationFiles[run,"timertree.*.xml"];
    procs = (StringCases[#,"timertree."~~n__~~".xml":>ToExpression[n]][[1]]) & /@ files;
    Table[ReadTimerTree[run,p],{p,procs}]];

ReadTimerTree[run_, n_: 0] :=
  Import[FindFirstRunFile[run, 
     "timertree." <> ToString[n] <> ".xml"]] //. {XMLElement["timer", 
      attrs_, {t_, children___}] :> 
     (Timer["name" /. attrs, Internal`StringToDouble[t], {children}]),
    XMLElement["value", 
      attrs_, _] :> 
     Sequence[],
    XMLObject[_][_, d_, _] :> d};

ReadSimulationTimer[run_String, timerPath_List, n:_Integer:0] :=
 subtree[ReadTimerTree[run, n], timerPath][[2]]

(* TimerTreeView[t : Timer[n_, v_, c_], all_] := *)
(*  Module[{node}, *)
(*   If[c === {}, *)
(*    Row[{"  ", N[100 v/all]," ", v, " ", n}], *)
(*    OpenerView[{Row[{N[100 v/all],"% ",v, " ", n}], *)
(*      Column[TimerTreeView[#,all] & /@ c]}, True]]]; *)

(* TimerTreeView[t : Timer[n_, v_, c_], all_, depth_:0] := *)
(*   Prepend[Flatten[Map[TimerTreeView[#,all,depth+1] &, c],1], *)
(*           {NumberForm[N[100 v/all],{10,1}],NumberForm[v,{10,2}],StringJoin@ConstantArray["| ",depth]<>ToString@n}]; *)
   

Options[TimerTreeView] = {"Threshold" -> 0.05, "Digits" -> 2, "Multiplier" -> 1};

TimerTreeView[run_String, proc_Integer:0, opts___] :=
  TimerTreeView[ReadTimerTree[run,proc],opts];

TimerTreeView[t : Timer[n_, v_, c_], opts:OptionsPattern[]] :=
  Grid[{(Text@Style[#,Bold]&/@{"Percentage", "Time/s", "Timer"}) ~Join~ ConstantArray[SpanFromLeft,10]}~Join~
       timerTreeViewList[RemoveSmallTimers[addUntimed[t],OptionValue[Threshold]],v], 
       Alignment -> {{".", ".", Sequence @@ ConstantArray[Left, 100]}}];

timerTreeViewList[t : Timer[n_, v_, c_], all_, depth_:0] :=
  Prepend[Flatten[Map[timerTreeViewList[#,all,depth+1] &, c],1],
          {Text@NumberForm[N[100 v/all],{10,1}],Text@NumberForm[v,{10,2}],
           Sequence@@ConstantArray[
             Graphics[{Black,Line[{{0,-1},{0,1}}]},ImageSize->{10,10}],
             depth],
           (* Graphics[{Disk[{0,0}],Line[{{0,0},{0,-100}}]},ImageSize->{10,Full}], *)
           (* Graphics[{Line[{{0,0},{0,-100}}]},ImageSize->{10,Full}], *)
           (* Item["",Frame->All], *)
           (* Graphics[{Rectangle[{-1,-0.25},{1,0.25}]},ImageSize->{10,10}], *)
           (* Graphics[{Gray,Line[{{-1,0},{1,0}}]},ImageSize->{10,10}], *)
           Text@n,
           Sequence@@ConstantArray[SpanFromLeft,10]}];
   

(* timerTreeStruct[t : Timer[n_, v_, c_], all_, depth_:0] := *)
(*   Prepend[Flatten[Map[TimerTreeView[#,all,depth+1] &, c],1], *)
(*           {Text@NumberForm[N[100 v/all],{10,1}],Text@NumberForm[v,{10,2}], *)
(*            Sequence@@ConstantArray[SpanFromAbove,depth], *)
(*            (\* Graphics[{Disk[{0,0}],Line[{{0,0},{0,-100}}]},ImageSize->{10,Full}], *\) *)
(*            Graphics[{Line[{{0,0},{0,-100}}]},ImageSize->{10,Full}], *)
(*            (\* "", *\) *)
(*            Text@n, *)
(*            Sequence@@ConstantArray[SpanFromLeft,10]}]; *)
   


(* TimerTreeView[t : Timer[n_, v_, c_], all_, depth_:0] := *)

(*   Prepend[Flatten[Map[TimerTreeView[#,all,depth+1] &, c],1], *)
(*           {Text@NumberForm[N[100 v/all],{10,1}],Text@NumberForm[v,{10,2}], *)
(*            Sequence@@ConstantArray[SpanFromAbove,depth], *)
(*            (\* Graphics[{Disk[{0,0}],Line[{{0,0},{0,-100}}]},ImageSize->{10,Full}], *\) *)
(*            Graphics[{Line[{{0,0},{0,-100}}]},ImageSize->{10,Full}], *)
(*            (\* "", *\) *)
(*            Text@n, *)
(*            Sequence@@ConstantArray[SpanFromLeft,10]}]; *)

  (* If[c === {}, *)
  (*  OpenerView[{Row[{N[100 v/all],"% ",v, " ", n}], *)
  (*    Column[TimerTreeView[#,all] & /@ c]}, True]]]; *)


TimerSubtree = subtree;

subtree[t : Timer[n_, v_, c_], elems_List] :=
 If[Length[elems] === 0, t,
  Module[{pos},
   pos = Position[c, Timer[First[elems], __], {1}];
   If[Length[pos] === 0,
    Throw["Timer " <> First[elems] <> " not found in " <> n]];
   subtree[c[[pos[[1, 1]]]], Drop[elems, 1]]]];

timerValue[Timer[n_, t_, c_]] := t

timerValue[t : Timer[_, _, _], elems_List] := timerValue[subtree[t, elems]];

filterTimes[Timer[name_, val_, children_], thresh_] :=
 Module[{isOK, f},
  isOK[Timer[n_, v_, ch_]] :=
   v > thresh;
  f[t_] := filterTimes[t, thresh];
  Timer[name, val, f /@ Select[children, isOK]]];

addUntimed[tim : Timer[n_, t_, c_]] :=
  Module[{cTot, rest},
   If[Length[c] === 0, Return[tim]];
   cTot = Plus @@ Map[timerValue, c];
   rest = t - cTot;
   Timer[n, t, Append[addUntimed /@ c, Timer["Untimed", rest, {}]]]];

prune[t : Timer[n_, v_, c_], lev_] :=
 Timer[n, v, If[lev == 0, {}, Map[prune[#, lev - 1] &, c]]];

norm[Timer[n_, v_, c_], tot_] :=
 Timer[n, 100 v/tot, Map[norm[#, tot] &, c]];

norm[t : Timer[n_, v_, c_]] :=
 norm[t, v]

add[timers_, name_] :=
 
 Timer[name, Plus @@ timers[[All, 2]], 
  Map[add[#, #[[1, 1]]] &, Transpose[timers[[All, 3]]]]];

CollectedTimers[t : Timer[n_, v_, c_], pattern_] :=
 Module[{posns, collected, timerPattern, rest, collectedRest},
  timerPattern = Timer[nn_?(StringMatchQ[#, pattern] &), __];
  posns = Position[c, timerPattern, {1}];
  collected = Extract[c, posns];
  rest = Delete[c, posns];
  collectedRest = Map[CollectedTimers[#, pattern] &, rest];
  Timer[n, v, 
   If[collected === {}, collectedRest, 
    Prepend[collectedRest, add[collected, ToString[pattern]]]]]];

string[Timer[n_, v_, c_], indent_: ""] :=
 StringJoin[
  Module[{val = ToString[NumberForm[v, {10, 5}]]}, 
    StringJoin[ConstantArray[" ", 10 - StringLength[val]]] <> val] <> " " <> 
   indent <> n <> "\n" <> StringJoin[Map[string[#, indent <> "| "] &, c]]];

Options[TimerTreeString] = Options[TimerTreeView];

TimerTreeString[run_String, proc_, opts:OptionsPattern[]] :=
  TimerTreeString[ReadTimerTree[run,proc],opts];

TimerTreeString[t : Timer[n_, v_, c_], opts:OptionsPattern[]] :=
  timerTreeString[RemoveSmallTimers[addUntimed[t],OptionValue[Threshold]],0,TimerValue[t],OptionValue[Digits],OptionValue[Multiplier]];

timerTreeString[t:Timer[n_, v1_, c_], depth_, total_, digits_, multiplier_] :=
 Module[
   {percent,indent,nf,v, divs, values},
   nf[x_]:= If[x<10^(-(digits+Ceiling@Log10[multiplier]+1)),NumberForm[0,{1000,digits}],NumberForm[x,{1000,digits}]];

   v = multiplier*If[ListQ[v1],v1,{v1}];
   percent = ToString[nf[100TimerValue[t]/total]];
   indent = StringJoin[ConstantArray[" ",depth]];
   rJust[s_,i_] :=
     StringJoin[ConstantArray[" ", Max[i - StringLength[s],0]]] <> s;

   divs = ConstantArray["| ",depth];

   values = StringJoin[Riffle[Map[rJust[ToString[nf[#]], StringLength@ToString[nf[total]]] &, v]," "]];

   rJust[percent,4+digits]<>" "<>values<>" "<>" "<>divs<>n<>"\n"<>
   StringJoin[Map[timerTreeString[#, depth+1,total,digits,multiplier] &, c]]];

sort[Timer[n_, v_, c_]] :=
 Timer[n, v, Map[sort, Sort[c, #1[[2]] > #2[[2]] &]]];

TimerValue[Timer[n_,v_?NumberQ,c_]] := v;
TimerValue[Timer[n_,v_List,c_]] := v[[3]];

compareTimes[v1_?NumberQ, v2_?NumberQ] := v1<v2;
compareTimes[v1_List, v2_List] := v1[[3]]<v2[[3]];

RemoveSmallTimers[Timer[n_,v_,c_], threshold_?NumberQ] :=
  Module[
    {c2, times, accTimes, nSmall},
    (* Sort into increasing order *)
    If[ListQ[v],
       c2 = Sort[c, (#1[[2,3]] < #2[[2,3]]) &];
       times = Map[#[[2,3]] &, c2],
       (* else *)
       c2 = Sort[c, (#1[[2]] < #2[[2]]) &];
       times = Map[#[[2]] &, c2]];
    accTimes = Accumulate[times]/If[ListQ[v],v[[3]],v];
    (* Count how many timers until they add up to the threshold *)
    nSmall = LengthWhile[accTimes, (# < threshold) &];
    Timer[n,v,Map[RemoveSmallTimers[#,threshold] &, Drop[Reverse[c2],-nSmall]]]];

ReadCommunicationSpeed[run_, proc_] :=
 
 Module[{files, file, fullFile, data, pos, data2, commTime, sentBytes,
    receivedBytes},
  files = 
   FindSimulationFiles[run, "carpetlib-timing-statistics.*.txt"];
  file = Select[
     files, (StringCases[#, 
          "carpetlib-timing-statistics." ~~ n__ ~~ ".txt" :> 
           ToExpression[n]][[1]] === proc) &][[1]];
  fullFile = FindSimulationFiles[run, file][[1]];
  data = Import[fullFile, "Table"];
  pos = Last[
     Position[
      data, {"CarpetLib", "timing", "information", "at", 
       "iteration", _, "time", _}]][[1]];
  data2 = data[[pos ;; All]];
  
  commTime = 
   Cases[data2, {"commstate::step:", __, "time:", "sum:", t_, __} :> 
      t][[1]];
  sentBytes = 
   Cases[data2, {"commit_send_space::isend:", __, "bytes:", "sum:", 
       s_, __} :> s][[1]];
  receivedBytes = 
   Cases[data2, {"commstate::sizes_irecv:", __, "bytes:", "sum:", 
       s_, __} :> s][[1]];
  
  Max[sentBytes, receivedBytes]/commTime];

CombinedTimerTrees[timers_List, f_: Automatic] :=
 Module[{names, vals, childrens, fn},
  names = Map[First, timers];
  vals = Map[#[[2]] &, timers];
  childrens = Map[#[[3]] &, timers];
  If[Unequal @@ names, 
   Throw["Attempt to combine timers with different names"]];
  fn = If[f === Automatic, Function[vs, {Mean[vs], Min[vs], Max[vs]}],
     f];
  timer[names[[1]], fn[vals], 
   CombinedTimerTrees[#, f] & /@ Transpose[childrens]]];

ReadSimulationEvolutionTime[run_] := 
 ToExpression[
  Cases[Import[FindSimulationFiles[run, "timertree.0.xml"][[1]]],
    XMLElement["timer", {"name" -> "Evolve"}, {value_, ___}] :> value,
     Infinity][[1]]];


End[];

EndPackage[];
