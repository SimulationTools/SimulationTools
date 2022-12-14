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

BeginPackage["SimulationTools`Timers`",
 {
  "SimulationTools`CactusTimers`",
  "SimulationTools`Error`",
  "SimulationTools`Memo`",
  "SimulationTools`RunFiles`"
 }];

(* Interface with CactusTimers package *)

ReadTimerFromRun(*::usage = "ReadTimerFromRun[sim, timername, proc] returns data for the timer called timername on process proc from the AllTimers output of the TimerReport thorn.  proc is optional and defaults to 0."*);
ChartTimersFromRun;
TimersFile;
IndependentTimersFromRun;
ParseTimersFileFromRun;
ParseTimersFileFromRunCore;
FindTimersFromRun;
ChartEvolutionTimersFromRun;
CollectTimers;
ReadAllTimers;
ReadAllTimerIterations;

CTGammaTimerCollections =
{"CTGamma RHS" -> {"CTGEvolution: CTGEvolution_CalcRHS_detg",
    "CTGGauge: ShiftGammaDriver", "CTGGauge: LapseBonaMasso"},
  "CTGamma SYNC" -> {"CTGEvolution: CTGEvolution_SelectBoundaries",
    "CTGConstraints: CTGConstraints_SelectBoundaries",
    "CTGGauge: CTGGauge_SelectBoundaries_GammaDriver",
    "CTGGauge: CTGGauge_SelectBoundaries_Lapse"} ,
  "CTGamma radiative bc" -> {"CTGEvolution: \
CTGEvolution_ApplyRadiativeBC",
    "CTGGauge: CTGGauge_ApplyRadiativeBC_GammaDriver"}};

Begin["`Private`"];

TimersFile[runName_] :=
 FindSimulationFiles[runName, "AllTimers.000000.txt"][[1]];

padInteger[i_, n_] :=
  Module[{str},
   str = ToString[i];
   StringJoin[Table["0", {a, n - StringLength[str]}]] <> str];

TimersFile[runName_, i_, segment_:1] :=
  Module[{segments, fileName},
    fileName = "AllTimers."<> padInteger[i, 6] <> ".txt";
    segments = FindSimulationFiles[runName, fileName];
    If[Length[segments] === 0, Error["Cannot find timers file " <> fileName <> " in run " <> runName]];
    If[segment > Length[segments], Error["Cannot find " <> ToString[segment] <> " segments in run " <> runName]];
    segments[[segment]]];

ReadTimerFromRun[runName_String, timerName_String] :=
  Module[{},
  ReadTimer[ParseTimersFileFromRun[runName], timerName]];

FindTimersFromRun[runName_String, pattern_] :=
  Module[{},
  FindTimers[ParseTimersFileFromRun[runName], pattern]];


ReadTimerFromRun[runName_String, timerName_String, i_] :=
  Module[{},
  ReadTimer[ParseTimersFileFromRunCore[runName, i], timerName]];

DefineMemoFunction[ParseTimersFileFromRunCore[runName_String, i_],
  ParseTimersFile[TimersFile[runName,i]]];

DefineMemoFunction[ParseTimersFileFromRun[runName_String, core_Integer : 0, segment_Integer : 1],
  ParseTimersFile[TimersFile[runName,core,segment]]];

ChartTimersFromRun[runName_String] :=
  ChartTimers[TimersFile[runName]];

ChartTimersFromRun[runName_String, core_:0, segment_Integer:1] :=
  ChartTimers[TimersFile[runName, core, segment]];

IndependentTimersFromRun[runName_String] :=
  Module[{},
    Return[IndependentTimers[ParseTimersFileFromRun[runName]]];
  ];

ChartEvolutionTimersFromRun[runName_, nTimers_:20 (* ,core_:0,segment_:1 *)] :=
 Module[{},
  ChartTimers[
   LargestTimers[EvolutionTimers[IndependentTimersFromRun[runName]],
    nTimers]]]

collectTimers1[timers_, collectionName_ -> (members_List)] :=
 Module[{cts, ctsDel, tSum, tRep},
  cts = Select[timers, MemberQ[members, #[[1]]] &];
  ctsDel = Complement[timers, cts];
  tSum = Apply[Plus, Map[Last, cts]];
  tRep = ctsDel~Join~{{collectionName, tSum}};
  Return[tRep]
  ]

CollectTimers[timers_, cols_List] :=
 If[Length[cols] === 0, timers,
  CollectTimers[collectTimers1[timers, First[cols]], Drop[cols, 1]]]

(* New AllTimers output - incompatible with the above functions *)

DefineMemoFunction[ReadAllTimers[run_, it_: Automatic],
 Module[{timerTable, columnHeadings, lastData, lastTimers, collect},
  timerTable = Import[FindSimulationFiles[run, "AllTimers.tsv"][[1]]];
  columnHeadings = Last@Select[timerTable, First[#] === "iteration" &];
  lastData = 
   If[it === Automatic, Last[timerTable], 
    Last@Select[timerTable, #[[1]] === it &]];
  lastTimers = 
   Thread[{columnHeadings, 
      Take[lastData, Length[columnHeadings]]}][[3 ;; All]];
  collect[
    ts_] :=
   {StringReplace[ts[[1, 1]], 
     x__ ~~ " (average)" ~~ EndOfString :> x], Map[Last, ts]};
  Map[collect, Partition[lastTimers, 3]]]];

DefineMemoFunction[ReadAllTimerIterations[run_, it_: Automatic],
 Module[{timerTable, data},
  timerTable = Import[FindSimulationFiles[run, "AllTimers.tsv"][[1]]];
  data = Select[timerTable, NumberQ[First[#]] &];
  Map[First, data]]];



End[];

EndPackage[];
