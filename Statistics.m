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

BeginPackage["SimulationTools`Statistics`",
 {
  "SimulationTools`DataTable`",
  "SimulationTools`ColumnFile`",
  "SimulationTools`Error`",
  "SimulationTools`Parameters`",
  "SimulationTools`RunFiles`",
  "SimulationTools`SimulationProperties`",
  "SimulationTools`SystemStatistics`",
  "SimulationTools`Utils`"
 }];

(* Experimental *)
FinishTime(*::usage = "FinishTime[sim] returns a date list corresponding to the estimated time at which the simulation will finish if it runs continuously."*);
LastOutputCoordinateTime;
FinalCoordinateTime;
TailStandardOutputOfSimulation;
TailStandardErrorOfSimulation;
SimulationStatus;
SimulationErrorReason;
SimulationPerformancePanel;

Begin["`Private`"];

segmentInfo[dirName_] :=
 Module[{mtFile, mtTimes, mtt1, mtt2, idNos, idNo, date, col},
  mtFile = FileNameJoin[{dirName, "/runstats.asc"}];
  col=2;
  If[FileType[mtFile] === None,
  mtFile =  FileNameJoin[{dirName, "/carpet::timing..asc"}];
  col=9];

  If[FileType[mtFile] =!= None,
   mtTimes = Flatten[ReadColumnFile[mtFile, {col}]];
   mtt1 = First[mtTimes];
   mtt2 = Last[mtTimes];
   date = DateString[FileDate[mtFile]],
   
   mtt1 = "";
   mtt2 = "";
   date = ""];
  
  idNos = StringCases[
     dirName, (__ ~~ "/output-" ~~ id__ ~~ "/" ~~ __) -> id];
  idNo = If[idNos =!= {}, idNos[[1]], "0000"];

  {ID -> idNo, LastDate -> date,
   T1 -> mtt1, T2 -> mtt2}]

segmentSummary[runName_] :=
 Module[{segs, segInfos, header, tab},
  segs = FindRunSegments[runName];
  segInfos = Map[segmentInfo, segs];
  header = {Style["Segment", Italic], Style["Last output", Italic], 
    Style["t1", Italic], Style["t2", Italic]};
  tab = {header}~Join~
    Map[{ID /. #, LastDate /. #, T1 /. #, T2 /. #} &, segInfos];
  Return[Grid[tab, Spacings -> 2, Alignment -> Left]];
  ]

LastOutputTime[run_] :=
  Module[{segs,segInfos,segInfo,lastDate},
    segs = FindRunSegments[run];
    segInfos = segmentInfo/@segs;
    segInfo = Last[Select[segInfos, ((T2 /. #) =!= "") &]];
    lastDate = DateList[LastDate/.segInfo]];

LastOutputCoordinateTime[run_] :=
  Module[{segs,segInfos,segInfo},
    segs = FindRunSegments[run];
    segInfos = segmentInfo/@segs;
    If[(T2/.segInfos[[1]]) === "", Return[None]];
    segInfo = Last[Select[segInfos, ((T2 /. #) =!= "") &]];
    T2/.segInfo];

FinalCoordinateTime[run_] :=
  If[FindSimulationParameters[run, "Cactus::cctk_final_time"] =!= {},
     ToExpression@LookupParameter[run, "Cactus::cctk_final_time"],
     None];

FinishTime[run_] :=
  Module[{segs,segInfos,segInfo,t,tFinal,speed,seconds,lastDate,finishDate},
    segs = FindRunSegments[run];
    segInfos = segmentInfo/@segs;
    If[(T2/.segInfos[[1]]) === "", Return[None]];
    segInfo = Last[Select[segInfos, ((T2 /. #) =!= "") &]];
    t = T2/.segInfo;

    tFinal = FinalCoordinateTime[run];

    speed = Last@DepVar@ReadRunSpeed@run;
    seconds = (tFinal-t)/speed*3600;

    lastDate = DateList[LastDate/.segInfo];
    finishDate = DatePlus[lastDate, {seconds,"Second"}]
  ];

FinishTimeString[run_] :=
  Module[{ft},
    If[Abs[LastOutputCoordinateTime[run] - FinalCoordinateTime[run]] < 1,
      "Finished",
       ft = FinishTime[run];
       If[ft =!= None, DateString[ft], "-"]]];


cost[run_] :=
  Item[#,Alignment->Right] & /@ 
  {run,
   WithExceptions[ReadSimulationCoreCount[run],            NoSimulationCoreCountAvailable -> "-"],
   WithExceptions[ReadSimulationCost[run],         NoSimulationCoreCountAvailable -> "-"],
   WithExceptions[ReadWalltimeHours[run]/24, NoSimulationRunTimeAvailable -> "-"],
   WithExceptions[ReadSimulationCoreCount[run]*24,         NoSimulationCoreCountAvailable -> "-"],
   FinishTimeString[run]};

formatStatus[x_] := x;
formatStatus[x_String[y_String]] := Pane[x <> " (" <> y <> ")", 700];

SimulationTools`Statistics`SimulationOverview`Plots[runNames_] :=
  Module[
    {costTable, segments, statusTable, statuses},

    segments = {{Style["Simulation", Bold], Style["Segments", Bold]}}~
    Join~Map[{#, segmentSummary[#]} &, runNames];

    statuses = SimulationStatus /@ runNames;

    MapThread[Sow[<|"SimulationName" -> #1, "Status" -> #2|>, SimulationTools`SimView`Statistic] &, {runNames, statuses}]; 

    statusTable = Grid[MapThread[{#1,formatStatus[#2]}&, {runNames, statuses}]];

    costTable = {{Grid[{{Style["Simulation",Bold],
                       Style["Cores",Bold],
                       Style["CPU hrs.",Bold],
                       Style["Days",Bold],
                       Style["CPU hrs./day",Bold],
                       Style["Estimated finish", Bold]}}
                     ~Join~
                     Map[cost, runNames], Spacings->{2,0.5}], SpanFromLeft},
                {statusTable, SpanFromLeft},
                {SimulationPerformancePanel[runNames]},
                {Grid[segments], SpanFromLeft}}];

TailStandardOutputOfSimulation[sim_String, n_Integer] :=
 Module[{outFiles},
  outFiles = StandardOutputOfRun[sim];
  If[Length[outFiles] === 0, 
   Error["No standard output available in simulation " <> sim]];
  TailFile[Last[outFiles], n]];

TailStandardErrorOfSimulation[sim_String, n_Integer] :=
 Module[{outFiles},
  outFiles = StandardErrorOfRun[sim];
  If[Length[outFiles] === 0, 
   Error["No standard error available in simulation " <> sim]];
  TailFile[Last[outFiles], n]];

SimulationErrorReason[sim_String] :=
  SimulationErrorReason[TailStandardOutputOfSimulation[sim,10240], TailStandardErrorOfSimulation[sim,10240]];

SimulationErrorReason[stdout_String, stderr_String] :=
 Module[{warnings, exceptions},
  warnings = 
   StringCases[stdout<>"\n"<>stderr, 
    ("WARNING level 0"|"ERROR in thorn") ~~ Shortest[__] ~~ EndOfLine ~~ Shortest[__] ~~ 
      EndOfLine ~~ (Whitespace ~~ "->" ~~Whitespace ~~ msg : Shortest[__]) ~~ EndOfLine :> msg];
  If[Length[warnings] > 0, warnings[[-1]],
   exceptions = 
    StringCases[stderr, 
     msg : ("terminate called after throwing an instance of" ~~ 
          Shortest[__]) ~~ EndOfLine :> msg];
   If[Length[exceptions] > 0, exceptions[[-1]],
     With[{mpiErrs =
     StringCases[stderr,
       "The InfiniBand retry count between two MPI processes has been"]},
     If[Length[mpiErrs] > 0, "Infiniband retry count exceeded to host "<>StringCases[stderr, "Peer host:"~~Whitespace~~Shortest[h__]~~EndOfLine:>h][[1]],
    "Unknown error"]]]]];

SimulationStatus[sim_String] :=
 Module[{tCurrent = LastOutputCoordinateTime[sim], 
   tFinal = FinalCoordinateTime[sim], 
   stdout = TailStandardOutputOfSimulation[sim, 20240]},
  Which[
   tCurrent == tFinal, "Finished",
   StringMatchQ[
    stdout, __ ~~ StartOfLine ~~ "Done." ~~ EndOfLine ~~ __], 
   Which[StringMatchQ[stdout, __ ~~ StartOfLine ~~ "INFO (TerminationTrigger): Remaining wallclock time for your job is " ~~ Shortest[__] ~~ " minutes.  Triggering termination..." ~~ EndOfLine ~~ __],
     "Unfinished[Walltime]",
     StringMatchQ[stdout, __ ~~ StartOfLine ~~ "INFO (SystemStatistics): Process resident set size "  ~~ Shortest[__] ~~ " is over the threshold of " ~~ Shortest[__] ~~ " set in termination_rss_threshold_mb.  Triggering termination..." ~~ EndOfLine ~~ __],
     "Unfinished[Memory]",
     StringMatchQ[stdout, __ ~~ StartOfLine ~~ "INFO (SystemStatistics): System swap usage "  ~~ Shortest[__] ~~ " is over the threshold of " ~~ Shortest[__] ~~ " set in termination_swap_threshold_mb.  Triggering termination..." ~~ EndOfLine ~~ __],
     "Unfinished[Memory]",
     True, "Unfinished"],
   StringMatchQ[
    stdout, __ ~~ StartOfLine ~~ "Simfactory Done at date" ~~ __], 
   "Error"[SimulationErrorReason[stdout, TailStandardErrorOfSimulation[sim, 10240]]],
   True, "Running"[N[tCurrent/tFinal]]]];

SimulationPerformancePanel[sims_List] :=
 Grid[Join[{Style[#,Bold]&/@{"Simulation","Speed","Memory/GB","Swap/MB","Cores"}},
   Table[
   Module[{speed = Last[ReadSimulationSpeed[sim]], cores = ReadSimulationCoreCount[sim]},
     Sow[<|"SimulationName" -> sim, "Speed" -> speed, "Cores" -> cores|>, SimulationTools`SimView`Statistic];
     {sim, NumberForm[speed,{2,1}], 
       NumberForm[Last[ReadSimulationMemoryUsage[sim]]/1024.,{2,1}],
       Last[ReadSimulationSwapUsage[sim]],
       cores}], {sim, sims}]],Alignment->{{Left,".",Right,Right,Right}}];  

End[];
EndPackage[];
