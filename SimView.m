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

BeginPackage["SimView`",
 {
  "BHCoordinates`",
  "DataRepresentations`",
  "DataTable`",
  "Error`",
  "Horizons`",
  "Memo`",
  "NR`",
  "Parameters`",
  "Plotting`",
  "Providers`",
  "RunFiles`",
  "SimViewRRMHD`",
  "SystemStatistics`",
  "Waveforms`"
 }];

SimulationOverview::usage = "SimulationOverview[sim] gives a quick overview of the simulation sim.
SimView[{sim1, sim2, ...}] gives a quick overview of the simulations sim1, sim2, ....";

(****************************************************************)
(* Deprecated                                                   *)
(****************************************************************)

SimView;
FinishTime(*::usage = "FinishTime[sim] returns a date list corresponding to the estimated time at which the simulation will finish if it runs continuously."*);

Begin["`Private`"];

(**********************************************************)
(* SimulationOverview                                     *)
(**********************************************************)

Options[SimulationOverview] = {
  "SimulationType" -> Automatic
};

SyntaxInformation[SimulationOverview] =
 {"ArgumentsPattern" -> {_, OptionsPattern[]}};

SimulationOverview[sim_String, opts___] :=
  SimulationOverview[{sim}, opts];

SimulationOverview[sims_List, OptionsPattern[]] :=
 Module[{type, res},
  type = OptionValue[SimulationType];

  (* TODO: Add detection of the simulation type *)
  type = type /. Automatic -> "BBH";

  Which[
   type === "BBH",
    res = SimView[sims];,
   MatchQ[type, {"BBH", "ExtractionRadius" -> _}],
   	res = SimView[sims, type[[2,2]]];,
   type === "RRMHD",
    res = SimViewRRMHD[sims];,
   True,
    Error["Unrecognised simulation type " <> ToString[type]];
  ];

  res
];


(****************************************************************)
(* Deprecated                                                   *)
(****************************************************************)

segmentInfo[dirName_] :=
 Module[{mtFile, mtTimes, mtt1, mtt2, idNo, date, col},
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
  
  idNo = StringCases[
     dirName, (__ ~~ "/output-" ~~ id__ ~~ "/" ~~ __) -> id][[1]];
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

SimView[runName_String, opts___] := SimView[{runName}, opts];

SimView[runNames_List] :=
  Module[{r, rads},
    If[And@@(HaveData["Waveforms",#] & /@ runNames),         
      rads = ReadPsi4Radii[First[runNames]],
      rads = {}];
      r = If[rads === {}, 0, First[rads]];
      SimView[runNames, r]];

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
    segInfo = Last[Select[segInfos, ((T2 /. #) =!= "") &]];
    T2/.segInfo];

FinalCoordinateTime[run_] :=
  ToExpression@LookupParameter[run, "Cactus::cctk_final_time"];

FinishTime[run_] :=
  Module[{segs,segInfos,segInfo,t,tFinal,speed,seconds,lastDate,finishDate},
    segs = FindRunSegments[run];
    segInfos = segmentInfo/@segs;
    segInfo = Last[Select[segInfos, ((T2 /. #) =!= "") &]];
    t = T2/.segInfo;

    tFinal = FinalCoordinateTime[run];

    speed = Last@DepVar@ReadRunSpeed@run;
    seconds = (tFinal-t)/speed*3600;

    lastDate = DateList[LastDate/.segInfo];
    finishDate = DatePlus[lastDate, {seconds,"Second"}]
  ];

FinishTimeString[run_] :=
  Module[{},
    If[Abs[LastOutputCoordinateTime[run] - FinalCoordinateTime[run]] < 1,
      "Finished",
      DateString[FinishTime[run]]]];

SimView[runNames_List, r_] :=
 Module[{speed, trajectories, size, memory, radius, frequency, rePsi4,
    segments, cost, costTable, ampPsi4, grid, plots},
  size = {350, 100};
  size = 250;

  (* spinNorms = Catch[ *)
  (*  PresentationListLinePlot[ *)
  (*    Flatten@Table[ *)
  (*      Norm@ReadIsolatedHorizonDimensionlessSpin[run, hn],{run,runNames},{hn,0,1}], *)
  (*   PlotRange -> {0,Automatic}, PlotLabel -> "S_i/m^2\n", ImageSize -> size],_]; *)

  (* spinPhases = Catch[ *)
  (*  PresentationListLinePlot[ *)
  (*    Flatten@Table[ *)
  (*      ReadIsolatedHorizonSpinPhase[run, hn]/Degree,{run,runNames},{hn,0,1}], *)
  (*   PlotRange -> Automatic, PlotLabel -> "arg[S_i]/deg\n", ImageSize -> size],_]; *)

  (* segments = {{Style["Simulation", Bold], Style["Segments", Bold]}}~ *)
  (*   Join~Map[{#, segmentSummary[#]} &, runNames]; *)

  (* cost[run_] :=  *)
  (*   Item[#,Alignment->Right] & /@ {run, Catch[ReadCores[run],_],  Catch[ReadCPUHours[run],_], *)
  (*     Catch[ReadWalltimeHours[run]/24,_],  Catch[ReadCores[run]*24,_], *)
  (*     DateString[FinishTime[run]] *)
  (*     }; *)

  (* costTable = Grid[{{Style["Simulation",Bold], *)
  (*                    Style["Cores",Bold],  *)
  (*                    Style["CPU hrs.",Bold],  *)
  (*                    Style["Days",Bold],  *)
  (*                    Style["CPU hrs./day",Bold], *)
  (*                    Style["Estimated finish", Bold]}} *)
  (*                   ~Join~ *)
  (*                  Map[cost, runNames], Spacings->{2,0.5}]; *)

  plots = Join[SystemStatistics`SimulationOverview`Plots[runNames],
               Binary`SimulationOverview`Plots[runNames],
               Waveforms`SimulationOverview`Plots[runNames]];

  grid = Grid[{{Text[Style[StringJoin[Riffle[runNames,", "]], Bold, 24]], SpanFromLeft}} ~Join~ plots,
       Spacings -> {0, 1}];


  Return[grid]
  ];

End[];

EndPackage[];
