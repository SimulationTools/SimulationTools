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

BeginPackage["Statistics`",
 {
  "DataTable`",
  "Error`",
  "Memo`",
  "Parameters`",
  "RunFiles`",
  "SystemStatistics`"
 }];


Begin["`Private`"];

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


cost[run_] :=
  Module[{cores = ReadCores[run]},
  Item[#,Alignment->Right] & /@ 
  {run, cores,  ReadCPUHours[run],
   ReadWalltimeHours[run]/24, If[cores =!= None, cores*24, None],
   DateString[FinishTime[run]]
  } /. None -> "-"];

Statistics`SimulationOverview`Plots[runNames_] :=
  Module[
    {costTable, segments},

    segments = {{Style["Simulation", Bold], Style["Segments", Bold]}}~
    Join~Map[{#, segmentSummary[#]} &, runNames];

    costTable = {{Grid[{{Style["Simulation",Bold],
                       Style["Cores",Bold],
                       Style["CPU hrs.",Bold],
                       Style["Days",Bold],
                       Style["CPU hrs./day",Bold],
                       Style["Estimated finish", Bold]}}
                     ~Join~
                     Map[cost, runNames], Spacings->{2,0.5}], SpanFromLeft},
                {Grid[segments], SpanFromLeft}}];

End[];
EndPackage[];
