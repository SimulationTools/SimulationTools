(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

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
  "RunFiles`",
  "SystemStatistics`",
  "Waveforms`"
 }];

SimView::usage = "SimView[sim] gives a quick overview of the simulation sim.
SimView[sim, rad] gives a quick overview of the simulation sim with waveforms extracted at radius rad.
SimView[{sim1, sim2, ...}] gives a quick overview of the simulations sim1, sim2, ....
SimView[{sim1, sim2, ...}, rad] gives a quick overview of the simulations sim1, sim2, ... with waveforms extracted at radius rad.";

FinishTime::usage = "FinishTime[run] returns a date list corresponding to the estimated time at which the simulation will finish if it runs continuously."

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

SimView[runName_String, opts___] := SimView[{runName}, opts];

SimView[runNames_List] :=
  Module[{r, rads},
    rads = ReadPsi4Radii[First[runNames]];
    r = If[rads === {}, 0, First[rads]];
    SimView[runNames, r]];

memoryPlot[runNames_List, size_] :=
  Module[{swaps, mems},
   swaps = Catch[Catch[Map[ReadSwap, runNames],RunFiles`Private`UnknownColumns],_];
   If[StringQ[swaps], swaps = {{0,0}}];
   mems = Catch[Catch[Map[ReadMemory, runNames],RunFiles`Private`UnknownColumns],_];
   If[StringQ[mems], mems = {{0,0}}];

   Show[PresentationListLinePlot[mems, PlotLegend -> runNames, LegendPosition -> {Left, Bottom}],
     PresentationListLinePlot[swaps, PlotStyle->{Dashed}],
     PlotRange -> {0, All}, AxesOrigin->{0,0}, PlotLabel -> "Memory\n", ImageSize -> size]];

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
    segments, cost, costTable, ampPsi4, grid},
  size = {350, 100};
  size = 250;
  speed = Catch[
   PresentationListLinePlot[Map[ReadRunSpeed, runNames], 
    PlotRange -> {0, All}, PlotLabel -> "Speed\n", ImageSize -> size,
    PlotLegend -> runNames, LegendPosition -> {Left, Bottom}], _];
  memory = memoryPlot[runNames, size];
  trajectories = Catch[
   PresentationListLinePlot[
    Flatten[Map[
      ReadBHTrajectories, runNames], 1], 
    AspectRatio -> Automatic, PlotLabel -> "Trajectories\n", 
    ImageSize -> size, PlotRange -> All],_];
  radius = Catch[
   PresentationListLinePlot[
    Map[ReadBHSeparation, runNames], 
    PlotRange -> {0, All}, PlotLabel -> "Separation\n", ImageSize -> size,
    PlotLegend -> runNames, LegendPosition -> {Right, Top}],_];
  frequency = Catch[
   PresentationListLinePlot[
    Map[NDerivative[ReadBHPhase[#]]&, runNames],
    PlotRange -> {0, Automatic}, PlotLabel -> "Frequency\n", 
    ImageSize -> size],_];
  rePsi4 = Catch[
   PresentationListLinePlot[
    Map[Re[ReadPsi4[#, 2, 2, r]]&, runNames], 
    PlotRange -> All, PlotLabel -> "Re[Psi422], R = "<>ToString[r]<>"\n", ImageSize -> size,
    PlotLegend -> runNames],_];
  ampPsi4 = Catch[
   PresentationListLinePlot[
    (Log10@Abs@ReadPsi4[#, 2, 2, r] &) /@ runNames, 
    PlotRange -> All, PlotLabel -> "|Psi422|, R = "<>ToString[r]<>"\n", ImageSize -> size,
    PlotLegend -> runNames,
    FrameTicks -> {{Table[{x,Superscript[10,x]}, {x,-10,10,2}],None},{Automatic,None}}],_];

  spinNorms = Catch[
   PresentationListLinePlot[
     Flatten@Table[
       Norm@ReadIsolatedHorizonDimensionlessSpin[run, hn],{run,runNames},{hn,0,1}],
    PlotRange -> {0,Automatic}, PlotLabel -> "S_i/m^2\n", ImageSize -> size],_];

  spinPhases = Catch[
   PresentationListLinePlot[
     Flatten@Table[
       ReadIsolatedHorizonSpinPhase[run, hn]/Degree,{run,runNames},{hn,0,1}],
    PlotRange -> Automatic, PlotLabel -> "arg[S_i]/deg\n", ImageSize -> size],_];

  segments = {{Style["Simulation", Bold], Style["Segments", Bold]}}~
    Join~Map[{#, segmentSummary[#]} &, runNames];

  cost[run_] := 
    Item[#,Alignment->Right] & /@ {run, Catch[ReadCores[run],_],  Catch[ReadCPUHours[run],_],
      Catch[ReadWalltimeHours[run]/24,_],  Catch[ReadCores[run]*24,_],
      DateString[FinishTime[run]]
      };

  costTable = Grid[{{Style["Simulation",Bold],
                     Style["Cores",Bold], 
                     Style["CPU hrs.",Bold], 
                     Style["Days",Bold], 
                     Style["CPU hrs./day",Bold],
                     Style["Estimated finish", Bold]}}
                    ~Join~
                   Map[cost, runNames], Spacings->{2,0.5}];

  grid = Grid[{{Text[Style[StringJoin[Riffle[runNames,", "]], Bold, 24]], SpanFromLeft},
       {speed, memory}, 
       {trajectories, radius},
       {rePsi4, ampPsi4},
       {costTable,SpanFromLeft},
       {spinNorms, spinPhases}}~Join~
       segments, 
       Spacings -> {0, 1}];
  Return[grid]
  ];

End[];

EndPackage[];
