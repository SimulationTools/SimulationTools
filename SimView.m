
BeginPackage["SimView`", {"NR`", "RunFiles`", "DataTable`", "Memo`", "BHCoordinates`", "Statistics`"}];

SimView;

Begin["`Private`"];

segmentInfo[dirName_] :=
 Module[{mtFile, mtTimes, mtt1, mtt2, idNo, id, date, col},
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

SimView[runName_String] := SimView[{runName}];

SimView[runNames_List] :=
  Module[{r, rads},
    rads = ReadPsi4Radii[First[runNames]];
    r = If[rads === {}, 0, First[rads]];
    SimView[runNames, r]];

memoryPlot[runNames_List, size_] :=
  Module[{swaps, mems},
   swaps = Catch[Map[ReadSwap, runNames],RunFiles`Private`UnknownColumns];
   If[StringQ[swaps], swaps = {{0,0}}];
   mems = Map[ReadMemory, runNames];

   Show[ListLinePlot[mems], ListLinePlot[swaps, PlotStyle->Dashed],
     PlotRange -> {0, All}, AxesOrigin->{0,0}, PlotLabel -> "Memory", ImageSize -> size]];

SimView[runNames_List, r_] :=
 Module[{speed, trajectories, size, memory, radius, frequency, rePsi4,
    freqPsi4, segments, cost, costTable, phases, lastPhase, phaseDiffs},
  size = {350, 100};
  size = 250;
  speed = Catch[
   ListLinePlot[Map[ReadRunSpeed, runNames], 
    PlotRange -> {0, All}, PlotLabel -> "Speed", ImageSize -> size]];
  memory = memoryPlot[runNames, size];
  trajectories = Catch[
   ListLinePlot[
    Flatten[Map[
      ReadBHTrajectories, runNames], 1], 
    AspectRatio -> Automatic, PlotLabel -> "Trajectories", 
    ImageSize -> size, PlotRange -> All]];
  radius = Catch[
   ListLinePlot[
    Map[ReadBHSeparation, runNames], 
    PlotRange -> {0, All}, PlotLabel -> "Separation", ImageSize -> size]];
  frequency = Catch[
   ListLinePlot[
    Map[NDerivative[ReadMinTrackerPhase[#]]&, runNames],
    PlotRange -> {0, Automatic}, PlotLabel -> "Frequency", 
    ImageSize -> size]];
  rePsi4 = Catch[
   ListLinePlot[
    Map[Re[ReadPsi4[#, 2, 2, r]]&, runNames], 
    PlotRange -> All, PlotLabel -> "Re[Psi422], R = "<>ToString[r], ImageSize -> size]];
  ampPsi4 = Catch[
   ListLogPlot[
    Map[ToList[Abs[ReadPsi4[#, 2, 2, r]]]&, runNames], 
    PlotRange -> All, Joined->True, PlotLabel -> "|Psi422|, R = "<>ToString[r], ImageSize -> size]];
  freqPsi4 = Catch[
   ListLinePlot[
    Map[NDerivative[
      Phase[ReadPsi4[#, 2, 2, r]]]&,runNames], 
    PlotRange -> Automatic, PlotLabel -> "Freq Psi422, R = "<>ToString[r], 
    ImageSize -> size]];

  segments = {{Style["Simulation", Bold], Style["Segments", Bold]}}~
    Join~Map[{#, segmentSummary[#]} &, runNames];

  cost[run_] := 
    {run, Catch@ReadCores[run],  Catch@ReadCPUHours[run],
      Catch@ReadWalltimeHours[run]/24,  Catch@ReadCores[run]*24};

  costTable = Grid[{{Style["Simulation",Bold],
                     Style["Cores",Bold], 
                     Style["CPU hours",Bold], 
                     Style["Days",Bold], 
                     Style["CPU hours per day",Bold]}}
                    ~Join~
                   Map[cost, runNames]];

  grid = Grid[{{Text[Style[StringJoin[Riffle[runNames,", "]], Bold, 24]], SpanFromLeft},
       {speed, memory}, 
       {trajectories, radius},
       {rePsi4, ampPsi4},
       {freqPsi4,costTable}}~Join~
       segments, 
       Spacings -> {0, 1}];
  Return[grid]
  ];

End[];

EndPackage[];
