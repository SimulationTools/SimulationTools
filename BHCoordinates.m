(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["BHCoordinates`", {"RunFiles`", "DataTable`", "Memo`"}];

ReadMinTrackerCoordinates;
ReadMinTrackerCoordinate;
ReadMinTrackerRadius;
ReadMinTrackerPhase;
ReadPunctureTrackerPhase;
ReadPunctureTrackerRadius;
ReadMinTrackerTrajectory;
ReadMinTrackerTrajectories;
ReadMinTrackerSpeed;
ReadMinTrackerVelocity;
HaveMinTracker;
ReadPunctureTrackerCoordinates;
ReadBHCoordinates;
ReadBHCoordinate;
ReadBHTrajectory;
ReadBHTrajectories;
ReadBHSeparation;
ReadBHPhase;
ReadBHRadius;
ReadBHPhaseOfFrequency;
ReadBHInclination;
ReadBHSpeed;
BHCoordinateMergerTime::usage = "BHCoordinateMergerTime[run,eps] returns the time at which the BHs in run reach a separation of eps (eps defaults to 0.01 if omitted).";

Begin["`Private`"];

(* MinTracker *)

HaveMinTracker[runName_, tracker_Integer] :=
  FindRunFile[runName, "MinTracker"<>ToString[tracker]<>".asc"] =!= {};

ReadMinTrackerCoordinates[runName_String, tracker_Integer] :=
  Module[{list, list2},
    list = ReadColumnFile[runName, "MinTracker"<>ToString[tracker]<>".asc", {2,3,4,5}];
    list2 = Map[{#[[1]], {#[[2]], #[[3]], #[[4]]}} &, list];
    Return[MakeDataTable[list2, {RunName -> runName}]]];

ReadMinTrackerCoordinate[runName_String, tracker_Integer, coord_Integer] :=
  Module[{coords},
    coords = ReadMinTrackerCoordinates[runName, tracker];
    MapData[#[[coord]]&, coords]];

ReadMinTrackerTrajectory[runName_String, tracker_Integer] :=
  Map[Take[Last[#], 2] &, 
    ToList[ReadMinTrackerCoordinates[runName, tracker]]];

ReadMinTrackerTrajectory[runName_String] :=
  Module[{coords1,coords2,rel},
    coords1 = ReadMinTrackerCoordinates[runName, 0];
    coords2 = ReadMinTrackerCoordinates[runName, 1];
    rel = coords1 - coords2;
    Map[Take[Last[#], 2] &, ToList[rel]]];

ReadMinTrackerTrajectories[runName_String] :=
  {ReadMinTrackerTrajectory[runName, 0], ReadMinTrackerTrajectory[runName, 1]};

ReadMinTrackerVelocity[runName_String, tracker_Integer] :=
  Module[{x,v},
    x = Table[ReadMinTrackerCoordinate[runName, tracker, dir], {dir, 1 3}];
    v = Map[NDerivative, x];
    MapThreadData[{#1,#2,#3}&, v]];

ReadMinTrackerVelocity[runName_String] :=
  ReadMinTrackerVelocity[runName, 0] - ReadMinTrackerVelocity[runName, 1];

ReadMinTrackerSpeed[runName_String] :=
  MapData[Norm, ReadMinTrackerVelocity[runName]];

ReadMinTrackerSpeed[runName_String, tr_Integer] :=
  MapData[Norm, ReadMinTrackerVelocity[runName, tr]];

(* PunctureTracker *)

DefineMemoFunction[ReadPunctureTrackerCoordinates[runName_, i_],
 Module[{nTrackers},
  nTrackers = 10;
  MakeDataTable[{#[[1]], {#[[2]], #[[3]], #[[4]]}} & /@ 
    ReadColumnFile[runName, 
     "puncturetracker::pt_loc..asc", {9, 13 + nTrackers*1 + i, 
      13 + nTrackers*2 + i, 13 + nTrackers*3 + i}]]]];

(* General BH coordinates *)

ReadBHCoordinates[runName_, i_] :=
  If[HaveMinTracker[runName, i],
    ReadMinTrackerCoordinates[runName, i],
    ReadPunctureTrackerCoordinates[runName, i]];

ReadBHCoordinate[runName_String, tracker_Integer, coord_Integer] :=
  Module[{coords},
    coords = ReadBHCoordinates[runName, tracker];
    MapData[#[[coord]]&, coords]];

ReadBHTrajectory[runName_String, tracker_Integer] :=
  Map[Take[Last[#], 2] &, 
    ToList[ReadBHCoordinates[runName, tracker]]];

ReadBHTrajectory[runName_String] :=
  Module[{coords1,coords2,rel},
    coords1 = ReadBHCoordinates[runName, 0];
    coords2 = ReadBHCoordinates[runName, 1];
    rel = coords1 - coords2;
    Map[Take[Last[#], 2] &, ToList[rel]]];

ReadBHTrajectories[runName_String] :=
  {ReadBHTrajectory[runName, 0], ReadBHTrajectory[runName, 1]};

ReadBHSeparation[runName_String] :=
  Module[{x0, x1, rad, l},
    x0 = ReadBHCoordinates[runName, 0];
    x1 = ReadBHCoordinates[runName, 1];
    l = Min[Length[x0],Length[x1]];
    rad = MapThreadData[Norm[#1-#2] &, {Take[x0,l], Take[x1,l]}];
    Return[rad];
  ];

ReadBHRadius[runName_String, i_] :=
  Module[{x0, rad},
    x0 = ReadBHCoordinates[runName, i];
    rad = MapData[Norm[#] &, x0];
    Return[rad];
  ];

ReadBHPhase[runName_String] :=
  Module[{x0, x1, l, xyTrans},
    x0 = ReadBHCoordinates[runName, 0];
    x1 = ReadBHCoordinates[runName, 1];
    l = Min[Length[x0],Length[x1]];
    xyTrans = MapThreadData[Take[#1-#2,2] &, {Take[x0,l], Take[x1,l]}]; (* Project into xy plane *)
    Return[Phase[xyTrans]];
  ];

ReadBHInclination[runName_String] :=
  Module[{x0, x1, l, x0p, x1p, rp, theta},
    x0 = ReadBHCoordinates[runName, 0];
    x1 = ReadBHCoordinates[runName, 1];
    l = Min[Length[x0],Length[x1]];
    x0p = Take[x0,l];
    x1p = Take[x1,l];
    rp = x0p - x1p;
    theta = MapData[If[Norm[#] > 10^-4, ArcCos[#[[3]]/Norm[#]], 0.0] &, rp];
    Return[Pi/2.0 - theta]];


ReadBHPhase[runName_String, i_] :=
  Module[{x0, xyTrans},
    x0 = ReadBHCoordinates[runName, i];
    xyTrans = MapData[Take[#,2] &, x0]; (* Project into xy plane *)
    Return[Phase[xyTrans]];
  ];

ReadBHPhaseOfFrequency[run_] :=
  Module[{phaseFreq, phaseFreqDeps, phaseOfFreq},
    phaseFreq =
     IntersectDataTables[{ReadBHPhase[run],
       NDerivative@ReadBHPhase[run]}];
    phaseFreqDeps = Reverse[DepVar /@ phaseFreq];
    phaseOfFreq = MakeDataTable[MapThread[List, phaseFreqDeps]]];

ReadBHSpeed[run_, bh_] :=
 Norm@NDerivative[ReadBHCoordinates[run, bh]];

(*--------------------------------------------------------------------
  Data conversion 
  --------------------------------------------------------------------*)

ReadMinTrackerRadius[runName_String] :=
  Module[{x0, x1, rad, l},
    x0 = ReadMinTrackerCoordinates[runName, 0];
    x1 = ReadMinTrackerCoordinates[runName, 1];
    l = Min[Length[x0],Length[x1]];
    rad = MapThreadData[Norm[#1-#2] &, {Take[x0,l], Take[x1,l]}];
    Return[rad];
  ];

ReadMinTrackerPhase[runName_String] :=
  Module[{x0, x1, xyTrans, l},
    x0 = ReadMinTrackerCoordinates[runName, 0];
    x1 = ReadMinTrackerCoordinates[runName, 1];
    l = Min[Length[x0],Length[x1]];
    xyTrans = MapThreadData[Take[#1-#2,2] &, {Take[x0,l], Take[x1,l]}]; (* Project into xy plane *)
    Return[Phase[xyTrans]];
  ];

ReadPunctureTrackerRadius[runName_String] :=
  Module[{x0, x1, rad, l},
    x0 = ReadPunctureTrackerCoordinates[runName, 0];
    x1 = ReadPunctureTrackerCoordinates[runName, 1];
    l = Min[Length[x0],Length[x1]];
    rad = MapThreadData[Norm[#1-#2] &, {Take[x0,l], Take[x1,l]}];
    Return[rad];
  ];

ReadPunctureTrackerPhase[runName_String] :=
  Module[{x0, x1, xyTrans, l},
    x0 = ReadPunctureTrackerCoordinates[runName, 0];
    x1 = ReadPunctureTrackerCoordinates[runName, 1];
    l = Min[Length[x0],Length[x1]];
    xyTrans = MapThreadData[Take[#1-#2,2] &, {Take[x0,l], Take[x1,l]}]; (* Project into xy plane *)
    Return[Phase[xyTrans]];
  ];

BHCoordinateMergerTime[run_, eps_:0.01] :=
 Module[{sep},
  sep = ReadBHSeparation[run];
  Select[ToList@sep, #[[2]] > eps &][[-1, 1]]]

End[];

EndPackage[];
