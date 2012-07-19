(* Copyright (C) 2012 Ian Hinder and Barry Wardell *)

BeginPackage["PunctureTracker`",
 {
  "DataRepresentations`",
  "DataTable`",
  "Error`",
  "Memo`",
  "RunFiles`"
 }];

Begin["`Private`"];

fileName =
  "puncturetracker::pt_loc..asc";

PunctureTracker`BHCoordinates`ReadBHCoordinates[runName_, i_] :=
 Module[{nTrackers},
  nTrackers = 10;
  MakeDataTable[{#[[1]], {#[[2]], #[[3]], #[[4]]}} & /@ 
    ReadColumnFile[runName, fileName,
                   {9, 13 + nTrackers*1 + i, 
      13 + nTrackers*2 + i, 13 + nTrackers*3 + i}]]];

PunctureTracker`Trackers`ReadCoordinates[runName_, i_] :=
 Module[{nTrackers},
  nTrackers = 10;
  Table[ToDataTable[{#[[1]], #[[dir+1]]} & /@ 
    ReadColumnFile[runName, fileName,
                   {9, 13 + nTrackers*1 + i, 
      13 + nTrackers*2 + i, 13 + nTrackers*3 + i}]], {dir, 1, 3}]];

PunctureTracker`BHCoordinates`HaveData[runName_String, tracker_Integer] :=
  FileIsInRun[runName, fileName];

End[];

EndPackage[];
