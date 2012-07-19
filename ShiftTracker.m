(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["ShiftTracker`",
 {
  "DataRepresentations`",
  "DataTable`",
  "Error`",
  "Memo`",
  "RunFiles`"
 }];

Begin["`Private`"];

fileName[tracker_] := "ShiftTracker"<>ToString[tracker]<>".asc";

ShiftTracker`BHCoordinates`ReadBHCoordinates[runName_String,
                                             tracker_Integer] :=
  Module[{list, list2},
    list = ReadColumnFile[runName, fileName[tracker], {2,3,4,5}];
    list2 = Map[{#[[1]], {#[[2]], #[[3]], #[[4]]}} &, list];
    Return[MakeDataTable[list2, {RunName -> runName}]]];

ShiftTracker`Trackers`ReadCoordinates[runName_String, 
                                      tracker_Integer] :=
  Module[{list},
    list = ReadColumnFile[runName, fileName[tracker], {2,3,4,5}];
    Return[Table[ToDataTable[list[[All,{1,dir+1}]]], {dir, 1, 3}]]];

ShiftTracker`BHCoordinates`HaveData[runName_String, tracker_Integer] :=
  FileIsInRun[runName, fileName[tracker]];

End[];

EndPackage[];
