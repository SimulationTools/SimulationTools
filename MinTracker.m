
BeginPackage["MinTracker`", {"RunFiles`", "DataTable`", "Memo`", "Error`"}];

Begin["`Private`"];

fileName[tracker_] := "MinTracker"<>ToString[tracker]<>".asc";

MinTracker`BHCoordinates`ReadBHCoordinates[runName_String,
                                                   tracker_Integer] :=
  Module[{list, list2},
    list = ReadColumnFile[runName, fileName[tracker], {2,3,4,5}];
    list2 = Map[{#[[1]], {#[[2]], #[[3]], #[[4]]}} &, list];
    Return[MakeDataTable[list2, {RunName -> runName}]]];

MinTracker`BHCoordinates`HaveData[runName_String, tracker_Integer] :=
  FileIsInRun[runName, fileName[tracker]];

End[];

EndPackage[];
