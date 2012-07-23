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
