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

BeginPackage["SimulationTools`MinTracker`",
 {
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`Memo`",
  "SimulationTools`RunFiles`"
 }];

Begin["`Private`"];

fileName[tracker_] := "MinTracker"<>ToString[tracker]<>".asc";

SimulationTools`MinTracker`BHCoordinates`ReadBHCoordinates[runName_String,
                                                   tracker_Integer] :=
  Module[{list, list2},
    list = ReadColumnFile[runName, fileName[tracker], {2,3,4,5}];
    list2 = Map[{#[[1]], {#[[2]], #[[3]], #[[4]]}} &, list];
    Return[MakeDataTable[list2, {RunName -> runName}]]];

SimulationTools`MinTracker`Trackers`ReadCoordinates[runName_String, 
                                      tracker_Integer] :=
  Module[{list},
    list = ReadColumnFile[runName, fileName[tracker], {2,3,4,5}];
    Return[Table[ToDataTable[list[[All,{1,dir+1}]]], {dir, 1, 3}]]];

SimulationTools`MinTracker`BHCoordinates`HaveData[runName_String, tracker_Integer] :=
  FileIsInRun[runName, fileName[tracker]];

End[];

EndPackage[];
