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

BeginPackage["SimulationTools`PunctureTracker`",
 {
  "SimulationTools`ColumnFile`",
  "SimulationTools`DataTable`",
  "SimulationTools`RunFiles`"
 }];

ReadPunctureVelocity;
ReadPuncturePosition;
$PunctureTrackerDataFileName;

Begin["`Private`"];

dataFileName[] :=
  If[ValueQ[$PunctureTrackerDataFileName],
    $PunctureTrackerDataFileName,
    "puncturetracker"~~("-"|"::")~~"pt_loc..asc"];

SimulationTools`PunctureTracker`BHCoordinates`ReadBHCoordinates[runName_, i_] :=
 Module[{nTrackers, data},
  nTrackers = 10;
  data = ReadColumnFile[runName, dataFileName[],
    {9, 13 + nTrackers*1 + i, 13 + nTrackers*2 + i, 13 + nTrackers*3 + i}];
  ToDataTable[data[[All, 1]], data[[All, {2, 3, 4}]]]
];

SimulationTools`PunctureTracker`Trackers`ReadCoordinates[runName_, i_] :=
 ReadPuncturePosition[runName, i];

ReadPunctureVelocity[runName_, i_] :=
 Module[{nTrackers, data},
  nTrackers = 10;
  data = ReadColumnFile[runName, "puncturetracker"~~("-"|"::")~~"pt_vel..asc",
    {9, 13 + nTrackers*1 + i, 13 + nTrackers*2 + i, 13 + nTrackers*3 + i}];
  Table[ToDataTable[data[[All, 1]],  data[[All, dir+1]]], {dir, 1, 3}]
];

ReadPuncturePosition[runName_, i_] :=
 Module[{nTrackers, data},
  nTrackers = 10;
  data = ReadColumnFile[runName, dataFileName[],
    {9, 13 + nTrackers*1 + i, 13 + nTrackers*2 + i, 13 + nTrackers*3 + i}];
  Table[ToDataTable[data[[All, 1]], data[[All, dir+1]]], {dir, 1, 3}]
];

SimulationTools`PunctureTracker`BHCoordinates`HaveData[runName_String, tracker_Integer] :=
  FileIsInRun[runName, dataFileName[]];

End[];

EndPackage[];
