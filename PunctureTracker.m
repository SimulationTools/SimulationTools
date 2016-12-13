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

SimulationTools`PunctureTracker`BHCoordinates`ReadBHCoordinates[sim_, i_] :=
 readPunctureVector[sim, dataFileName[], i];

SimulationTools`PunctureTracker`Trackers`ReadCoordinates[sim_, i_] :=
 readPunctureScalar[sim, dataFileName[], i];

ReadPuncturePosition[sim_, i_] :=
  readPunctureScalar[sim, dataFileName[], i];

ReadPunctureVelocity[sim_, i_] :=
  readPunctureScalar[sim, "puncturetracker"~~("-"|"::")~~"pt_vel..asc", i];

(* Read data as a list of scalar-valued DataTables *)
readPunctureScalar[sim_, filename_, i_Integer] :=
  First[readPunctureScalar[sim, filename, {i}]];

readPunctureScalar[sim_, filename_, is_List] :=
 Module[{data},
  data = readPunctureData[sim, filename, is];
  Table[ToDataTable[data[[All, 1]], data[[All, 3(i-1)+dir+1]]], {i, Length[is]}, {dir, 1, 3}]
];

(* Read data as a list of vector-valued DataTables *)
readPunctureVector[sim_, filename_, i_Integer] :=
  First[readPunctureVector[sim, filename, {i}]];
readPunctureVector[sim_, filename_, is_List] :=
 Module[{data},
  data = readPunctureData[sim, filename, is];
  Table[ToDataTable[data[[All, 1]], data[[All, 3(i-1)+1+{1,2,3}]]], {i, Length[is]}]
];

(* Load data *)
readPunctureData[sim_, filename_, is_List] :=
 Module[{nTrackers, data},
  nTrackers = 10;
  data = ReadColumnFile[sim, filename,
    Flatten[{"time", {"pt_loc_x[" <> ToString[#] <> "]",
     "pt_loc_y[" <> ToString[#] <> "]",
     "pt_loc_z[" <> ToString[#] <> "]"} & /@ is}]];
  data
];

SimulationTools`PunctureTracker`BHCoordinates`HaveData[runName_String, tracker_Integer] :=
  FileIsInRun[runName, dataFileName[]];

End[];

EndPackage[];
