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

BeginPackage["SimulationTools`CarpetVTK`",
 {
  "SimulationTools`DataRegion`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`Error`"
 }];

(* Exported symbols *)

ReadVTKFile;

Begin["`Private`"];

(* VTK file reader *)
ReadVTKFile[fileName_String] :=
  Module[{s,result},
    s = OpenRead[fileName, BinaryFormat->True];
    result = ReadVTKFile[s];
    Close[s];
    result];

ReadVTKFile[s_InputStream] :=
 Module[{header, header2, readInfo, dims, origin, spacing, data, 
   dataRange, max, nx, ny, nz, nPoints, data2, zSlices, varName},
  header = ReadList[s, String, 10];
  If[Length[header] == 0,
    Return[EndOfFile]];
  If[Length[header] != 10,
    Error["Did not read complete header from VTK stream"]];
  header2 = Map[StringSplit, header];
  readInfo[h_List, key_String] :=
   Module[{result},
    result = ToExpression /@ Cases[h, {key, vals__} -> {vals}];
    If[Length[result] == 0, 
     Error["Cannot find key " <> key <> " in header: " <> 
       ToString[h]]];
    result[[1]]];
  dims = readInfo[header2, "DIMENSIONS"];
  origin = readInfo[header2, "ORIGIN"];
  spacing = readInfo[header2, "SPACING"];
  nPoints = readInfo[header2, "POINT_DATA"][[1]];
  varName = ToString@readInfo[header2, "SCALARS"][[1]];
  max = origin + (dims - 1)*spacing;
  dataRange = MapThread[List, {origin, max}];
  data = BinaryReadList[s, "Real32", nPoints, ByteOrdering -> 1];
  {nx, ny, nz} = dims;
  If[Length[data] < nPoints, data = Append[data, 0.0]];

  zSlices = Partition[data, nx*ny];
  data2 = Map[Partition[#, nx] &, zSlices];
  ToDataRegion[data2, origin, spacing, "VariableName" -> varName]
];

End[];
EndPackage[];
