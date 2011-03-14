(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["CarpetVTK`", {"DataRegion`"}];

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
    Throw["Did not read complete header from VTK stream"]];
  header2 = Map[StringSplit, header];
  readInfo[h_List, key_String] :=
   Module[{result},
    result = ToExpression /@ Cases[h, {key, vals__} -> {vals}];
    If[Length[result] == 0, 
     Throw["Cannot find key " <> key <> " in header: " <> 
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
  MakeDataRegion[data2, varName, dims, origin, spacing, None]
];

End[];
EndPackage[];
