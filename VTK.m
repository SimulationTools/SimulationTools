
BeginPackage["VTK`"];

ReadVTKFile;
SliceData;
VTKFile;

Begin["`Private`"];

ReadVTKFile[fileName_] :=
 Module[{s, header, header2, readInfo, dims, origin, spacing, data, 
   dataRange, max, nx, ny, nz, data2, newHeader},
  If[FileType[fileName] === None,
    Throw["File "<>ToString[fileName]<>" not found"]];
  s = OpenRead[fileName, BinaryFormat -> True];
  header = ReadList[s, String, 10];
  header2 = Map[StringSplit, header];
  readInfo[h_List, key_String] :=
   Module[{result, vals},
    result = ToExpression /@ Cases[h, {key, vals__} -> {vals}];
    If[Length[result] == 0, 
     Throw["Cannot find key " <> key <> " in header: " <> 
       ToString[h]]];
    result[[1]]];
  dims = readInfo[header2, "DIMENSIONS"];
  origin = readInfo[header2, "ORIGIN"];
  spacing = readInfo[header2, "SPACING"];
  max = origin + (dims - 1)*spacing;
  dataRange = MapThread[List, {origin, max}];
  data = BinaryReadList[s, "Real32", ByteOrdering -> 1];
  Close[s];
  {nx, ny, nz} = dims;
  data2 = Partition[Partition[data, nz], ny];
  newHeader = {Dimensions -> dims, Origin -> origin, 
    Spacing -> spacing, DataRange -> dataRange};
  Return[VTKFile[newHeader, data2]]
  ]

ReadVTKFile[s_InputStream] :=
 Module[{header, header2, readInfo, dims, origin, spacing, data, 
   dataRange, max, nx, ny, nz, data2, newHeader, zSlices},
  header = ReadList[s, String, 10];
  If[Length[header] == 0,
    Return[EndOfFile]];
  If[Length[header] != 10,
    Throw["Did not read complete header from VTK stream"]];
  header2 = Map[StringSplit, header];
  readInfo[h_List, key_String] :=
   Module[{result, vals},
    result = ToExpression /@ Cases[h, {key, vals__} -> {vals}];
    If[Length[result] == 0, 
     Throw["Cannot find key " <> key <> " in header: " <> 
       ToString[h]]];
    result[[1]]];
  dims = readInfo[header2, "DIMENSIONS"];
  origin = readInfo[header2, "ORIGIN"];
  spacing = readInfo[header2, "SPACING"];
  nPoints = readInfo[header2, "POINT_DATA"][[1]];
  max = origin + (dims - 1)*spacing;
  dataRange = MapThread[List, {origin, max}];
  data = BinaryReadList[s, "Real64", nPoints, ByteOrdering -> -1];
  {nx, ny, nz} = dims;
  zSlices = Partition[data, nx*ny];
  data2 = Map[Partition[#, nx] &, zSlices];
  newHeader = {Dimensions -> dims, Origin -> origin, 
    Spacing -> spacing, DataRange -> dataRange};
  Return[VTKFile[newHeader, data2]]
  ]



SliceData[VTKFile[h_, data_], dim_, coord_] :=
 Module[{index},
  If[dim != 3, Throw["Can only slice in the z direction"]];
  index = 
   Round[(coord - (Origin /. h)[[dim]])/(Spacing /. h)[[dim]]];
  Return[data[[index]]];
  ];

End[];

EndPackage[];
