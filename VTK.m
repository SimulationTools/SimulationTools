
(* This file was written by Ian Hinder *)

BeginPackage["VTK`"];

ReadVTKFile;
SliceData;
VTKFile;
GetDataRange;
GetOrigin;
GetSpacing;
GetDimensions;
GetData;
GetAttributes;
GetVariableName;
VTKFileDensityPlot;
VTKFileArrayPlot;
ScaledColorFunction;
ColorMapLegend;
QuickSlicePlot;

Begin["`Private`"];

(* ReadVTKFile[fileName_] := *)
(*  Module[{s, header, header2, readInfo, dims, origin, spacing, data,  *)
(*    dataRange, max, nx, ny, nz, data2, newHeader}, *)
(*   If[FileType[fileName] === None, *)
(*     Throw["File "<>ToString[fileName]<>" not found"]]; *)
(*   s = OpenRead[fileName, BinaryFormat -> True]; *)
(*   header = ReadList[s, String, 10]; *)
(*   header2 = Map[StringSplit, header]; *)
(*   readInfo[h_List, key_String] := *)
(*    Module[{result, vals}, *)
(*     result = ToExpression /@ Cases[h, {key, vals__} -> {vals}]; *)
(*     If[Length[result] == 0,  *)
(*      Throw["Cannot find key " <> key <> " in header: " <>  *)
(*        ToString[h]]]; *)
(*     result[[1]]]; *)
(*   dims = readInfo[header2, "DIMENSIONS"]; *)
(*   origin = readInfo[header2, "ORIGIN"]; *)
(*   spacing = readInfo[header2, "SPACING"]; *)
(*   max = origin + (dims - 1)*spacing; *)
(*   dataRange = MapThread[List, {origin, max}]; *)
(*   data = BinaryReadList[s, "Real32", ByteOrdering -> 1]; *)
(*   Close[s]; *)
(*   {nx, ny, nz} = dims; *)
(*   data2 = Partition[Partition[data, nz], ny]; *)
(*   newHeader = {Dimensions -> dims, Origin -> origin,  *)
(*     Spacing -> spacing, DataRange -> dataRange}; *)
(*   Return[VTKFile[newHeader, data2]] *)
(*   ] *)

Format[v_VTKFile] := "VTKFile"[GetVariableName[v], GetDataRange[v]];

ReadVTKFile[fileName_String] :=
  Module[{s,result},
    s = OpenRead[fileName, BinaryFormat->True];
    result = ReadVTKFile[s];
    Close[s];
    result];

ReadVTKFile[s_InputStream] :=
 Module[{header, header2, readInfo, dims, origin, spacing, data, 
   dataRange, max, nx, ny, nz, data2, newHeader, zSlices, varName},
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
  varName = ToString@readInfo[header2, "SCALARS"][[1]];
  max = origin + (dims - 1)*spacing;
  dataRange = MapThread[List, {origin, max}];
  data = BinaryReadList[s, "Real32", nPoints, ByteOrdering -> 1];
  {nx, ny, nz} = dims;
  If[Length[data] < nPoints, data = Append[data, 0.0]];

  zSlices = Partition[data, nx*ny];
  data2 = Map[Partition[#, nx] &, zSlices];
  newHeader = {Dimensions -> dims, Origin -> origin, 
    Spacing -> spacing (*, DataRange -> dataRange *), VariableName -> varName};
  Return[VTKFile[newHeader, data2]]
  ]

GetData[VTKFile[h_, data_]] :=
  data;

GetOrigin[VTKFile[h_, data_]] :=
  Origin /. h;

GetSpacing[VTKFile[h_, data_]] :=
  Spacing /. h;

GetDimensions[VTKFile[h_, data_]] :=
  Dimensions /. h;

GetDataRange[v_VTKFile] :=
  Module[{origin, spacing, dimensions, min, max},
    origin = GetOrigin[v];
    spacing = GetSpacing[v];
    dimensions = GetDimensions[v];
    min = origin;
    max = origin + spacing * (dimensions - {1,1,1});
    MapThread[List, {min, max}]];

GetAttributes[VTKFile[h_, data_]] := h;

GetVariableName[VTKFile[h_, data_]] := VariableName /. h;

replaceRule[list_List, key_ -> newValue_] :=
  list /. (key -> _) :> (key -> newValue);  

replaceRules[list_List, replacements_List] :=
  If[Length[replacements] == 0, list, replaceRules[replaceRule[list, First[replacements]], Drop[replacements, 1]]];

SliceData[v:VTKFile[h_, data_], dim_:3, coord_:0] :=
 Module[{index, newOrigin, newDims, h2},
  If[dim != 3, Throw["Can only slice in the z direction"]];
  origin = GetOrigin[v];
  spacing = GetSpacing[v];
  dims = GetDimensions[v];
  index = 
   Round[(coord - origin[[dim]])/spacing[[dim]]];
  newOrigin = origin;
  newOrigin[[dim]] = coord;
  newDims = dims;
  newDims[[dim]] = 1;
  h2 = replaceRules[h, {Dimensions -> newDims, Origin -> newOrigin}];
  Return[VTKFile[h2, {data[[index]]}]];
  ];

VTKFileDensityPlot[v_VTKFile, args___] :=
 ListDensityPlot[GetData[v][[1]], 
  DataRange -> Take[GetDataRange[v], 2]];

VTKFileArrayPlot[v_VTKFile, args___] :=
 ArrayPlot[GetData[v][[1]], args, 
  DataRange -> Take[GetDataRange[v], 2]];

(* Convenient plotting functions *)

ScaledColorFunction[name_, {min_, max_}] :=
 (ColorData[name][(# - min)/(max - min)] &);

ColorMapLegend[colorFn_, {min_, max_, dx_: Automatic}] :=
 ArrayPlot[
  Table[{c, c}, {c, min, max, 
     (max - min)/100.}], 
  DataRange -> {{0, 0.1}, {min, max}}, ColorFunction -> colorFn, 
  FrameTicks -> {{None, 
     Table[x, {x, min, max, If[dx === Automatic, (max - min)/10., dx]}]}, {None, None}}, ImageSize->{60,300}];

QuickSlicePlot[v_VTKFile, {min_, max_}, colorMap_: "TemperatureMap", opts___] :=
 Module[{cf},
  cf = ScaledColorFunction[colorMap, {min, max}];
  GraphicsGrid[{{
     VTKFileArrayPlot[v, FrameTicks -> True, FrameLabel -> {"y", "x"},
       ColorFunction -> cf, ImageSize->300,opts], ColorMapLegend[cf, {min, max}]}}]];

Unprotect[Interpolation];

Interpolation[v_VTKFile, opts___] :=
  Module[{nz = GetDimensions[v][[3]], data = GetData[v]},
    fn = If[nz == 1, ListInterpolation[data[[1]], Take[Reverse@GetDataRange[v], 2], opts],
                     ListInterpolation[Transpose[data,{3,2,1}], Reverse[GetDataRange[v]]], opts]];

Protect[Interpolation];

End[];

EndPackage[];
