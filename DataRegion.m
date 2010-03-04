
(* This package was written by Ian Hinder *)

BeginPackage["DataRegion`"];

(* Exported symbols *)

MakeDataRegion;
ReadVTKFile;
SliceData;
DataRegion;
GetDataRange;
GetOrigin;
GetSpacing;
GetDimensions;
GetData;
GetAttributes;
GetVariableName;
DataRegionDensityPlot;
DataRegionArrayPlot;
ScaledColorFunction;
ColorMapLegend;
QuickSlicePlot;
Outline;
Strip;
MergeDataRegions;
MergeDataRegions2;
ReadCarpetHDF5;
CarpetHDF5DatasetName;
ReadCarpetHDF5Variable;
ReadCarpetHDF5Components;
StripGhostZones;

Begin["`Private`"];

(* DataRegion object low-level implementation; this is where we define
   the internal format of a DataRegion object. *)

MakeDataRegion[data_List, name_String, dims_List, origin_List, spacing_List] :=
  DataRegion[{VariableName -> name, Dimensions -> dims, Origin -> origin, Spacing -> spacing}, data];

GetData[DataRegion[h_, data_]] :=
  data;

GetOrigin[DataRegion[h_, data_]] :=
  Origin /. h;

GetSpacing[DataRegion[h_, data_]] :=
  Spacing /. h;

GetDimensions[DataRegion[h_, data_]] :=
  Dimensions /. h;

GetAttributes[DataRegion[h_, data_]] := 
  h;

GetVariableName[DataRegion[h_, data_]] := 
  VariableName /. h;

(* DataRegion high-level interface; no assumptions should be made
   about the structure of a DataRegion object here. All access should
   be by the preceding functions.*)

(*Format[DataRegion[{}, data_List]] :=
  "DataRegion"[name, dims, range]; *)

Format[v_DataRegion, StandardForm] :=
  Module[{name,dims,range},
    name = GetVariableName[v];
    dims = GetDimensions[v];
    range = GetDataRange[v];
    "DataRegion[" <>ToString[If[StringQ[name], name, "<unknown name>"]] <>", "
                 <>ToString[If[ListQ[dims],ToString[dims], "<unknown dims>"]]  <>", "
                 <>ToString[If[ListQ[range],ToString[range],"<unknown range"]]];

GetDataRange[v_DataRegion] :=
  Module[{origin, spacing, dimensions, min, max},
    origin = GetOrigin[v];
    spacing = GetSpacing[v];
    dimensions = GetDimensions[v];
    min = origin;
    max = origin + spacing * (dimensions - {1,1,1});
    MapThread[List, {min, max}]];

replaceRule[list_List, key_ -> newValue_] :=
  list /. (key -> _) :> (key -> newValue);  

replaceRules[list_List, replacements_List] :=
  If[Length[replacements] == 0, 
     list, 
     replaceRules[replaceRule[list, First[replacements]], Drop[replacements, 1]]];

(* This function can be replaced by more sophisticated use of Part *)
SliceData[v:DataRegion[h_, data_], dim_:3, coord_:0] :=
 Module[{index, newOrigin, newDims, h2},
  If[dim != 3, Throw["Can only slice in the z direction"]];
  origin = GetOrigin[v];
  spacing = GetSpacing[v];
  dims = GetDimensions[v];
  index = Round[(coord - origin[[dim]])/spacing[[dim]]];
  newOrigin = origin;
  newOrigin[[dim]] = coord;
  newDims = dims;
  newDims[[dim]] = 1;
  h2 = replaceRules[h, {Dimensions -> newDims, Origin -> newOrigin}];
  Return[DataRegion[h2, {data[[index]]}]]];

Unprotect[Interpolation];

Interpolation[v_DataRegion, opts___] :=
  Module[{nz = GetDimensions[v][[3]], data = GetData[v]},
    fn = If[nz == 1, ListInterpolation[data[[1]], Take[Reverse@GetDataRange[v], 2], opts],
                     ListInterpolation[Transpose[data,{3,2,1}], Reverse[GetDataRange[v]]], opts]];

Protect[Interpolation];

(* VTK file reader *)

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
    Spacing -> spacing, VariableName -> varName};
  Return[DataRegion[newHeader, data2]]];

(* Plotting wrappers *)

DataRegionDensityPlot[v_DataRegion, args___] :=
 ListDensityPlot[GetData[v][[1]], 
  DataRange -> Take[GetDataRange[v], 2]];

DataRegionArrayPlot[v_DataRegion, args___] :=
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

QuickSlicePlot[v_DataRegion, {min_, max_}, colorMap_: "TemperatureMap", opts___] :=
 Module[{cf},
  cf = ScaledColorFunction[colorMap, {min, max}];
  GraphicsGrid[{{
     DataRegionArrayPlot[v, FrameTicks -> True, FrameLabel -> {"y", "x"},
       ColorFunction -> cf, ImageSize->300,opts], ColorMapLegend[cf, {min, max}]}}]];

(* Operations on DataRegion objects *)

Outline[d_DataRegion] :=
  Cuboid[GetOrigin[d], GetOrigin[d] + GetSpacing[d] * (GetDimensions[d] - {1,1,1})];

Strip[d_DataRegion, n_Integer] :=
  Strip[d, {n,n,n}];

Strip[d_DataRegion, n_List] :=
  Module[{data, data2, attrs, attrs2, d2},
    data = GetData[d];
    data2 = Take[data, Apply[Sequence, Map[{#+1,-(#+1)}&, Reverse[n]]]];
    attrs = GetAttributes[d];
    attrs2 = replaceRules[attrs, 
      {Origin -> (GetOrigin[d] + n * GetSpacing[d]), 
       Dimensions -> (GetDimensions[d] - 2n)}];
    d2 = DataRegion[attrs2, data2]];

Attributes[insertArray] = {HoldFirst};

(* Insert a1 into a2, offsetting the indices by s *)
insertArray[a2_, a1_, s : {sx_, sy_, sz_}] :=
 Module[{n1x, n1y, n1z, n2z, n2y, n2x},
  {n1z, n1y, n1x} = Dimensions[a1];
  {n2z, n2y, n2x} = Dimensions[a2];
  a2[[sz+1 ;; sz + n1z, sy+1 ;; sy + n1y, sx+1 ;; sx + n1x]] = a1];

chunkOffset[d_DataRegion, origin_, spacing_] :=
 Module[{},
  Round[(GetOrigin[d] - origin)/spacing]];

MergeDataRegions[regions_List] :=
 Module[{headers, origins, dims, x1, y1, z1, spacings, spacing, x2s, x2, y2, z2,
    X1, X2, n, dat, header, attrs, attrs2, dat2},
  origins = Map[GetOrigin, regions];
  dims = Map[GetDimensions, regions];
  x1 = Min[Map[#[[1]] &, origins]];
  y1 = Min[Map[#[[2]] &, origins]];
  z1 = Min[Map[#[[3]] &, origins]];
  spacings = Map[GetSpacing, regions];
  If[!Apply[Equal, spacings], 
    Throw["MergeDataRegions: Attempt to merge DataRegions with different spacings"]];
  spacing = First[spacings];
  x2s = MapThread[#1 + spacing * (#2 - 1) &, {origins, 
     dims}];
  x2 = Max[Map[#[[1]] &, x2s]];
  y2 = Max[Map[#[[2]] &, x2s]];
  z2 = Max[Map[#[[3]] &, x2s]];
  X1 = {x1, y1, z1};
  X2 = {x2, y2, z2};
  n = Round[(X2 - X1)/spacing] + 1;
  dat = ConstantArray[None, Reverse[n]];
  Scan[insertArray[dat, GetData[#], chunkOffset[#, X1, spacing]] &, 
   regions];
  attrs = GetAttributes[regions[[1]]];
  attrs2 = replaceRules[attrs, {Dimensions -> n, Origin -> X1}];
  Return[DataRegion[attrs2, dat]]];

(* Carpet HDF5 functions *)

CarpetHDF5DatasetName[var_String, it_Integer, rl_Integer, c_Integer] :=
  "/" <> var <> " it=" <> ToString[it] <> " tl=0 rl=" <> ToString[rl] <> " c=" <> ToString[c];

Options[ReadCarpetHDF5] = {StripGhostZones -> True};

ReadCarpetHDF5[file_String, ds_, OptionsPattern[]] :=
 Module[{data, annots, dims, origin, spacing, name, idx, strip, reg, ghosts, posns},
  strip = OptionValue[StripGhostZones];
  If[!StringQ[ds] && !IntegerQ[ds],
    Throw["ReadCarpetHDF5: expected a string or integer dataset specification, but instead got " <>ToString[ds]]];

  If[IntegerQ[ds], idx = ds,
    allds = Import[file, "Datasets"]; (* Should at least cache this *)
    posns = Position[allds, ds];
    If[posns === {}, Throw["Cannot find dataset " <> ds <> " in HDF5 file " <> file]];
    idx = posns[[1]][[1]]];
  data = Import[file, {"Datasets", idx}];
  annots = Import[file, {"Annotations", idx}];
  dims = Reverse@Import[file, {"Dimensions", idx}];
  origin = "origin" /. annots;
  spacing = "delta" /. annots;
  name = "name" /. annots;
  ghosts = "cctk_nghostzones" /. annots;
  reg = MakeDataRegion[data, name, dims, origin, spacing];
  If[strip, Strip[reg, ghosts], reg]];

Options[ReadCarpetHDF5Components] = {StripGhostZones -> True};

ReadCarpetHDF5Components[file_, var_, it_, rl_, OptionsPattern[]] :=
  Module[{filePrefix, fileNames, n, datasets, pattern, strip},
    strip = OptionValue[StripGhostZones];
    If[FileType[file] === None,
      Throw["File " <> file <> " not found in ReadCarpetHDF5Components"]];
    filePrefix = StringReplace[file, ".file_0.h5" -> ""];
    pattern = StringReplace[file, ".file_0.h5" -> ".file_*.h5"];
    fileNames = FileNames[FileNameTake[pattern, -1], FileNameDrop[pattern, -1]];
    (* Should check here that the found files are a complete set *)
    n = Length[fileNames];
    datasets = Table[ReadCarpetHDF5[
      filePrefix<>".file_"<>ToString[c]<>".h5",
      CarpetHDF5DatasetName[var, it, rl, c], StripGhostZones -> strip], 
      {c,0,n-1}];
    datasets];

ReadCarpetHDF5Variable[file_, var_, it_, rl_] :=
  MergeDataRegions[ReadCarpetHDF5Components[file, var, it, rl]];

End[];

EndPackage[];
