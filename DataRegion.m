(* ::Package:: *)

(* This package was originally written by Ian Hinder and modified to support arbitrary dimensional data by Barry Wardell *)

BeginPackage["DataRegion`","DataTable`"];

(* Exported symbols *)

MakeDataRegion;
ReadVTKFile;
SliceData;
DataRegion;
ToDataTable;
GetDataRange;
GetOrigin;
GetSpacing;
GetDimensions;
GetNumDimensions;
GetData;
GetTime;
GetAttributes;
GetVariableName;
DataRegionDensityPlot;
DataRegionArrayPlot;
DataRegionMatrixPlot;
DataRegionPlot3D;
DataRegionPlot;
CarpetHDF5Manipulate;
ScaledColorFunction;
ColorMapLegend;
QuickSlicePlot;
Outline;
Strip;
MergeDataRegions;
ReadCarpetHDF5;
ClearCarpetHDF5Cache;
CarpetHDF5DatasetName;
ReadCarpetHDF5Variable;
ReadCarpetHDF5Components;
StripGhostZones;
VerboseRead;
PreloadCarpetHDF5Data;
CarpetHDF5Iterations;
CarpetHDF5Components;
CarpetHDF5Maps;
CarpetHDF5RefinementLevels;
CarpetHDF5TimeLevels;
CarpetHDF5Variables;
CarpetHDF5FileInfo;
CarpetManipulatePlotFunction;
GetCoordinate;
MapDataRegion;
FilterNaNs;
NaNQ;
DataRegionContourPlot;
MapThreadDataRegion;

Begin["`Private`"];

(* DataRegion object low-level implementation; this is where we define
   the internal format of a DataRegion object. *)

MakeDataRegion[data_List, name_String, dims_List, origin_List, spacing_List, time_] :=
  DataRegion[{VariableName -> name, Dimensions -> dims, Origin -> origin, Spacing -> spacing, NumDimensions -> Length[dims], Time -> time}, data];

DataRegion /: ToDataTable[v_DataRegion] := Module[{ndims, xmin, xmax, spacing, data},
  ndims = GetNumDimensions[v];
  If[ ndims != 1,
	Throw["Number of dimensions " <> ToString[ndims] <> " in DataRegion '" 
          <> SymbolName[x] <> "' is greater than 1."]
  ];

  {{xmin, xmax}} = GetDataRange[v];
  {spacing} = GetSpacing[v];
  data = GetData[v];
  MakeDataTable[Thread[{Range[xmin, xmax, spacing],data}]]
];


GetData[DataRegion[h_, data_]] :=
  data;

GetOrigin[DataRegion[h_, data_]] :=
  Origin /. h;

GetSpacing[DataRegion[h_, data_]] :=
  Spacing /. h;

GetDimensions[DataRegion[h_, data_]] :=
  Dimensions /. h;

GetNumDimensions[DataRegion[h_, data_]] :=
  NumDimensions /. h;

GetTime[DataRegion[h_, _]] :=
  Time /. h;

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
    "DataRegion[" <>If[StringQ[name], name, "<unknown name>"] <>", "
                  <>If[ListQ[dims], ToString[dims], "<unknown dims>"]  <>", "
                  <>If[ListQ[range], ToString[range, StandardForm], "<unknown range>"]<>"]"];

GetDataRange[v_DataRegion] :=
  Module[{origin, spacing, dimensions, min, max},
    origin = GetOrigin[v];
    spacing = GetSpacing[v];
    dimensions = GetDimensions[v];
    min = origin;
    max = origin + spacing * (dimensions - 1);
    MapThread[List, {min, max}]];

DataRegion/:Min[DataRegion[_,data_]]:=Min[data];

DataRegion/:Max[DataRegion[_,data_]]:=Max[data];

DataRegion/:Times[a_, DataRegion[h_,data_]] :=
  DataRegion[h, a data];

DataRegion/:Abs[DataRegion[h_,data_]] :=
  DataRegion[h, Abs[data]];

DataRegion/:Re[DataRegion[h_,data_]] :=
  DataRegion[h, Re[data]];

DataRegion/:Im[DataRegion[h_,data_]] :=
  DataRegion[h, Im[data]];

DataRegion/:Log10[DataRegion[h_,data_]] :=
  DataRegion[h, Log10[data]];

DataRegion/:Power[DataRegion[h_,data_], a_] :=
  DataRegion[h, data^a];

DataRegion/:Plus[DataRegion[h1_,data1_], DataRegion[h2_,data2_]]:=DataRegion[h1, data1+data2]

DataRegion/:Plus[DataRegion[h1_,data1_], a_]:=DataRegion[h1, data1+a]

DataRegion/:Times[DataRegion[h1_,data1_], DataRegion[h2_,data2_]]:=DataRegion[h1, data1*data2]

DataRegion/:Position[DataRegion[h1_,data1_], pattern_, opts___] := Position[data1, pattern, opts];

DataRegion/:Extract[DataRegion[h1_,data1_], positions_List] := Extract[data1, positions];

replaceRule[list_List, key_ -> newValue_] :=
  list /. (key -> _) :> (key -> newValue);  

replaceRules[list_List, replacements_List] :=
  If[Length[replacements] == 0, 
     list, 
     replaceRules[replaceRule[list, First[replacements]], Drop[replacements, 1]]];

SliceData[v:DataRegion[h_, data_], dim_Integer, coord_:0] :=
 Module[{index, newOrigin, newSpacing, newDims, newNDims, h2, origin, spacing, dims, range, slice, ndims},
  origin = GetOrigin[v];
  spacing = GetSpacing[v];
  dims = GetDimensions[v];
  ndims = GetNumDimensions[v];

  If[dim > ndims,
    Throw["Slicing direction "<>ToString[dim]<>" is greater than dimension "<>
      ToString[ndims]<>" of the DataRegion."]
  ];

  range = GetDataRange[v][[dim]];

  If[(coord < range[[1]]) || (coord >range[[2]]),
    Throw["Slice coordinate "<>ToString[coord]<>" is outside the range "<>
      ToString[range, StandardForm]<>" of the DataRegion."]
  ];

  index = Round[(coord - origin[[dim]])/spacing[[dim]]+1];
  newOrigin = Drop[origin, {dim}];
  newSpacing = Drop[spacing, {dim}];
  newDims = Drop[dims, {dim}];
  newNDims = ndims-1;
  h2 = replaceRules[h, {Dimensions -> newDims, Origin -> newOrigin, NumDimensions -> newNDims, Spacing-> newSpacing}];
  slice = Sequence @@ Reverse[Join[ConstantArray[All,dim-1],{index},ConstantArray[All,ndims-dim]]];
  Return[DataRegion[h2, data[[slice]] ]]
];

SliceData[v_DataRegion, dims_List, coords_:0] := 
  Fold[SliceData[#, Sequence@@#2]&, v, Reverse[SortBy[Thread[{dims, coords}], First]]];

Unprotect[Interpolation];

Interpolation[v_DataRegion, opts___] :=
  Module[{data = GetData[v], fn, ndims = GetNumDimensions[v]},
    ListInterpolation[Transpose[data,Reverse[Range[ndims]]], Reverse[GetDataRange[v]], opts]
];

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
DataRegionPlot[plotFunction_, plotDims_, v_DataRegion, args___] := Module[{ndims, dataRange},
 ndims = GetNumDimensions[v];
 If[ndims!=plotDims,
   Throw[SymbolName[plotFunction]<>" only supports data with dimensionality "<>ToString[plotDims]<>
     ". The provided data has dimensionality "<>ToString[ndims]<>"."];
 ];

 dataRange =  If[ndims==1, GetDataRange[v][[1]], GetDataRange[v]];

 plotFunction[If[plotFunction===ArrayPlot, Reverse, Identity]@GetData[v], args,
  DataRange -> dataRange]
];

DataRegion1DPlot[plotFunction_, v_DataRegion, args___] := DataRegionPlot[plotFunction, 1, v, args];
DataRegion2DPlot[plotFunction_, v_DataRegion, args___] := DataRegionPlot[plotFunction, 2, v, args];

DataRegionPlot[v_DataRegion, args___]        := DataRegion1DPlot[ListPlot, v, args]; 
DataRegionDensityPlot[v_DataRegion, args___] := DataRegion2DPlot[ListDensityPlot, v, args];
DataRegionArrayPlot[v_DataRegion, args___]   := DataRegion2DPlot[ArrayPlot, v, args];
DataRegionMatrixPlot[v_DataRegion, args___]   := DataRegion2DPlot[MatrixPlot, v, args];
DataRegionPlot3D[v_DataRegion, args___]      := DataRegion2DPlot[ListPlot3D, v, args]; 

DataRegionContourPlot[v_, args___]      := DataRegion2DPlot[ListContourPlot, v, args]; 

Options[CarpetHDF5Manipulate] = {CarpetManipulatePlotFunction -> DataRegionDensityPlot};
CarpetHDF5Manipulate[file_, var_String, rl_, map_:None, opts:OptionsPattern[]]:= Module[{data, axesOrigin, numDims, plotType},
  data = Table[ReadCarpetHDF5Variable[file, var, it, rl, map, Sequence@@FilterRules[{opts}, Options[ReadCarpetHDF5Variable]]], {it, CarpetHDF5Iterations[file]}];
  numDims = GetNumDimensions[data[[1]]];

  (*data = SliceData[#, {2,3}]&/@data;*)
  axesOrigin = {Min[GetOrigin/@data], Automatic};

  If[numDims == 1,
    plotType = DataRegionPlot;
  , If[numDims == 2,
    plotType = OptionValue[CarpetManipulatePlotFunction];
  , Throw["CarpetHDF5Manipulate does not support HDF5 data with dimension "<>ToString[numDims]<>"."];
  ]];

  Manipulate[plotType[data[[i]], PlotLabel->"t="<>ToString[GetTime[data[[i]]]] (* ,
                             PlotRange->{Min[data], Max[data]} *), AxesOrigin-> axesOrigin],
             {{i, 1, "Iteration"}, 1, Length[data], 1}]
];

CarpetHDF5Manipulate[file_, opts___]:= Module[{var, rl, maps, map},
  var  = First[Vars /. CarpetHDF5FileInfo[file]];
  rl   = 0;
  maps = Maps /. CarpetHDF5FileInfo[file];
  map  = If[Length[maps]>0, First[maps], None];

  CarpetHDF5Manipulate[file, var, rl, map, opts]
];

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
       ColorFunction -> cf, ImageSize->300,opts], ColorMapLegend[cf, {min, max}]}}]
];


(* Operations on DataRegion objects *)

Outline[d_DataRegion] := Module[{coords, ndims, shapes},
  ndims = GetNumDimensions[d];
  coords = {GetOrigin[d], GetOrigin[d] + GetSpacing[d] * (GetDimensions[d] - 1)};
  If[ndims == 1, coords = {Thread[{ {0,0}, coords[[All,1]] }]}];

  shapes = {Line, Rectangle, Cuboid};

  If[ndims > 3,
    Throw["Dimension "<>ToString[ndims]<>" of DataRegion not supported by Outline."]
  ];

  shapes[[ndims]]@@coords
];

Strip[d_DataRegion, n_Integer] := Module[{ndims},
  ndims = GetNumDimensions[d];
  Strip[d, ConstantArray[n,ndims]]];

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
insertArray[a2_, a1_, s_] :=
 Module[{n1, n2, s2, position},
  n1 = Dimensions[a1];
  n2 = Dimensions[a2];
  s2 = Reverse[s];
  position = Table[s2[[n]]+1;;s2[[n]]+n1[[n]],{n,Length[s]}];
  Part[a2, Sequence @@ position ] = a1];

chunkOffset[d_DataRegion, origin_, spacing_] :=
 Module[{},
  Round[(GetOrigin[d] - origin)/spacing]];

MergeDataRegions[regions_List] :=
 Module[{headers, ndims, origins, dims, spacings, spacing, spacingDiffs,
    X1, X2s, X2, n, dat, header, attrs, attrs2, dat2},
  If[Length[regions] === 0, Return[{}]];

  origins = Map[GetOrigin, regions];
  dims = Map[GetDimensions, regions];
  ndims = GetNumDimensions[regions[[1]]];

  (* Find the lower coordinate of the bounding box *)
  X1 = Min[origins[[All,#]]]&/@ Range[ndims];

  spacings = Map[GetSpacing, regions];
  spacingDiffs = (# - spacings[[1]]) & /@ spacings;

  If[ Max[Norm/@spacingDiffs] > 10^-8,
    Throw["MergeDataRegions: Attempt to merge DataRegions with different spacings: " <> ToString[spacings] <> ", " <> ToString[spacingDiffs]]];
  spacing = First[spacings];
  
  (* Find the upper coordinate of the bounding box *)
  X2s = MapThread[#1 + spacing * (#2 - 1) &, {origins, 
     dims}];
  X2 = Max[X2s[[All,#]]]&/@ Range[ndims];

  n = Round[(X2 - X1)/spacing] + 1;
  dat = ConstantArray[None, Reverse[n]];
  Scan[insertArray[dat, GetData[#], chunkOffset[#, X1, spacing]] &, 
   regions];
  attrs = GetAttributes[regions[[1]]];
  attrs2 = replaceRules[attrs, {Dimensions -> n, Origin -> X1}];
  Return[DataRegion[attrs2, dat]]];

(* Carpet HDF5 functions *)

(* Gather various information about a file *)
carpetHDF5List[file_, item_]        := Sort[First /@ Select[DeleteDuplicates[StringCases[Datasets[file],item]], Length[#] > 0 &]];
carpetHDF5ListN[file_, item_String] := carpetHDF5List[file, item<>"="~~x:DigitCharacter..:>ToExpression[x]];

datasetNamesWith[file_, items_List] :=
  Module[{patterns, datasetsMatching, allMatches, matchAll},
    patterns = ((___ ~~ (# <> " ") ~~ ___) &) /@ items;
    datasetsMatching[p_] :=
      Select[Datasets[file], StringMatchQ[#, p] &];
    allMatches = datasetsMatching /@ patterns;
    matchAll = Intersection @@ allMatches];

compOf[ds_] :=
  StringCases[ds, "c="~~x:DigitCharacter..:>ToExpression[x]][[1]];

itOf[ds_] :=
  StringCases[ds, "it="~~x:DigitCharacter..:>ToExpression[x]][[1]];

componentsWith[file_, items_List] :=
  Sort[compOf /@ datasetNamesWith[file,items]];

(*componentsWith[file_, items_List] :=
  Module[{patterns, datasetsMatching, allMatches, matchAll},
(*    patterns = (#<>"="~~DigitCharacter..) & /@ items;*)
    patterns = ((___ ~~ (# <> " ") ~~ ___) &) /@ items;
    datasetsMatching[p_] :=
      Select[Datasets[file], StringMatchQ[#, p] &];
    allMatches = datasetsMatching /@ patterns;
    matchAll = Intersection @@ allMatches;
    compOf[ds_]:=StringCases[ds, "c="~~x:DigitCharacter..:>ToExpression[x]][[1]];
    Sort[compOf /@ matchAll]];*)

CarpetHDF5Iterations[file_] := carpetHDF5ListN[file, "it"];

CarpetHDF5Iterations[file_, rl_:0] := 
  Union[itOf/@datasetNamesWith[file, {"rl="<>ToString@rl}]];

CarpetHDF5Components[file_] := carpetHDF5ListN[file, "c"];

CarpetHDF5Components[file_, it_, rl_] := 
  componentsWith[file, {"it=" <> ToString[it], "rl=" <> ToString[rl]}];

CarpetHDF5Maps[file_]             := carpetHDF5ListN[file, "m"];
CarpetHDF5RefinementLevels[file_] := carpetHDF5ListN[file, "rl"];
CarpetHDF5TimeLevels[file_]       := carpetHDF5ListN[file, "tl"];
CarpetHDF5Variables[file_]        := carpetHDF5List [file, RegularExpression["^/([^ ]+) it="]:>"$1"];

CarpetHDF5FileInfo[file_]:=
  Module[{iterations, components, maps, refLevels, timeLevels, vars},
  iterations = CarpetHDF5Iterations[file];

  components = CarpetHDF5Components[file];

  maps = CarpetHDF5Maps[file];

  refLevels = CarpetHDF5RefinementLevels[file];

  timeLevels = CarpetHDF5TimeLevels[file];

  vars = CarpetHDF5Variables[file];

  {Iterations->iterations, Components->components, Maps->maps, RefinemeneLevels->refLevels, TimeLevels->timeLevels, Vars->vars}
];

CarpetHDF5DatasetName[var_String, it_Integer, m:(_Integer|None), rl_Integer, c:(_Integer|None)] :=Module[{map="", component=""},
  If[m =!= None, map=" m="<>ToString[m]];
  If[c =!= None, component=" c="<>ToString[c]];
  "/" <> var <> " it=" <> ToString[it] <> " tl=0"<>map<>" rl=" <> ToString[rl] <> component];

Options[ReadCarpetHDF5] = {StripGhostZones -> True, VerboseRead -> False};

Datasets[file_]:= Datasets[file] =
  Module[{result = Import[file, "Datasets"]},
    If[ListQ[result],
      result,
      Throw["Could not open HDF5 file " <> file]]];

Annotations[file_]:= Annotations[file] = Import[file, "Annotations"];
Dims[file_]:= Dims[file] = Import[file, "Dimensions"];
HDF5Data[file_, dataset:(_String|_Integer)]:= HDF5Data[file, dataset] = Import[file, {"Datasets", dataset}];

PreloadCarpetHDF5Data[file_]:= Module[{allData, data},
  allData = Import[file, "Rules"];
  data = "Data"/. allData;

  Datasets[file] = "Datasets" /. allData;
  Annotations[file] = "Annotations" /. allData;
  Dims[file] = "Dimensions" /. allData;
  (HDF5Data[file, #] = data[[#]])& /@ Range[Length[data]];
];

ReadCarpetHDF5[file_String, ds_, OptionsPattern[]] :=
 Module[{data, annots, dims, origin, spacing, name, idx, strip, verbose, reg, ghosts, posns, allds, time},
  strip = OptionValue[StripGhostZones];
  verbose = OptionValue[VerboseRead];

  If[!StringQ[ds] && !IntegerQ[ds],
    Throw["ReadCarpetHDF5: expected a string or integer dataset specification, but instead got " <>ToString[ds]]];

  If[verbose, Print["Reading File: "<>file]];

  If[IntegerQ[ds], idx = ds,
    If[verbose, Print["Reading Dataset Names"]];
    allds = Datasets[file];
    posns = Position[allds, ds];
    If[posns === {}, Throw["Cannot find dataset " <> ds <> " in HDF5 file " <> file]];
    idx = posns[[1]][[1]]
  ];

  If[verbose, Print["Reading Data"]];
  data = HDF5Data[file, idx];

  If[verbose, Print["Reading Annotations"]];   
  annots = Annotations[file][[idx]];

  If[verbose, Print["Reading Dimensions"]];
  dims = Reverse[Dims[file][[idx]]];

  origin = "origin" /. annots;
  spacing = "delta" /. annots;
  name = "name" /. annots;

  ghosts = "cctk_nghostzones" /. annots;
  If[ghosts=="cctk_nghostzones", ghosts=0];

  time = "time" /. annots;

  reg = MakeDataRegion[data, name, dims, origin, spacing, time];
  If[strip, Strip[reg, ghosts], reg]
];

ClearCarpetHDF5Cache[] :=
  Module[{},
    ClearAll[Datasets, Annotations, Dims, HDF5Data];
    Datasets[file_]:= Datasets[file] = Import[file, "Datasets"];
    Annotations[file_]:= Annotations[file] = Import[file, "Annotations"];
    Dims[file_]:= Dims[file] = Import[file, "Dimensions"];
    HDF5Data[file_, dataset:(_String|_Integer)]:= HDF5Data[file, dataset] = Import[file, {"Datasets", dataset}]];

Options[ReadCarpetHDF5Components] = {StripGhostZones -> True};

ReadCarpetHDF5Components[file_, var_, it_, rl_, map_, opts___] :=
  Module[{filePrefix, fileNames, datasets, pattern, MultiFile, Filetype1D, Filetype2D, components},
    If[FileType[file] === None,
      Throw["File " <> file <> " not found in ReadCarpetHDF5Components"]];
 
    (* TODO: Add support for 2D. *)	
    Filetype1D = RegularExpression["\\.[dxyz]\\.h5"];
    Filetype2D = RegularExpression["\\.[xyz]{2}\\.h5"];
    MultiFile = RegularExpression["file_\\d+\\.h5"];
    If[StringCount[file, Filetype1D]>0 || StringCount[file, Filetype2D]>0,
		components = CarpetHDF5Components[file, it, rl];
		If[Length[components]==0,
			datasets = {ReadCarpetHDF5[file, CarpetHDF5DatasetName[var, it, map, rl, None], opts]};
		,
			datasets = Table[ReadCarpetHDF5[file, CarpetHDF5DatasetName[var, it, map, rl, c], opts], {c,components}];
		];
    , If[StringCount[file, MultiFile]>0,
		filePrefix = StringReplace[file, ".file_0.h5" -> ""];
		pattern = StringReplace[file, ".file_0.h5" -> ".file_*.h5"];
		fileNames = FileNames[FileNameTake[pattern], DirectoryName[pattern]];
		components = Flatten[DeleteDuplicates[StringCases[fileNames,"file_"~~x:DigitCharacter..:>ToExpression[x]]]];
		datasets = Table[ReadCarpetHDF5[filePrefix<>".file_"<>ToString[c]<>".h5",
			CarpetHDF5DatasetName[var, it, map, rl, c], opts], {c,components}];
      , datasets={ReadCarpetHDF5[file,
			CarpetHDF5DatasetName[var, it, map, rl, None], opts]};
      ]
    ];

	Return[datasets]
];

ReadCarpetHDF5Variable[file_, var_, it_, rl_, map_:None, opts___]:=
  MergeDataRegions[ReadCarpetHDF5Components[file, var, it, rl, map, opts]];

(* Fiendishly clever code *)
GetCoordinate[d_DataRegion, dim_] :=
  Module[{origin, spacing, dims, max, ca, dm, dp, res},
    origin = GetOrigin[d];
    spacing = GetSpacing[d];
    dims = GetDimensions[d];
    max = origin + spacing * (dims - 1);

    ca[x_, {}] := x;
    ca[x_, l_] := ConstantArray[x, l];

    dm = Reverse@Table[dims[[i]], {i, 1, dim - 1}];
    dp = Reverse@Table[dims[[i]], {i, dim + 1, Length[dims]}];

    res = ca[Table[ca[x, dm], {x, origin[[dim]], max[[dim]], spacing[[dim]]}], dp];

    If[Reverse@Dimensions[res] =!= dims, Throw["GetCoordinateError"]];
    DataRegion[GetAttributes[d], res]
  ];

MapDataRegion[f_, d_DataRegion] :=
 Module[{dim, header, data},
  dim = GetNumDimensions[d];
  header = GetAttributes[d];
  data = Map[f, GetData[d], {dim}];
  DataRegion[header, data]];

FilterNaNs[d_DataRegion] :=
 MapDataRegion[If[NaNQ[#], Missing[], #] &, d];

NaNQ[x_] :=
 Round[x] == -2147483648;

MapThreadDataRegion[f_, drs_] :=
  Module[{attrs, datas, newData},
    attrs = GetAttributes[First[drs]];
    datas = GetData /@ drs;
    newData = MapThread[f, datas, GetNumDimensions[First[drs]]];
    DataRegion[attrs, newData]];

End[];

EndPackage[];
