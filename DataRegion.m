(* ::Package:: *)

(* This package was originally written by Ian Hinder and modified to support arbitrary dimensional data by Barry Wardell *)

BeginPackage["DataRegion`","DataTable`"];

(* Exported symbols *)

MakeDataRegion;
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
ScaledColorFunction;
ColorMapLegend;
QuickSlicePlot;
Outline;
Strip;
MergeDataRegions;
GetCoordinate;
MapDataRegion;
FilterNaNs;
NaNQ;
DataRegionContourPlot;
MapThreadDataRegion;

Iteration;
Variable;
RefinementLevel;

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
  MakeDataTable[Thread[{Range[xmin, xmax, spacing],data}], GetAttributes[v]/.((a_->b_):>(SymbolName[a]->b))]
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

(* Plotting wrappers *)
DataRegionPlot[plotFunction_, plotDims_, v_DataRegion, args___] := Module[{ndims, dataRange},
 ndims = GetNumDimensions[v];
 If[ndims!=plotDims,
   Throw[SymbolName[plotFunction]<>" only supports data with dimensionality "<>ToString[plotDims]<>
     ". The provided data has dimensionality "<>ToString[ndims]<>"."];
 ];

 dataRange =  If[ndims==1, GetDataRange[v][[1]], GetDataRange[v]];

 plotFunction[GetData[v], args, DataRange -> dataRange, Sequence@@If[plotFunction === ArrayPlot, {DataReversed -> True}, {}]]
];

DataRegion1DPlot[plotFunction_, v_DataRegion, args___] := DataRegionPlot[plotFunction, 1, v, args];
DataRegion2DPlot[plotFunction_, v_DataRegion, args___] := DataRegionPlot[plotFunction, 2, v, args];

DataRegionPlot[v_DataRegion, args___]        := DataRegion1DPlot[ListPlot, v, args]; 
DataRegionDensityPlot[v_DataRegion, args___] := DataRegion2DPlot[ListDensityPlot, v, args];
DataRegionArrayPlot[v_DataRegion, args___]   := DataRegion2DPlot[ArrayPlot, v, args];
DataRegionMatrixPlot[v_DataRegion, args___]   := DataRegion2DPlot[MatrixPlot, v, args];
DataRegionPlot3D[v_DataRegion, args___]      := DataRegion2DPlot[ListPlot3D, v, args]; 

DataRegionContourPlot[v_, args___]      := DataRegion2DPlot[ListContourPlot, v, args]; 

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
