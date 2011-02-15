(* ::Package:: *)

(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["DataRegion`","DataTable`", "Profile`"];

(* Exported symbols *)

MakeDataRegion;
SliceData::usage = "SliceData[d, dim, coord] slices the DataRegion d through the dimension dim at the coordinate location coord. The result is a DataRegion with dimensionality 1 lower than that of d. If coord is not given, it uses a default value of 0.";
DataRegionPart::usage = "DataRegionPart[d, {a;;b, c;;d, ...}] gives the part of d which lies between the coordinates a;;b, c;;d, etc.";
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
NDerivative::usage = "NDerivative[d, dir] returns the first derivative of the DataRegion d along the direction dir.  This is uses second order accurate centered differencing and the result omits the first and last points of d.";
TimeDerivative::usage = "TimeDerivative[{d1, d2,...}, center] returns a numerical time derivative computed from DataRegions d1, d2, ... . The derivative is computed using finite differencing, the order of which is determined by the number of DataRegions given. The optional center argument specifies the number of timesteps from the first DataRegion at which to compute derivatives (using lop-sided differencing, if necessary), with the default value being half-way between the first and last times.";


Iteration;
Variable;
RefinementLevel;
ResampleDataRegion;
ResampleDataRegions;
Global`Sub;

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

DataRegion /: Part[d_DataRegion, s__] := DataRegionPart[d, {s}];

DataRegionPart[d:DataRegion[h_, data_], s_]:=
 Module[{ndims, spacing, origin, dataRange, newS, indexrange, newOrigin, h2, newData},
  ndims = GetNumDimensions[d];
  spacing = GetSpacing[d];
  origin = GetOrigin[d];
  dataRange = GetDataRange[d];

  If[Length[s] != ndims,
    Throw["Range "<>ToString[s]<>" does not match the dimensionality of the DataRegion."]
  ];

  (* Convert all to a range *)
  newS = MapThread[#1/.#2&, {s, Thread[All -> dataRange]}];

  (* Clip the rang to the lower and upper bound *)
  newS = MapThread[{Max[#1[[1]],#2[[1]]], Min[#1[[2]],#2[[2]]]}&, {dataRange, newS}];

  (* Convert coordinate range to index range *)
  indexrange = Round[(Apply[List,newS,1]-origin)/spacing]+1;

  (* Get the relevant part of the data *)
  newData = Part[data, Sequence@@Reverse[Apply[Span,indexrange,1]]];

  (* Get new origin *)
  newOrigin = origin + spacing (indexrange[[All,1]]-1);

  h2 = replaceRules[h, {Dimensions -> Reverse[Dimensions[newData]], Origin -> newOrigin}];

  DataRegion[h2, newData]
];

DataRegionPart[d:DataRegion[h_, data_], s_Span] := DataRegionPart[d, {s}];

(* Plotting wrappers *)
DataRegionPlot[plotFunction_, plotDims_, v_DataRegion, args___] := Module[{ndims, dataRange, data},
 ndims = GetNumDimensions[v];
 If[ndims!=plotDims,
   Throw[SymbolName[plotFunction]<>" only supports data with dimensionality "<>ToString[plotDims]<>
     ". The provided data has dimensionality "<>ToString[ndims]<>"."];
 ];

 dataRange =  If[ndims==1, GetDataRange[v][[1]], GetDataRange[v]];

 data = GetData[v];
 opts = {args};

 If[plotFunction === ArrayPlot, 
    data = Reverse[data];
    opts = opts /. (FrameLabel -> {x_,y_}) :> (FrameLabel -> {y,x})];

 plotFunction[data, Sequence@@opts, DataRange -> dataRange]
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

Strip[d_DataRegion, n_List, m_List] :=
  Module[{data, data2, attrs, attrs2, d2},
    data = GetData[d];
	 data2 = Take[data, Apply[Sequence, MapThread[{#1+1,-(#2+1)}&, {Reverse[n], Reverse[m]}]]];
    attrs = GetAttributes[d];
    attrs2 = replaceRules[attrs,
      {Origin -> (GetOrigin[d] + n * GetSpacing[d]),
       Dimensions -> (GetDimensions[d] - n - m)}];
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
 Profile["MergeDataRegions",
 Module[{headers, ndims, origins, dims, spacings, spacing, spacingDiffs,
    X1, X2s, X2, n, dat, header, attrs, attrs2, dat2},
  If[Length[regions] === 0, Return[{}]];

  If[!And@@Map[MatchQ[#, _DataRegion] &, regions],
    Throw["MergeDataRegions: Expected a list of DataRegion objects but got instead " <> ToString[regions]]];
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
  Return[DataRegion[attrs2, dat]]]];

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

ResampleDataRegion[d_DataRegion, {x1_List, x2_List, dx_List}, p_] :=
  Module[{dFn, newData},
    dFn = Interpolation[d, InterpolationOrder->p];
    If[GetNumDimensions[d] === 2,
      newData = Table[dFn[x,y], {x, x1[[1]], x2[[1]], dx[[1]]},
                                {y, x1[[2]], x2[[2]], dx[[2]]}];
      MakeDataRegion[newData, GetVariableName[d], Dimensions[newData], 
        x1, dx, GetTime[d]],

      Throw["Cannot resample DataRegions of dimension other than 2"]]
  ];

intersectBoxes[boxes_] :=
  (* A "box" is a list of the form {x1, x2} where x1 and x2 are vectors
     for the lower and upper coordinates of the box. *)
  Module[{x1s, x2s, x1, x2},
    x1s = Map[First, boxes];
    x2s = Map[Last, boxes];
    x1 = Map[Max, Transpose[x1s]];
    x2 = Map[Min, Transpose[x2s]];

    (* Could return a special value here if needed.  Alternatively,
       could generically return a list of boxes, which would be empty
       if the boxes don't intersect. *)
    If[Negative[x2 - x1], Throw["Intersection of boxes is empty"]];
    {x1, x2}];

ResampleDataRegions[ds:{DataRegion[__]...}, p_:3] :=
  Module[{dxs,ranges,x1,x2,dx},
    If[Length[ds] === 0, Return[{}]];
    dxs = Map[GetSpacing, ds];
    ranges = Map[Transpose[GetDataRange[#]] &, ds];
    {x1, x2} = intersectBoxes[ranges];
    dx = Map[Min, Transpose[dxs]];
    Map[ResampleDataRegion[#, {x1, x2, dx}, p] &, ds]];

DataRegion/:
Global`Sub[d1_DataRegion, d2_DataRegion,p_:3] :=
  Apply[Subtract, ResampleDataRegions[{d1, d2},p]];

(*******************************************************************************************)
(* Redefine various built-in Mathematica functions to work on DataRegions                  *)
(*******************************************************************************************)

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

DataRegion /: Interpolation[v_DataRegion, opts___] :=
  Module[{data = GetData[v], fn, ndims = GetNumDimensions[v]},
    ListInterpolation[Transpose[data,Reverse[Range[ndims]]], GetDataRange[v], opts]
];

NDerivative[d_DataRegion] :=
 Module[{ndims, result},
  ndims = GetNumDimensions[d];

  If[ndims == 1,
   result = NDerivative[d, 1];,
   Throw["Must specify a direction when applying NDerivative to a DataRegion of dimension greater than 1."];
   result = $Failed;
  ];

  result
];

UFDWeights[m_, n_, s_, h_] := 
 CoefficientList[Normal[Series[x^s Log[x]^m, {x, 1, n}]/h^m], x]

NDerivative[d:DataRegion[h_,_], dir_Integer] :=
 Module[{spacing, ndims, data, dr1, dr2, newh, deriv, lowerdims, upperdims, leftpart, rightpart},
  ndims   = GetNumDimensions[d];
  spacing = GetSpacing[d][[ndims-dir+1]];

  data = GetData[d];
  dr1 = Map[RotateLeft, data, {ndims-dir}];
  dr2 = Map[RotateRight, data, {ndims-dir}];
  deriv = (dr1-dr2)/(2 spacing);


  (* Build up sequences for use in Part corresponding to the boundaries *)
  If[ndims-dir == 0, lowerdims = Sequence[], lowerdims = Sequence @@ ConstantArray[All, ndims-dir]];
  If[dir == 1, upperdims = Sequence[], upperdims = Sequence @@ ConstantArray[All, dir-1]];
  leftpart = Sequence[lowerdims, 1, upperdims];
  rightpart = Sequence[lowerdims, -1, upperdims];

  (* Deal with boundary points using asymmetric stencil *)
  deriv[[leftpart]] = (dr1[[leftpart]]-data[[leftpart]])/spacing;
  deriv[[rightpart]] = (data[[rightpart]]-dr2[[rightpart]])/spacing;

  newh = replaceRules[h, {VariableName -> "dx"<>ToString[dir]<>"_"<>GetVariableName[d]}];

  DataRegion[newh, deriv]
]

TimeDerivative[dr:{__DataRegion}, centering_:Automatic] :=
 Module[{nd, dims, spacing, origin, variable, sorted, times, dt, stencil, offset, deriv, attr, newTime},
  nd = GetNumDimensions/@dr;
  If[Not[Equal@@nd],
   Throw["Error, can't compute time derivative from DataRegions with a different number of dimensions."];
   Return[$Failed];
  ];

  dims = GetDimensions/@dr;
  If[Not[Equal@@dims],
   Throw["Error, can't compute time derivative from DataRegions with different dimensions."];
   Return[$Failed];
  ];

  spacing = GetSpacing/@dr;
  If[Not[Equal@@spacing],
    Throw["Error, can't compute time derivative from DataRegions with different spacings."];
    Return[$Failed];
  ];

  origin = GetSpacing/@dr;
  If[Not[Equal@@origin],
    Throw["Error, can't compute time derivative from DataRegions with different origins."];
    Return[$Failed];
  ];

  variable = GetVariableName/@dr;
  If[Not[Equal@@variable],
    Throw["Error, can't compute time derivative from DataRegions with different variables."];
    Return[$Failed];
  ];

  (* Sort DataRegions by time *)
  sorted = SortBy[dr, GetTime];

  (* Check spacing is uniform *)
  times = GetTime/@sorted;
  If[Apply[Or, Map[# > 10^-10 &, Abs[1 - Differences[times]/Differences[times][[1]]]]],
    Throw["Error, can't compute time derivative from DataRegions with non-uniform spacing in time."];
    Return[$Failed];
  ];

  (* Get finite differencing stencil *)
  If[centering === Automatic, offset = (Length[dr]-1)/2, offset = centering];
  dt = times[[2]] - times[[1]];
  stencil = UFDWeights[1, Length[dr]-1, offset, dt];
  
  (* Compute derivative using finite differences *)
  If[Length[stencil]!=Length[sorted],
    Throw["Error, can't compute time derivative from inconsistent stencil and DataRegions."];
    Return[$Failed];
  ];
  deriv = stencil.sorted;

  (* Correct time and variable name attributes *)
  newTime = times[[1]]+offset*dt;
  attr=replaceRules[GetAttributes[deriv], {Time-> newTime, VariableName -> "dt_"<>variable[[1]]}];

  DataRegion[ attr, GetData[deriv]]
];

(* We cannot use upvalues here, as the DataRegion appears too deep in
the expression *)

Unprotect[ListLinePlot];

ListLinePlot[ds:List[DataRegion[___]..], opts___] :=
  ListLinePlot[ToList /@ ToDataTable /@ ds, opts];

ListLinePlot[d:DataRegion[___], opts___] :=
   ListLinePlot[{d}, opts];

Protect[ListLinePlot];


End[];

EndPackage[];
