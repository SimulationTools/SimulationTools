(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["DataRegion`",
 {
  "DataRepresentations`",
  "DataTable`",
  "Error`",
  "NR`",
  "Profile`"
 }];

DataRegion::usage = "DataRegion[...] is a representation of an N-dimensional array of numbers on a regular grid.";
ToDataRegion::usage = "ToDataRegion[data, origin, spacing] creates a DataRegion object from the N-dimensional array (nested list) data.";

VariableName::usage = "VariableName[d] returns the variable name in DataRegion d.";

SameGridQ::usage = "SameGridQ[d1, d2] returns True if d1 and d2 are DataRegions defined on the same grid (origin, spacing, size).";

Slab::usage = "Slab[d, {{x1min, x1max}, ...}] gives the hyperslab of specified by the coordinates the coordinates {x1min, x1max}, ....";

CoordinateOutline::usage = "CoordinateOutline[d] generates a graphical representation of the outline of d";
Coordinate::usage = "Coordinate[d, i] returns a DataRegion of the same shape as d whose data is the i coordinate of d.";

(* TODO: Add Metadata function and user-defined metadata *)

(****************************************************************)
(* Experimental                                                 *)
(****************************************************************)

TimeDerivative;

(****************************************************************)
(* Deprecated *)
(****************************************************************)

MakeDataRegion;
SliceData;
GetData;
GetAttributes;
GetDimensions;
GetNumDimensions;
GetDataRange;
GetOrigin;
GetSpacing;
GetTime;
GetVariableName;
DataRegionPart;
DataRegionDensityPlot;
DataRegionArrayPlot;
DynamicDataRegionArrayPlot;
DataRegionMatrixPlot;
DataRegionPlot3D;
DataRegionPlot;
DataRegionContourPlot;
NormL2;
Outline;
Strip;
TableToDataRegion;
EvaluateOnDataRegion;
MergeDataRegions;
MapDataRegion;
MapThreadDataRegion;
GetCoordinate;
ResampleDataRegion;
ResampleDataRegions;


Begin["`Private`"];

(******************************************************************************)
(* DataRegion object low-level implementation. This is where we define        *)
(* the internal format of a DataRegion object.                                *)
(******************************************************************************)

SetAttributes[DataRegion, {NHoldFirst, ReadProtected}];
DataRepresentationQ[DataRegion[attrs_, data_]] = True;
attributes[DataRegion[attrs_, data_]] := attrs;
data[DataRegion[attrs_, d_]] := d;

DataRegion /: MakeBoxes[d_DataRegion, StandardForm] :=
 Module[{name, dims, range},
  name = VariableName[d] /. Undefined -> "<<unnamed>>";
  dims = Dimensions[d];
  range = CoordinateRanges[d];

  TagBox[
   RowBox[
    {"DataRegion",
     "[",
     RowBox[
      {ToString[name],
       ",",
       "\"<\"",
       "\[InvisibleSpace]",
       Sequence@@Riffle[dims, ","],
       "\[InvisibleSpace]",
       "\">\"",
       ",",
       ToBoxes[range]
      }],
     "]"
    }],
   DataRegion,
   Editable -> False]
];


(**********************************************************)
(* CoordinateSpacings                                     *)
(**********************************************************)

CoordinateSpacings[d_DataRegion] := Spacing /. attributes[d];


(**********************************************************)
(* CoordinateRanges                                       *)
(**********************************************************)

CoordinateRanges[d_DataRegion] :=
 MapThread[List, {MinCoordinates[d], MaxCoordinates[d]}];


(**********************************************************)
(* MinCoordinates                                         *)
(**********************************************************)

MinCoordinates[d_DataRegion] := Origin /. attributes[d];


(**********************************************************)
(* MaxCoordinates                                         *)
(**********************************************************)

MaxCoordinates[d_DataRegion] :=
 MinCoordinates[d] + CoordinateSpacings[d] * (Dimensions[d] - 1);


(**********************************************************)
(* VariableName                                           *)
(**********************************************************)

SyntaxInformation[VariableName] =
 {"ArgumentsPattern" -> {_}};

VariableName[d_DataRegion] := VariableName /. attributes[d];


(**********************************************************)
(* ToDataRegion                                           *)
(**********************************************************)

(* TODO: Add something like TableToDataRegion which takes a list of elements 
   each of the form {x,y,..., f} and converts it to a DataRegion. It is assumed
   (and not checked) that the data grid is regular.
*)

(* TODO: make ToDataRegion more flexible about creating DataRegions from
   expressions - make it like DataRegionTable *)
(* TODO: add a CoordinateGrid[d] which returns {{min,max,d},...} *)

Options[ToDataRegion] := {
  "VariableName" -> Undefined,
  "Time" -> Undefined};

SyntaxInformation[ToDataRegion] =
 {"ArgumentsPattern" -> {_, ___, OptionsPattern[]}};

ToDataRegion[data_List, origin_List, spacing_List, opts:OptionsPattern[]] :=
 Module[{precision},
  precision = Precision[{origin, spacing}];
  If[ArrayDepth[data] =!= Length[origin],
    Error["ToDataRegion: Data and origin have inconsistent dimensions"]];
  If[ArrayDepth[data] =!= Length[spacing],
    Error["ToDataRegion: Data and spacing have inconsistent dimensions"]];
  DataRegion[
    {VariableName -> OptionValue["VariableName"],
     Origin -> SetPrecision[origin, precision],
     Spacing -> SetPrecision[spacing, precision],
     Time -> OptionValue["Time"]
    },
    Developer`ToPackedArray[data]
  ]
];

ToDataRegion[ds:List[DataRegion[___]..]] :=
 Module[{x1, x2, dx, n, data, offsets},
  (* Find the shape of the bounding-box DataRegion *)
  x1 = Min /@ Transpose[MinCoordinates /@ ds];
  x2 = Max /@ Transpose[MaxCoordinates /@ ds];
  dx = DeleteDuplicates[CoordinateSpacings /@ ds];

  (* TODO: Check that all origins are separated by multiples of their spacing. *)
  If[Length[dx] =!= 1,
    Error["ToDataRegion only supportes merging DataRegions with the same grid spacing."];
  ,
    dx = First[dx];
  ];

  (* Create the data List for the new DataRegion and initially fill all entries with None *)
  n = Round[(x2 - x1)/dx] + 1;
  data = ConstantArray[None, n];

  (* Find the offsets in the new DataRegion where the existing DataRegions should go *)
  offsets = Map[Thread[Span[Round[(MinCoordinates[#] - x1)/dx] + 1, Round[(MinCoordinates[#] - x1)/dx] + Dimensions[#]]] &, ds];

  (* Insert existing data *)
  MapThread[(data[[Sequence@@#1]] = ToListOfData[#2])&, {offsets, ds}];

  (* Create the new DataRegion *)
  ToDataRegion[Developer`ToPackedArray[data], x1, dx, VariableName -> VariableName[ds[[1]]]]
];


(**********************************************************)
(* ToListOfData                                           *)
(**********************************************************)

DataRegion /: ToListOfData[d_DataRegion] :=
  data[d];


(**********************************************************)
(* ToListOfCoordinates                                    *)
(**********************************************************)

DataRegion /: ToListOfCoordinates[d_DataRegion, OptionsPattern[]] :=
 Module[{dims, origin, spacing, coords, coordList},
  dims    = ArrayDepth[d];
  origin  = MinCoordinates[d];
  spacing = CoordinateSpacings[d];

  coords = ToListOfData[#] & /@
    (Coordinate[d, #] & /@ Range[dims]);
  coordList = Transpose[coords, RotateRight[Range[dims + 1]]];

  If[OptionValue[Flatten],
    Flatten[coordList, dims-1],
    coordList
  ]
];


(**********************************************************)
(* ToList                                                 *)
(**********************************************************)

DataRegion /: ToList[d_DataRegion, OptionsPattern[]] :=
 Module[{dims, coords, data, list},
  dims   = ArrayDepth[d];
  coords = ToListOfCoordinates[d, Flatten -> False];
  data   = ToListOfData[d];

  list = Map[Flatten, Transpose[{coords, data}, RotateRight[Range[dims + 1]]], {dims}];

  If[OptionValue[Flatten],
    Flatten[list, dims-1],
    list
  ]
];


(**********************************************************)
(* Functions which should just see a regular data List.   *)
(* These fall into two categories:                        *)
(* 1. Those which take a single DataRegion and return a   *)
(*    DataRegion. We assume all functions which are       *)
(*    Listable and NumericFunction fall into this         *)
(*    category.                                           *)
(* 2. Those which take a single DataRegion and return     *)
(*    something else. We assume all functions which are   *)
(*    NumericFunction fall into this category.            *)
(**********************************************************)

$NonDataRegionFunctions =
  {ArrayDepth, Dimensions, Total, Mean, Position, Extract};

$DataRegionFunctions =
  {FilterNaNs};

DataRegion /: f_Symbol[x___, d_DataRegion, y___] /;
 MemberQ[$DataRegionFunctions, f] ||
 (MemberQ[Attributes[f], NumericFunction] && MemberQ[Attributes[f], Listable]) :=
 Module[{args},
  (* TODO: Check DataRegions are on the same grid *)
  args = {x, d, y} /. dr_DataRegion :> ToListOfData[dr];
  DataRegion[attributes[d], f@@args]
];

DataRegion /: f_Symbol[x___, d_DataRegion, y___] /;
 MemberQ[$NonDataRegionFunctions, f] || MemberQ[Attributes[f], NumericFunction] :=
 Module[{},
  f[x, ToListOfData[d], y]
];


(****************************************************************)
(* Shifted                                                      *)
(****************************************************************)

Shifted[d_DataRegion, delta_List] :=
 Module[{data, origin, spacing},
  data    = ToListOfData[d];
  origin  = MinCoordinates[d] + delta;
  spacing = CoordinateSpacings[d];

  ToDataRegion[data, origin, spacing, VariableName -> VariableName[d]]
];


(**********************************************************)
(* Slab                                                   *)
(**********************************************************)

Options[Slab] = {
  "Tolerance" -> 0.
};

SyntaxInformation[Slab] =
 {"ArgumentsPattern" -> {_, __, OptionsPattern[]}};

(* TODO: Implement Tolerance support *)
Slab[d_DataRegion, s__, OptionsPattern[]]:=
 Module[{slabSpec, spacing, origin, indexrange},
  spacing = CoordinateSpacings[d];
  origin  = MinCoordinates[d];

  slabSpec = PadRight[{s}, ArrayDepth[d], All];

  (* Convert coordinate range to index range *)
  indexrange = Round[(slabSpec-origin)/spacing] + 1 //.
    {Round[(All + a_) b_] + 1 -> All,
     Round[(Span[x__] + a_) b_] +1 :> Span[Sequence @@ (Round[({x} + a) b] + 1)]
    };

  (* Get the relevant part of the data *)
  Part[d, Sequence@@indexrange]
];


(**********************************************************)
(* Part                                                   *)
(**********************************************************)

DataRegion /: Part[d_DataRegion, s__] :=
 Module[{partSpec, dimensionExists, makeExplicit, start, stride, data, origin, spacing},
  (* TODO: remove this restriction *)
  If[Count[{s}, _?Negative, Infinity] > 0,
    Error["Negative part specifications are not currently supported by DataRegion."];
  ];

  (* Any dimensions not explicitly mentioned are assumed to be All *)
  partSpec = PadRight[{s}, ArrayDepth[d], All];

  (* Figure out which dimensions still exist in the new DataRegion *)
  dimensionExists =
   Map[(# /.
    {All -> True,
     Span[__] -> True,
     List[__] -> True,
     x : (_Real | _Integer) -> False}) &,
    partSpec
   ];

  (* Work out a start index and stride from a part specification *)
  makeExplicit[partSpec_] :=
   Module[{start, stride},
     Which[
      partSpec === All,
       start  = 1;
       stride = 1;,
      NumberQ[partSpec],
       start  = partSpec;
       stride = 0;,
      MatchQ[partSpec, Span[_,_]],
       start  = partSpec[[1]] /. All -> 1;
       stride = 1;,
      MatchQ[partSpec, Span[_,_,_]],
       start  = partSpec[[1]] /. All -> 1;
       stride = partSpec[[3]];,
      MatchQ[partSpec, List[_]],
       start  = partSpec[[1]];
       stride = 0;,
      MatchQ[partSpec, List[__]],
       If[!(Equal @@ Differences[partSpec]),
         Error["Part specification "<>ToString[partSpec]<>" would yield an irregularly spaced DataRegion."];
       ];
       start  = partSpec[[1]];
       stride = partSpec[[2]] - partSpec[[1]];,
      True,
       Error["Part specification "<>ToString[partSpec]<>" not supported."];
     ];
     {start, stride}
   ];

  {start, stride} = Transpose[makeExplicit /@ partSpec];

  (* Slab the data *)
  data    = Part[ToListOfData[d], s];
  origin  = Pick[MinCoordinates[d] + (start - 1) * CoordinateSpacings[d], dimensionExists];
  spacing = Pick[stride * CoordinateSpacings[d], dimensionExists];

  (* TODO: What happens if the result is a 0d DataRegion?  Should you
     just get the data value, or should you get a 0d DataRegion? *)

  (* TODO: Make sure other attributes get propagated *)
  ToDataRegion[data, origin, spacing, VariableName -> VariableName[d]]
];


(**********************************************************)
(* Take                                                   *)
(**********************************************************)

DataRegion /: Take[d_DataRegion, s__] :=
 Module[{partSpec},
  (* TODO: remove this restriction *)
  If[Count[{s}, None, Infinity] > 0,
    Error["\"None\" part specifications are not currently supported by DataRegion."];
  ];

  partSpec = Map[# /.
    {List[x_]  :> List[x],
     List[x__] :> Span[x],
     n_Integer :> If[n<0, Span[n, All], Span[1, n]]
    } &, {s}];

  Part[d, Sequence @@ partSpec]
];


(**********************************************************)
(* Drop                                                   *)
(**********************************************************)

DataRegion /: Drop[d_DataRegion, s__] :=
 Module[{partSpec},
  (* TODO: remove this restriction *)
  If[Count[{s}, All, Infinity] > 0,
    Error["\"All\" part specifications are not currently supported by DataRegion."];
  ];

  (* TODO: relax this restriction *)
  If[Count[{s}, List[__]] > 0,
    Error["\"List\" part specifications are not currently supported by DataRegion."];
  ];

  partSpec = Map[# /.
    {n_Integer :> If[n<0, Span[1, n-1], Span[n+1, All]],
     None -> All
    } &, {s}];

  Part[d, Sequence @@ partSpec]
];


(**********************************************************)
(* SameGridQ                                              *)
(**********************************************************)

SyntaxInformation[SameGridQ] =
 {"ArgumentsPattern" -> {_, _}};

DataRegion /: SameGridQ[dr1_DataRegion, dr2_DataRegion] :=
 Module[{origin, spacing, dims},
   origin  = MinCoordinates /@ {dr1, dr2};
   spacing = CoordinateSpacings /@ {dr1, dr2};
   dims    = Dimensions /@ {dr1, dr2};

   (SameQ@@origin) && (SameQ@@spacing) && (SameQ@@dims)
];


(**********************************************************)
(* Map                                                    *)
(**********************************************************)

DataRegion /: Map[f_, d_DataRegion, n_:Automatic] :=
 Module[{depth},
  depth = {ArrayDepth[d]};
  If[n =!= Automatic && n =!= depth,
  	Error["Map can only operate on the deepest level in a DataRegion."];
  ];
  DataRegion[attributes[d], Map[f, ToListOfData[d], depth]]
];


(**********************************************************)
(* MapThread                                              *)
(**********************************************************)

Unprotect[MapThread];

MapThread[f_, ds:List[DataRegion[___]..], n_:Automatic] :=
 Module[{depth},
  If[Length[DeleteDuplicates[ds, SameGridQ]] =!= 1,
  	Error["MapThread is only supported for DataRegions on the same grid."];
  ];
  depth = ArrayDepth[ds[[1]]];
  If[n =!= Automatic && n =!= depth,
  	Error["MapThread can only operate on the deepest level in a DataRegion."];
  ];
  DataRegion[attributes[ds[[1]]], MapThread[f, ToListOfData /@ ds, depth]]
];

Protect[MapThread];


(**********************************************************)
(* Downsampled                                            *)
(**********************************************************)

Downsampled[d_DataRegion, n_Integer] :=
 Module[{ndims},
  ndims = ArrayDepth[d];
  Downsampled[d, ConstantArray[n,ndims]]
];

Downsampled[d_DataRegion, n_List] :=
  Take[d, Apply[Sequence, Transpose[{ConstantArray[1, ArrayDepth[d]], Dimensions[d], n}]]];


(**********************************************************)
(* Resampled                                              *)
(**********************************************************)

DataRegion /: Resampled[d_DataRegion, grid_List] :=
 Module[{dims, interp, vars, tmp, iterators, data},
  dims = Dimensions[grid];
  If[dims =!= {ArrayDepth[d], 3},
  	Error["Expected a list of triples for each dimension in the DataRegion."];
  ];
  Quiet[interp = Interpolation[d];, ListInterpolation::inhr];
  vars = tmp /@ Range[ArrayDepth[d]];
  iterators = MapThread[Join[{#1}, #2] &, {vars, grid}];
  data = Table[interp @@ vars, Evaluate[Sequence @@ iterators]];
  ToDataRegion[data, grid[[All, 1]], grid[[All, 3]], VariableName -> VariableName[d]]
];

Resampled[ds:{DataRegion[__]...}] :=
  Module[{x1, x2, dx, grid},
    x1 = Max /@ Transpose[MinCoordinates /@ ds];
    x2 = Min /@ Transpose[MaxCoordinates /@ ds];
    dx = Min /@ Transpose[CoordinateSpacings /@ ds];
    grid = Transpose[{x1, x2, dx}];

    If[Or@@Negative[x2 - x1], Error["Intersection of boxes is empty"]];

    Map[Resampled[#, grid] &, ds]
];


(**********************************************************)
(* Plotting functions                                     *)
(**********************************************************)

(* TODO: Provide undocumented workaround functions for the SaveDefinitions/Protected issue? *)

$1DPlotFunctions = {ListPlot, ListLinePlot};
$2DPlotFunctions = {ListDensityPlot, ArrayPlot, ListPlot3D, ListContourPlot};

DataRegion /: f_Symbol[d_DataRegion, args___] :=
 plotWrapper[f, 1, d, args] /; MemberQ[$1DPlotFunctions, f];

DataRegion /: f_Symbol[d_DataRegion, args___] :=
 plotWrapper[f, 2, d, args] /; MemberQ[$2DPlotFunctions, f];

plotWrapper[plotFunction_, plotDims_, d_DataRegion, args___] :=
 Module[{ndims, dataRange, data},
  ndims = ArrayDepth[d];
  If[ndims!=plotDims,
    Error[SymbolName[plotFunction]<>" only supports data with dimensionality "<>ToString[plotDims]<>
      ". The provided data has dimensionality "<>ToString[ndims]<>"."];
  ];

  dataRange =  If[ndims==1, CoordinateRanges[d][[1]], CoordinateRanges[d]];
  data = ToListOfData[d];
  plotFunction[data, args, DataRange -> dataRange]
];

(* We cannot use upvalues in the following as the DataRegion appears too deep *)
(* TODO: Make this use plotWrapper for consistency *)
Unprotect /@ $1DPlotFunctions;
Scan[(#[ds:List[DataRegion[___]..], opts___] := #[ToList/@ ds, opts])&, $1DPlotFunctions];
Protect /@ $1DPlotFunctions;


(**********************************************************)
(* CoordinateOutline                                      *)
(**********************************************************)

SyntaxInformation[CoordinateOutline] =
 {"ArgumentsPattern" -> {_}};

CoordinateOutline[d_DataRegion] :=
 Module[{ndims, coords, shapes},
  ndims = ArrayDepth[d];

  If[ndims > 3,
    Error["Dimension "<>ToString[ndims]<>" of DataRegion not supported by Outline."]
  ];

  coords = CoordinateRanges[d];

  If[ndims === 1,
    coords = {Transpose[Join[{{0,0}}, coords]]},
    coords = Transpose[coords]
  ];

  shapes = {Line, Rectangle, Cuboid};

  shapes[[ndims]]@@coords
];


(**********************************************************)
(* Coordinate                                             *)
(**********************************************************)

SyntaxInformation[Coordinate] =
 {"ArgumentsPattern" -> {_, _}};

Coordinate[d_DataRegion, dim_] :=
 Module[{origin, spacing, dims, max, ca, dm, dp, res},
  dims    = Dimensions[d];
  origin  = MinCoordinates[d];
  spacing = CoordinateSpacings[d];
  max     = MaxCoordinates[d];

  ca[x_, {}] := x;
  ca[x_, l_] := ConstantArray[x, l];

  (* Fiendishly clever code *)
  (* TODO: Simplify these Transpose/Reverse uses *)
  dm = Reverse @ Table[dims[[i]], {i, 1, dim - 1}];
  dp = Reverse @ Table[dims[[i]], {i, dim + 1, Length[dims]}];

  res = Transpose[
    ca[Table[ca[x, dm], {x, origin[[dim]], max[[dim]], spacing[[dim]]}], dp],
    Reverse[Range[ArrayDepth[d]]]];

  If[Dimensions[res] =!= dims, Error["GetCoordinateError"]];

  ToDataRegion[res, origin, spacing, VariableName -> VariableName[d]]
];


(**********************************************************)
(* NDerivative                                             *)
(**********************************************************)

NDerivative[d:DataRegion[h_,_], dir_Integer] :=
 Module[{ndims},
   ndims   = ArrayDepth[d];
   (NDerivative@@UnitVector[ndims, dir])[d]
];

NDerivative[d_DataRegion] :=
 Module[{ndims},
  ndims = ArrayDepth[d];

  If[ndims =!= 1,
   Error["Must specify a direction when applying NDerivative to a DataRegion of dimension greater than 1."];
  ];

  NDerivative[d, 1];
];

NDerivative[derivs__][d:DataRegion[h_,_], opts___] :=
 Module[{origin, spacing, dimensions, grid, data, deriv},
  origin  = MinCoordinates[d];
  spacing = CoordinateSpacings[d];
  dimensions = Dimensions[d];

  (* Get the grid in the form {{x1, ..., xn}, {y1, ..., yn}, ...} *)
  grid = origin + spacing (Range /@ dimensions-1);
  data = ToListOfData[d];

  deriv = NDSolve`FiniteDifferenceDerivative[Derivative[derivs], grid, data, opts];

  ToDataRegion[deriv, origin, spacing]
];


(**********************************************************)
(* Interpolation                                          *)
(**********************************************************)

DataRegion /: Interpolation[v_DataRegion, opts___] :=
  Module[{data = ToListOfData[v]},
    ListInterpolation[data, CoordinateRanges[v], opts]
];







(****************************************************************)
(* Experimental                                                 *)
(****************************************************************)

(**********************************************************)
(* TimeDerivative                                         *)
(**********************************************************)

SyntaxInformation[TimeDerivative] =
 {"ArgumentsPattern" -> {_, ___}};

UFDWeights[m_, n_, s_, h_] :=
 CoefficientList[Normal[Series[x^s Log[x]^m, {x, 1, n}]/h^m], x]

TimeDerivative[dr:{__DataRegion}, centering_:Automatic] :=
 Module[{nd, dims, spacing, origin, variable, sorted, times, dt, stencil, offset, deriv, attr, newTime},
  nd = ArrayDepth/@dr;
  If[Not[Equal@@nd],
   Error["Error, can't compute time derivative from DataRegions with a different number of dimensions."];
   Return[$Failed];
  ];

  dims = Dimensions/@dr;
  If[Not[Equal@@dims],
   Error["Error, can't compute time derivative from DataRegions with different dimensions."];
   Return[$Failed];
  ];

  spacing = CoordinateSpacings/@dr;
  If[Not[Equal@@spacing],
    Error["Error, can't compute time derivative from DataRegions with different spacings."];
    Return[$Failed];
  ];

  origin = MinCoordinates/@dr;
  If[Not[Equal@@origin],
    Error["Error, can't compute time derivative from DataRegions with different origins."];
    Return[$Failed];
  ];

  variable = VariableName/@dr;
  If[Not[Equal@@variable],
    Error["Error, can't compute time derivative from DataRegions with different variables."];
    Return[$Failed];
  ];

  (* Sort DataRegions by time *)
  sorted = SortBy[dr, GetTime];

  (* Check spacing is uniform *)
  times = GetTime/@sorted;
  If[Apply[Or, Map[# > 10^-10 &, Abs[1 - Differences[times]/Differences[times][[1]]]]],
    Error["Error, can't compute time derivative from DataRegions with non-uniform spacing in time."];
    Return[$Failed];
  ];

  (* Get finite differencing stencil *)
  If[centering === Automatic, offset = (Length[dr]-1)/2, offset = centering];
  dt = times[[2]] - times[[1]];
  stencil = UFDWeights[1, Length[dr]-1, offset, dt];
  
  (* Compute derivative using finite differences *)
  If[Length[stencil]!=Length[sorted],
    Error["Error, can't compute time derivative from inconsistent stencil and DataRegions."];
    Return[$Failed];
  ];
  deriv = stencil.sorted;

  (* Correct time and variable name attributes *)
  newTime = times[[1]]+offset*dt;
  attr=replaceRules[attributes[deriv], {Time-> newTime, VariableName -> "dt_"<>variable[[1]]}];

  DataRegion[attr, ToListOfData[deriv]]
];







(**********************************************************************************)
(* Deprecated functions                                                           *)
(**********************************************************************************)

(**********************************************************)
(* ResampleDataRegion                                     *)
(**********************************************************)
interpolation[v_DataRegion, opts___] :=
  Module[{data = GetData[v], ndims = GetNumDimensions[v]},
    ListInterpolation[Transpose[data,Reverse[Range[ndims]]], GetDataRange[v], opts]
];

ResampleDataRegion[d_DataRegion, {x1_List, x2_List, dx_List}, p_] :=
  Module[{dFn, newData},
    dFn = interpolation[d, InterpolationOrder->p];
    If[GetNumDimensions[d] === 2,
      newData = Table[dFn[x,y], {x, x1[[1]], x2[[1]], dx[[1]]},
                                {y, x1[[2]], x2[[2]], dx[[2]]}];
      ToDataRegion[newData, x1, dx, VariableName -> GetVariableName[d], Time -> GetTime[d]],

      Error["Cannot resample DataRegions of dimension other than 2"]]
  ];


(**********************************************************)
(* ResampleDataRegions                                    *)
(**********************************************************)

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
    If[Negative[x2 - x1], Error["Intersection of boxes is empty"]];
    {x1, x2}];

ResampleDataRegions[ds:{DataRegion[__]...}, p_:3] :=
  Module[{dxs,ranges,x1,x2,dx},
    If[Length[ds] === 0, Return[{}]];
    dxs = Map[GetSpacing, ds];
    ranges = Map[Transpose[GetDataRange[#]] &, ds];
    {x1, x2} = intersectBoxes[ranges];
    dx = Map[Min, Transpose[dxs]];
    Map[ResampleDataRegion[#, {x1, x2, dx}, p] &, ds]];


MakeDataRegion[data_List, name_String, dims_List, origin_List, spacing_List, time_] :=
  DataRegion[{VariableName -> name, Origin -> origin, Spacing -> spacing, Time -> time},
             Developer`ToPackedArray[data]];

SliceData[v:DataRegion[h_, data_], dim_Integer, coord_:0] :=
 Module[{index, newOrigin, newSpacing, h2, origin, spacing, dims, range, slice, ndims},
  origin = GetOrigin[v];
  spacing = GetSpacing[v];
  dims = GetDimensions[v];
  ndims = GetNumDimensions[v];

  If[dim > ndims,
    Error["Slicing direction "<>ToString[dim]<>" is greater than dimension "<>
      ToString[ndims]<>" of the DataRegion."]
  ];

  range = GetDataRange[v][[dim]];

  If[(coord < range[[1]]) || (coord >range[[2]]),
    Error["Slice coordinate "<>ToString[coord]<>" is outside the range "<>
      ToString[range, StandardForm]<>" of the DataRegion."]
  ];

  index = Round[(coord - origin[[dim]])/spacing[[dim]]+1];
  newOrigin = Drop[origin, {dim}];
  newSpacing = Drop[spacing, {dim}];
  h2 = replaceRules[h, {Origin -> newOrigin, Spacing-> newSpacing}];
  slice = Sequence @@ Reverse[Join[ConstantArray[All,dim-1],{index},ConstantArray[All,ndims-dim]]];
  Return[DataRegion[h2, data[[slice]] ]]
];

SliceData[v_DataRegion, dims_List, coords_:0] := 
  Fold[SliceData[#, Sequence@@#2]&, v, Reverse[SortBy[Thread[{dims, coords}], First]]];

GetData[d_DataRegion] := data[d];
GetAttributes[d_DataRegion] := attributes[d];
GetDimensions[d_DataRegion] := Reverse[Dimensions[GetData[d]]];
GetNumDimensions[DataRegion[h_, data_]] := Length[Dimensions[data]];
GetOrigin[DataRegion[h_, data_]] := Origin /. h;
GetSpacing[DataRegion[h_, data_]] := Spacing /. h;
GetTime[DataRegion[h_, _]] := Time /. h;
GetVariableName[DataRegion[h_, data_]] := VariableName /. h;
GetDataRange[v_DataRegion] :=
  Module[{origin, spacing, dimensions, min, max},
    origin = GetOrigin[v];
    spacing = GetSpacing[v];
    dimensions = GetDimensions[v];
    min = origin;
    max = origin + spacing * (dimensions - 1);
    MapThread[List, {min, max}]];

DataRegionPart[d:DataRegion[h_, data_], s_]:=
 Module[{ndims, spacing, origin, dataRange, newS, indexrange, newOrigin, h2, newData},
  ndims = GetNumDimensions[d];
  spacing = GetSpacing[d];
  origin = GetOrigin[d];
  dataRange = GetDataRange[d];

  If[Length[s] != ndims,
    Error["Range "<>ToString[s]<>" does not match the dimensionality of the DataRegion."]
  ];

  (* Convert all to a range *)
  newS = MapThread[#1/.#2&, {s, Thread[All -> dataRange]}];

  (* Clip the range to the lower and upper bound *)
  newS = MapThread[{Max[#1[[1]],#2[[1]]], Min[#1[[2]],#2[[2]]]}&, {dataRange, newS}];

  (* Convert coordinate range to index range *)
  indexrange = Round[(Apply[List,newS,1]-origin)/spacing]+1;

  (* Get the relevant part of the data *)
  newData = Part[data, Sequence@@Reverse[Apply[Span,indexrange,1]]];

  (* Get new origin *)
  newOrigin = origin + spacing (indexrange[[All,1]]-1);

  h2 = replaceRules[h, {Origin -> newOrigin}];

  DataRegion[h2, newData]
];

DataRegionPart[d:DataRegion[h_, data_], s_Span] := DataRegionPart[d, {s}];

(* Plotting wrappers *)
DataRegionPlot[plotFunction_, plotDims_, v_DataRegion, args___] := Module[{ndims, dataRange, data, opts},
 ndims = GetNumDimensions[v];
 If[ndims!=plotDims,
   Error[SymbolName[plotFunction]<>" only supports data with dimensionality "<>ToString[plotDims]<>
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
(*DynamicDataRegionArrayPlot[v_DataRegion, args___]   := DataRegion2DPlot[DynamicArrayPlot, v, args];*)
DataRegionMatrixPlot[v_DataRegion, args___]   := DataRegion2DPlot[MatrixPlot, v, args];
DataRegionPlot3D[v_DataRegion, args___]      := DataRegion2DPlot[ListPlot3D, v, args]; 

DataRegionContourPlot[v_, args___]      := DataRegion2DPlot[ListContourPlot, v, args]; 


NormL2[d_DataRegion] :=
 Sqrt[Times@@GetSpacing[d] * Plus @@ Flatten[GetData[d^2]]];

Outline[d_DataRegion] := Module[{coords, ndims, shapes},
  ndims = GetNumDimensions[d];
  coords = {GetOrigin[d], GetOrigin[d] + GetSpacing[d] * (GetDimensions[d] - 1)};
  If[ndims == 1, coords = {Thread[{ {0,0}, coords[[All,1]] }]}];

  shapes = {Line, Rectangle, Cuboid};

  If[ndims > 3,
    Error["Dimension "<>ToString[ndims]<>" of DataRegion not supported by Outline."]
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
    attrs = attributes[d];
    attrs2 = replaceRules[attrs, 
      {Origin -> (GetOrigin[d] + n * GetSpacing[d])}];
    d2 = DataRegion[attrs2, data2]];

Strip[d_DataRegion, n_List, m_List] :=
  Module[{data, data2, attrs, attrs2, d2},
    data = GetData[d];
	 data2 = Take[data, Apply[Sequence, MapThread[{#1+1,-(#2+1)}&, {Reverse[n], Reverse[m]}]]];
    attrs = attributes[d];
    attrs2 = replaceRules[attrs,
      {Origin -> (GetOrigin[d] + n * GetSpacing[d])}];
    d2 = DataRegion[attrs2, data2]];


Downsample[d_DataRegion, n_Integer] :=
  Module[{ndims},
    ndims = GetNumDimensions[d];
    Downsample[d, ConstantArray[n,ndims]]];

Downsample[d_DataRegion, n_List] :=
 Module[{data, data2, attrs, attrs2, d2},
  data = GetData[d];
  data2 = Take[data, Apply[Sequence, Map[{1, -1, #} &, Reverse[n]]]];
  attrs = attributes[d];
  attrs2 =
   replaceRules[
    attrs, {Origin -> GetOrigin[d],  Spacing -> GetSpacing[d]*n}];
  d2 = DataRegion[attrs2, data2]];

MapDataRegion[f_, d_DataRegion] :=
 Module[{dim, header, data},
  dim = GetNumDimensions[d];
  header = attributes[d];
  data = Map[f, GetData[d], {dim}];
  DataRegion[header, data]];

MapThreadDataRegion[f_, drs_] :=
  Module[{attrs, datas, newData},
    attrs = attributes[First[drs]];
    datas = GetData /@ drs;
    newData = MapThread[f, datas, GetNumDimensions[First[drs]]];
    DataRegion[attrs, newData]];

(* SetAttributes[EvaluateOnDataRegion, HoldFirst]; *)
EvaluateOnDataRegion[exprp_, {t_, x_, y_, z_}, dp_DataRegion] :=
  Module[
    {xd, yd, zd, td, result, expr, d},
    expr = ReleaseHold[exprp];
    d = ReleaseHold[dp];
    {xd, yd, zd} = Table[GetCoordinate[d, i], {i, 3}];
    td = GetTime[d];
    result = (0.*x + expr) /. {x :> xd, y :> yd, z :> zd, t :> td};
    If[!NumberQ@Max@Flatten[GetData[result]],
       Error["Expression " <> ToString[expr,InputForm]<> " did not evaluate to a numeric value in coordinates "<>ToString[{t,x,y,z}]<>" ("<>ToString[Max@Flatten[GetData[result]],InputForm]]];
    result];

EvaluateOnDataRegion[exprp_, {t_, x_, y_}, dp_DataRegion] :=
  Module[
    {xd, yd, td, result, expr, d},
    expr = ReleaseHold[exprp];
    d = ReleaseHold[dp];
    {xd, yd} = Table[GetCoordinate[d, i], {i, 2}];
    td = GetTime[d];
    result = (0.*x + expr) /. {x :> xd, y :> yd, t :> td};
    If[!NumberQ@Max@Flatten[GetData[result]],
       Error["Expression " <> ToString[expr,InputForm]<> " did not evaluate to a numeric value in coordinates "<>ToString[{t,x,y}]<>" ("<>ToString[Max@Flatten[GetData[result]],InputForm]]];
    result];


(* Take a list of elements of the form {x,y,..., f} and convert it to
   a DataRegion. It is assumed (and not checked) that the data grid is
   regular.  *)

TableToDataRegion[t_List] :=
 Module[{d, sorted, split, spacing, origin, subregions, s, subdims,
   suborigin, subspacing},
  d = Length[t[[1]]] - 1; (* Number of dimensions in the data *)

  sorted = Sort[t, #1[[d]] < #2[[d]] &];
  split = SplitBy[sorted, #[[d]] &];
  spacing = split[[2, 1, d]] - split[[1, 1, d]];
  origin = split[[1, 1, d]];
  If[d == 1,
   ToDataRegion[Map[Last, sorted],
    {origin}, {spacing}],
   (* else *)
   subregions =
     Map[TableToDataRegion, Map[Drop[#, {d}] &, split, {2}]];
   s = First[subregions];
   subdims = GetDimensions[s];
   suborigin = GetOrigin[s];
   subspacing = GetSpacing[s];
   MakeDataRegion[Map[GetData, subregions], "table",
    Append[subdims, Length[subregions]], Append[suborigin, origin],
    Append[subspacing, spacing], 0]]];

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

    If[Reverse@Dimensions[res] =!= dims, Error["GetCoordinateError"]];
    DataRegion[attributes[d], res]
  ];

(**********************************************************)
(* MergeDataRegions                                       *)
(**********************************************************)

replaceRule[list_List, key_ -> newValue_] :=
  list /. (key -> _) :> (key -> newValue);  

replaceRules[list_List, replacements_List] :=
  If[Length[replacements] == 0, 
     list, 
     replaceRules[replaceRule[list, First[replacements]], Drop[replacements, 1]]];

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
 Module[{ndims, origins, dims, spacings, spacing, spacingDiffs,
    X1, X2s, X2, n, dat, attrs, attrs2},
  If[Length[regions] === 0, Return[{}]];

  If[!And@@Map[MatchQ[#, _DataRegion] &, regions],
    Error["MergeDataRegions: Expected a list of DataRegion objects but got instead " <> ToString[regions]]];
  origins = Map[GetOrigin, regions];
  dims = Map[GetDimensions, regions];
  ndims = GetNumDimensions[regions[[1]]];

  (* Find the lower coordinate of the bounding box *)
  X1 = Min[origins[[All,#]]]&/@ Range[ndims];

  spacings = Map[GetSpacing, regions];
  spacingDiffs = (# - spacings[[1]]) & /@ spacings;

  If[ Max[Norm/@spacingDiffs] > 10^-8,
    Error["MergeDataRegions: Attempt to merge DataRegions with different spacings: " <> ToString[spacings] <> ", " <> ToString[spacingDiffs]]];
  spacing = First[spacings];
  
  (* Find the upper coordinate of the bounding box *)
  X2s = MapThread[#1 + spacing * (#2 - 1) &, {origins, 
     dims}];
  X2 = Max[X2s[[All,#]]]&/@ Range[ndims];

  n = Round[(X2 - X1)/spacing] + 1;
  dat = ConstantArray[None, Reverse[n]];
  Scan[insertArray[dat, GetData[#], chunkOffset[#, X1, spacing]] &, 
   regions];
  attrs = attributes[regions[[1]]];
  attrs2 = replaceRules[attrs, {Origin -> X1}];
  Return[DataRegion[attrs2, Developer`ToPackedArray[dat]]]]
];

End[];

EndPackage[];
