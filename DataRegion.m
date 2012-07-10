(* ::Package:: *)

(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["DataRegion`", {"DataTable`", "Profile`", "Error`"}];

DataRegion::usage = "DataRegion[...] is a representation of an N-dimensional array of numbers on a regular grid.";
ToDataRegion::usage = "ToDataRegion[data, origin, spacing] creates a DataRegion object from the N-dimensional array (nested list) data.";

ToListOfData::usage = ToListOfData::usage<>"
ToListOfData[d] returns the N-dimensional array of data in DataRegion d.";
ToListOfCoordinates::usage = ToListOfCoordinates::usage<>"
ToListOfCoordinates[d] returns the N-dimensional array of coordinates of DataRegion d.";
ToList::usage = ToList::usage<>"
ToList[d] returns the N-dimensional array of coordinates and data in DataRegion d.";

CoordinateRanges::usage = "CoordinateRanges[d] returns the data range of a DataRegion. This is a list of {min,max} pairs, each corresponding to one dimension of d.";
CoordinateSpacings::usage = "CoordinateSpacings[d] returns a list giving the spacing of the data in each dimension of the DataRegion d.";
MinCoordinates::usage = "MinCoordinates[d] returns a list of the coordinates of the first point in each direction in the DataRegion d.";
MaxCoordinates::usage = "MaxCoordinates[d] returns a list of the coordinates of the last point in each direction in the DataRegion d.";
VariableName::usage = "VariableName[d] returns the variable name in DataRegion d.";

SameGridQ::usage = "SameGridQ[d1, d2] returns True if d1 and d2 are DataRegions defined on the same grid (origin, spacing, size).";

Slab::usage = "Slab[d, {{x1min, x1max}, ...}] gives the hyperslab of specified by the coordinates the coordinates {x1min, x1max}, ....";
Downsampled::usage = "Downsampled[d, n] returns a version of d with only every nth element.\n"<>
  "Downsampled[d, {n1, n2, ...nk}] returns a version of d with only every {n1, n2, ...}-th element in the direction k."

(* TODO: Add WithResampling (and WithResampling[order]) which evaluate their argument allowing resampling for algebraic operations.  Use InheritedBlock for this *)
(* TODO: add these functions *)
Resampled::usage = "Resampled[d, {{x0, x1, dx}, {y0, y1, dy}, ...}] resamples d to produce a DataRegion with coordinate ranges {x0, x1}, {y0, y1}, ... and spacings {dx, dy, ...}.";

(* TODO: decide if this is a good name *)
Intersection::usage = "Intersection[{d1, d2, ...}] returns a list of DataRegions {D1, D2, ...} which are resampled versions of the DataRegions {d1, d2, ...} over the intersection of their bounding boxes using interpolation of order p.  The new DataRegions contain the same points.";

NDerivative::usage = "NDerivative[derivs][d] returns a numerical derivative of the DataRegion d. The derivs argument should be of the same form as in the first argument of Derivative.";
GridNorm::usage = "GridNorm[d] returns the L2,dx norm of d. This is the discrete approximation to the L2 norm.";
CoordinateOutline::usage = "CoordinateOutline[d] generates a graphical representation of the outline of d";
Coordinate::usage = "Coordinate[d, i] returns a DataRegion of the same shape as d whose data is the i coordinate of d.";

(* TODO: move this to the NRMMA context.  Think about another way to do this *)
Global`Sub::usage = "Sub[d1,d2,p] returns a DataRegion whose data is the subtraction of d1 and d2 after they have been resampled at order p onto the intersection of their bounding boxes.  p is optional and defaults to 3.  Mathematica's infix notation, where a binary function can be written as an infix operator, is useful with this function.  For example, d = d1 ~Sub~ d2.";
FilterNaNs::usage = "FilterNaNs[d] replaces any NaN (Not a Number) values in the DataRegion d with Missing[], which is Mathematica's notation for missing data.";
NaNQ::usage = "NaNQ[x] returns True if x is a NaN (Not a Number) value and False if it is not.  Mathematica deals strangely with NaN values imported from other programs.  This function was developed for use with the h5mma package for reading HDF5 data.";

(* TODO: Add Metadata function and user-defined metadata *)
(* TODO: Add MapCoordinates, MapThreadCoordinates, MapData, MapThreadData, Map, MapThread *)


(* TODO: don't support this - put it in NRMMA`Experimental *)
TimeDerivative::usage = "TimeDerivative[{d1, d2,...}, center] returns a numerical time derivative computed from DataRegions d1, d2, ... . The derivative is computed using finite differencing, the order of which is determined by the number of DataRegions given. The optional center argument specifies the number of timesteps from the first DataRegion at which to compute derivatives (using lop-sided differencing, if necessary), with the default value being half-way between the first and last times.";

(* DEPRECATED *)

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
attributes[DataRegion[attrs_, data_]] := attrs;
data[DataRegion[attrs_, data_]] := data;

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

SyntaxInformation[CoordinateSpacings] =
 {"ArgumentsPattern" -> {_}};

CoordinateSpacings[d_DataRegion] := Spacing /. attributes[d];


(**********************************************************)
(* CoordinateRanges                                       *)
(**********************************************************)

SyntaxInformation[CoordinateRanges] =
 {"ArgumentsPattern" -> {_}};

CoordinateRanges[d_DataRegion] :=
 MapThread[List, {MinCoordinates[d], MaxCoordinates[d]}];


(**********************************************************)
(* MinCoordinates                                         *)
(**********************************************************)

SyntaxInformation[MinCoordinates] =
 {"ArgumentsPattern" -> {_}};

MinCoordinates[d_DataRegion] := Origin /. attributes[d];


(**********************************************************)
(* MaxCoordinates                                         *)
(**********************************************************)

SyntaxInformation[MaxCoordinates] =
 {"ArgumentsPattern" -> {_}};

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

(* TODO: Add equivalent to MergeDataRegions returns a DataRegion formed from
   merging the content of the DataRegions in the list regions.  If the regions
   do not cover a rectangular domain, any missing points will have value None.
   All the DataRegions must have the same spacing and their origins must be
   separated by multiples of their spacing.
*)

(* TODO: make ToDataRegion more flexible about creating DataRegions from
   expressions - make it like DataRegionTable *)
(* TODO: add a CoordinateGrid[d] which returns {{min,max,d},...} *)

Options[ToDataRegion] := {
  "VariableName" -> Undefined,
  "Time" -> Undefined};

SyntaxInformation[ToDataRegion] =
 {"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}};

ToDataRegion[data_List, origin_List, spacing_List, opts:OptionsPattern[]] :=
 Module[{precision},
  precision = Precision[{origin, spacing}];
  DataRegion[
    {VariableName -> OptionValue["VariableName"],
     Origin -> SetPrecision[origin, precision],
     Spacing -> SetPrecision[spacing, precision],
     Time -> OptionValue["Time"]
    },
    Developer`ToPackedArray[data]
  ]
];


(**********************************************************)
(* ToListOfData                                           *)
(**********************************************************)

Options[ToListOfData] = {"Flatten" -> True};

SyntaxInformation[ToListOfData] =
 {"ArgumentsPattern" -> {_, OptionsPattern[]}};

DataRegion /: ToListOfData[d_DataRegion, OptionsPattern[]] :=
  If[OptionValue[Flatten],
    Flatten[data[d]],
    data[d]
  ];


(**********************************************************)
(* ToListOfCoordinates                                    *)
(**********************************************************)

Options[ToListOfCoordinates] = {"Flatten" -> True};

SyntaxInformation[ToListOfCoordinates] =
 {"ArgumentsPattern" -> {_, OptionsPattern[]}};

DataRegion /: ToListOfCoordinates[d_DataRegion, OptionsPattern[]] :=
 Module[{dims, origin, spacing, coords, coordList},
  dims    = ArrayDepth[d];
  origin  = MinCoordinates[d];
  spacing = CoordinateSpacings[d];

  coords = ToListOfData[#, Flatten -> False] & /@
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

Options[ToList] = {"Flatten" -> True};

SyntaxInformation[ToList] =
 {"ArgumentsPattern" -> {_, OptionsPattern[]}};

DataRegion /: ToList[d_DataRegion, OptionsPattern[]] :=
 Module[{dims, coords, data, list},
  dims   = GetNumDimensions[d];
  coords = ToListOfCoordinates[d, Flatten -> False];
  data   = ToListOfData[d, Flatten -> False];

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
  {Map};

DataRegion /: f_Symbol[x___, d_DataRegion, y___] /;
 MemberQ[$DataRegionFunctions, f] ||
 (MemberQ[Attributes[f], NumericFunction] && MemberQ[Attributes[f], Listable]) :=
 Module[{args},
  (* TODO: Check DataRegions are on the same grid *)
  args = {x, d, y} /. dr_DataRegion :> ToListOfData[dr, Flatten -> False];
  DataRegion[attributes[d], f@@args]
];

DataRegion /: f_Symbol[x___, d_DataRegion, y___] /;
 MemberQ[$NonDataRegionFunctions, f] || MemberQ[Attributes[f], NumericFunction] :=
 Module[{},
  f[x, ToListOfData[d, Flatten -> False], y]
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
 Module[{slabSpec, spacing, origin},
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
  data    = Part[ToListOfData[d, Flatten -> False], s];
  origin  = Pick[MinCoordinates[d] + (start - 1) * CoordinateSpacings[d], dimensionExists];
  spacing = Pick[stride * CoordinateSpacings[d], dimensionExists];

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
(* MapThread                                              *)
(**********************************************************)

Unprotect[MapThread];

MapThread[f_, ds:List[DataRegion[___]..]] :=
 Module[{},
  If[Length[DeleteDuplicates[ds, SameGridQ]] =!= 1,
  	Error["MapThread is only supported for DataRegions on the same grid."];
  ];

  DataRegion[attributes[ds[[1]]], MapThread[f, ToListOfData[#, Flatten -> False] & /@ ds]]
];

Protect[MapThread];


(**********************************************************)
(* Downsampled                                            *)
(**********************************************************)

SyntaxInformation[DownSampled] =
 {"ArgumentsPattern" -> {_, _}};

Downsampled[d_DataRegion, n_Integer] :=
 Module[{ndims},
  ndims = ArrayDepth[d];
  Downsampled[d, ConstantArray[n,ndims]]
];

Downsampled[d_DataRegion, n_List] :=
  Take[data, Apply[Sequence, Map[{1, -1, #} &, n]]];


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
  data = ToListOfData[d, Flatten -> False];
  plotFunction[data, args, DataRange -> dataRange]
];

(* We cannot use upvalues in the following as the DataRegion appears too deep *)
(* TODO: Make this use plotWrapper for consistency *)
Unprotect /@ $1DPlotFunctions;
Scan[(#[ds:List[DataRegion[___]..], opts___] := #[ToList/@ ds, opts])&, $1DPlotFunctions];
Protect /@ $1DPlotFunctions;


(**********************************************************)
(* GridNorm                                               *)
(**********************************************************)

SyntaxInformation[GridNorm] =
 {"ArgumentsPattern" -> {_}};

GridNorm[d_DataRegion] :=
 Sqrt[Times @@ CoordinateSpacings[d] * Plus @@ ToListOfData[d^2, Flatten -> True]];


(**********************************************************)
(* CoordinateOutline                                      *)
(**********************************************************)

SyntaxInformation[CoordinateOutline] =
 {"ArgumentsPattern" -> {_}};

CoordinateOutline[d_DataRegion] :=
 Module[{coords, shapes},
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

SyntaxInformation[NDerivative] =
 {"ArgumentsPattern" -> {_, ___}};

Global`StandardDefinition[NDerivative] = True;

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
  data = ToListOfData[d, Flatten->False];

  deriv = NDSolve`FiniteDifferenceDerivative[Derivative[derivs], grid, data, opts];

  ToDataRegion[deriv, origin, spacing]
];


(**********************************************************)
(* FilterNaNs                                             *)
(**********************************************************)

SyntaxInformation[FilterNaNs] =
 {"ArgumentsPattern" -> {_}};

FilterNaNs[d_DataRegion] :=
 MapDataRegion[If[NaNQ[#], Missing[], #] &, d];


(**********************************************************)
(* NaNQ                                                   *)
(**********************************************************)

SyntaxInformation[NaNQ] =
 {"ArgumentsPattern" -> {_}};

NaNQ[x_] :=
 Round[x] == -2147483648;


(**********************************************************)
(* ResampleDataRegion                                     *)
(**********************************************************)

SyntaxInformation[ResampleDataRegion] =
 {"ArgumentsPattern" -> {_, _, _}};

ResampleDataRegion[d_DataRegion, {x1_List, x2_List, dx_List}, p_] :=
  Module[{dFn, newData},
    dFn = Interpolation[d, InterpolationOrder->p];
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

SyntaxInformation[ResampleDataRegions] =
 {"ArgumentsPattern" -> {_, ___}};

ResampleDataRegions[ds:{DataRegion[__]...}, p_:3] :=
  Module[{dxs,ranges,x1,x2,dx},
    If[Length[ds] === 0, Return[{}]];
    dxs = Map[GetSpacing, ds];
    ranges = Map[Transpose[GetDataRange[#]] &, ds];
    {x1, x2} = intersectBoxes[ranges];
    dx = Map[Min, Transpose[dxs]];
    Map[ResampleDataRegion[#, {x1, x2, dx}, p] &, ds]];


(**********************************************************)
(* Interpolation                                          *)
(**********************************************************)

DataRegion /: Interpolation[v_DataRegion, opts___] :=
  Module[{data = GetData[v], ndims = GetNumDimensions[v]},
    ListInterpolation[Transpose[data,Reverse[Range[ndims]]], GetDataRange[v], opts]
];


(*******************************************************************************************)
(* Sub                                                                                     *)
(*******************************************************************************************)

DataRegion/:
Global`Sub[d1_DataRegion, d2_DataRegion,p_:3] :=
  Apply[Subtract, ResampleDataRegions[{d1, d2},p]];


(**********************************************************)
(* TimeDerivative                                         *)
(**********************************************************)

SyntaxInformation[TimeDerivative] =
 {"ArgumentsPattern" -> {_, ___}};

UFDWeights[m_, n_, s_, h_] :=
 CoefficientList[Normal[Series[x^s Log[x]^m, {x, 1, n}]/h^m], x]

TimeDerivative[dr:{__DataRegion}, centering_:Automatic] :=
 Module[{nd, dims, spacing, origin, variable, sorted, times, dt, stencil, offset, deriv, attr, newTime},
  nd = GetNumDimensions/@dr;
  If[Not[Equal@@nd],
   Error["Error, can't compute time derivative from DataRegions with a different number of dimensions."];
   Return[$Failed];
  ];

  dims = GetDimensions/@dr;
  If[Not[Equal@@dims],
   Error["Error, can't compute time derivative from DataRegions with different dimensions."];
   Return[$Failed];
  ];

  spacing = GetSpacing/@dr;
  If[Not[Equal@@spacing],
    Error["Error, can't compute time derivative from DataRegions with different spacings."];
    Return[$Failed];
  ];

  origin = GetSpacing/@dr;
  If[Not[Equal@@origin],
    Error["Error, can't compute time derivative from DataRegions with different origins."];
    Return[$Failed];
  ];

  variable = GetVariableName/@dr;
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

  DataRegion[ attr, GetData[deriv]]
];


(**********************************************************************************)
(* Deprecated functions *)
(**********************************************************************************)

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
GetDimensions[d_DataRegion] := Reverse[Dimensions[d]];
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

(* TODO: generalise this to arbitrary number of dimensions *)
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
