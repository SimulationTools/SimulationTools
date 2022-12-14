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

BeginPackage["SimulationTools`DataRegion`",
 {
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`ProfileCall`",
  "SimulationTools`Utils`",
  If[$VersionNumber >= 10, "GeneralUtilities`", Unevaluated[Sequence[]]]
 }];

DataRegion::usage = "DataRegion[...] is a representation of an N-dimensional array of numbers on a regular grid.";
ToDataRegion::usage = "ToDataRegion[data, origin, spacing] creates a DataRegion object from the N-dimensional array (nested list) data.";
MergedDataRegion::usage = "MergedDataRegion[{dr1, dr2, ..., drn}] creates a DataRegion object by combining each of the DataRegions dr1, dr2, ..., drn.";

VariableName::usage = "VariableName[d] returns the variable name in DataRegion d.";

(****************************************************************)
(* Experimental                                                 *)
(****************************************************************)

(* TODO: Add Metadata function and user-defined metadata *)

TimeDerivative;
CommonPoints;
StripNone;

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

(* Exceptions *)
CoordinateSpacingDimensions;
NoNegativeParts;

Begin["`Private`"];

(******************************************************************************)
(* DataRegion object low-level implementation. This is where we define        *)
(* the internal format of a DataRegion object.                                *)
(******************************************************************************)

DocumentationBuilder`SymbolDescription["DataRegion"] =
  "representation of arbitrary dimensional data on an evenly spaced grid";

SetAttributes[DataRegion, {NHoldFirst, ReadProtected}];
DataRepresentationQ[DataRegion[attrs_, data_]] = True;
attributes[DataRegion[attrs_, data_]] := attrs;
data[DataRegion[attrs_, d_]] := d;

(* Todo: this could be extended *)
validQ[d_DataRegion] :=
  MatchQ[d,DataRegion[{_Rule...}, _List]] &&
  Complement[{"VariableName", "Origin", "Spacing"}, (List@@d)[[1,All,1]]] === {};

(* DataRegions are formatted like SparseArrays *)
Format[d:DataRegion[attrs_List, l_List]] :=
  If[!validQ[d], 
     DataRegion[SequenceForm@@{"<", "invalid", ">"}],
     Module[
       {name, dims, range},
       name = VariableName[d] /. Null -> "<<unnamed>>";
       dims = Dimensions[d];
       range = CoordinateRanges[d];

       DataRegion[ToString[name], SequenceForm@@{"<", Sequence@@Riffle[dims,","], ">"},
                  range]]];

(**********************************************************)
(* CommonInterval                                         *)
(**********************************************************)

CommonInterval[ds:{(_DataRegion)...}] :=
 Module[{mins, maxs},
  mins = Max /@ Transpose[MinCoordinates /@ ds];
  maxs = Min /@ Transpose[MaxCoordinates /@ ds];
  Transpose[{mins, maxs}]
];

(**********************************************************)
(* CoordinateSpacing                                      *)
(**********************************************************)

CoordinateSpacing[d_DataRegion] :=
 Module[{},
  If[ArrayDepth[d] =!= 1,
  	Error[CoordinateSpacingDimensions,
              "CoordinateSpacing can only be used with 1-dimensional DataRegions."];
  ];

  First[CoordinateSpacings[d]]
];


(**********************************************************)
(* CoordinateSpacings                                     *)
(**********************************************************)

CoordinateSpacings[d_DataRegion] := "Spacing" /. attributes[d];


(**********************************************************)
(* CoordinateRanges                                       *)
(**********************************************************)

CoordinateRanges[d_DataRegion] :=
 MapThread[List, {MinCoordinates[d], MaxCoordinates[d]}];


(**********************************************************)
(* Length                                                 *)
(**********************************************************)

DataRegion /: Length[d_DataRegion] :=
 Module[{length},
  If[ArrayDepth[d] =!= 1,
    Error["Length is only defined on 1D DataRegions."];
  ,
    length = Length[ToListOfData[d]];
  ];
  length
];

(**********************************************************)
(* MinCoordinates                                         *)
(**********************************************************)

MinCoordinates[d_DataRegion] := "Origin" /. attributes[d];


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

VariableName[d_DataRegion] := "VariableName" /. attributes[d];


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
  "VariableName" -> Null,
  "Attributes" -> {}};

DocumentationBuilder`OptionDescriptions["ToDataRegion"] = {
"VariableName" -> "A variable name to associate with the DataRegion",
"Attributes" -> "Attributes to associate with the DataRegion"};

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
    {"VariableName" -> OptionValue["VariableName"],
     "Origin" -> SetPrecision[origin, precision],
     "Spacing" -> SetPrecision[spacing, precision],
     Sequence@@OptionValue["Attributes"]
    },
    Developer`ToPackedArray[data]
  ]
];

ToDataRegion[d_SimulationTools`DataTable`DataTable] :=
 Module[{origin, spacing, data},
  origin = MinCoordinates[d];
  spacing = CoordinateSpacings[d];
  data = ToListOfData[d];
  ToDataRegion[Developer`ToPackedArray[data], origin, spacing]
];


(**********************************************************)
(* MergedDataRegion                                       *)
(**********************************************************)

SyntaxInformation[MergedDataRegion] =
 {"ArgumentsPattern" -> {_}};

MergedDataRegion[ds:List[DataRegion[___]..]] :=
 Module[{x1, x2, dx, n, data, offsets, attrs, spacings},
  (* Find the shape of the bounding-box DataRegion *)
  x1 = Min /@ Transpose[MinCoordinates /@ ds];
  x2 = Max /@ Transpose[MaxCoordinates /@ ds];

  (* We allow the spacings to differ by a very small amount, close to roundoff *)
  spacings = CoordinateSpacings /@ ds;
  dx = DeleteCases[
    DeleteDuplicates[spacings, (Max[Abs[1-#1[[1]]/#2[[1]]]]<10^-7)&],
    {0.,_}];

  (* TODO: Check that all origins are separated by multiples of their spacing. *)
  If[Length[dx] =!= 1,
    Error["MergedDataRegion only supports merging DataRegions with the same grid spacing: "<>ToString[NumberForm[spacings,{15,15}]]];
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

  attrs = DeleteCases[GetAttributes[ds[[1]]], ("VariableName" -> _) | ("Spacing" -> _) | ("Origin" -> _)];

  (* Create the new DataRegion *)
  ToDataRegion[Developer`ToPackedArray[data], x1, dx, "VariableName" -> VariableName[ds[[1]]], "Attributes" -> attrs]
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
  {Accumulate, FilterNaNs, UnwrapPhaseVector};

DataRegion /: f_Symbol[x___, d_DataRegion, y___] /;
 MemberQ[$DataRegionFunctions, f] ||
 (MemberQ[Attributes[f], NumericFunction] && MemberQ[Attributes[f], Listable]) :=
 Module[{args, ds, rds},
  ds = Cases[{x, d, y}, _DataRegion, Infinity];
  If[Length[ds] > 1 && !(SameGridQ@@ds),
    rds = Resampled[ds];
    If[rds === $Failed,
      Error[ToString[f]<>" cannot operate on DataRegions with different coordinates. To allow this, use the WithResampling function."];
    ];
    args = ({x, d, y} /. Thread[ds -> rds]) /. dr_DataRegion :> ToListOfData[dr];
  ,
    rds = ds;
    args = {x, d, y} /. dr_DataRegion :> ToListOfData[dr];
  ];
  DataRegion[attributes[First[rds]], f@@args]
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

  ToDataRegion[data, origin, spacing, "VariableName" -> VariableName[d]]
];


(**********************************************************)
(* Part                                                   *)
(**********************************************************)

DataRegion /: Part[d_DataRegion, s__] :=
 Module[{dims, dimensionExists, makeExplicit, data, origin, spacing, result, attrs},

  (* Any dimensions not explicitly mentioned are assumed to be All *)
  partSpec = PadRight[{s}, ArrayDepth[d], All];

  (* Convert negative part specifications to positive ones *)
  dims = Dimensions[d];
  partSpec = MapThread[(#1 /. (n_?Negative :> n+#2+1))&,{partSpec, dims}];

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
      partSpec === None,
       Error["Part specification would yield a DataRegion with no data."];,
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

  Which[
   ArrayDepth[data] === 0,
     result = data;,
   True,
     attrs = DeleteCases[GetAttributes[d], ("VariableName" -> _) | ("Spacing" -> _) | ("Origin" -> _)];
     result = ToDataRegion[data, origin, spacing, "VariableName" -> VariableName[d], "Attributes" -> attrs];
  ];

  result
];


(**********************************************************)
(* Take                                                   *)
(**********************************************************)

DataRegion /: Take[d_DataRegion, s__] :=
 Module[{partSpec},
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
  If[Count[{s}, List[__]] > 0,
    Error["\"List\" part specifications are not currently supported by DataRegion."];
  ];

  partSpec = Map[# /.
    {n_Integer :> If[n<0, Span[1, n-1], Span[n+1, All]],
     None -> All,
     All -> None
    } &, {s}];

  Part[d, Sequence @@ partSpec]
];


(**********************************************************)
(* SameGridQ                                              *)
(**********************************************************)

DataRegion /: SameGridQ[drs:DataRegion[__]..] :=
 Module[{origins, spacings, dims},
  If[Length[{drs}] < 2,
    Error["SameGridQ expects at least two arguments."];
  ];

  origins  = MinCoordinates /@ {drs};
  spacings = CoordinateSpacings /@ {drs};
  dims     = Dimensions /@ {drs};

  (SameQ@@Rationalize[Chop@origins,10^-8]) && (SameQ@@Rationalize[Chop@spacings,10^-8]) && (SameQ@@dims)
];


(**********************************************************)
(* MapList                                                *)
(**********************************************************)

MapList[f_, d_DataRegion, n_:Automatic] :=
 Module[{depth},
  depth = {ArrayDepth[d]};
  If[n =!= Automatic && n =!= depth,
  	Error["Map can only operate on the deepest level in a DataRegion."];
  ];
  DataRegion[attributes[d], Map[f, ToList[d], depth]]
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
  If[Length[ds] > 1 && !(SameGridQ@@ds),
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
(* Plotting functions                                     *)
(**********************************************************)

(* TODO: Provide undocumented workaround functions for the SaveDefinitions/Protected issue? *)

$1DPlotFunctions = {ListPlot, ListLinePlot, ListLogPlot, ListLogLogPlot};
$2DPlotFunctions = {ListDensityPlot, ArrayPlot, ListPlot3D, ListContourPlot};
$3DPlotFunctions = {ListContourPlot3D, ListDensityPlot3D, ListSliceContourPlot3D, ListSliceDensityPlot3D};

DataRegion /: f_Symbol[d_DataRegion, args___] :=
 plotWrapper[f, 1, d, args] /; MemberQ[$1DPlotFunctions, f];

DataRegion /: f_Symbol[d_DataRegion, args___] :=
 plotWrapper[f, 2, d, args] /; MemberQ[$2DPlotFunctions, f];

DataRegion /: f_Symbol[d_DataRegion, args___] :=
 plotWrapper[f, 3, d, args] /; MemberQ[$3DPlotFunctions, f];

plotWrapper[plotFunction_, plotDims_, d_DataRegion, args___] :=
 Module[{ndims, dataRange, data},
  ndims = ArrayDepth[d];
  If[ndims!=plotDims,
    Error[SymbolName[plotFunction]<>" only supports data with dimensionality "<>ToString[plotDims]<>
      ". The provided data has dimensionality "<>ToString[ndims]<>"."];
  ];

  dataRange = If[ndims==1, CoordinateRanges[d][[1]], CoordinateRanges[d]];
  data = ToListOfData[d];

  (* We need to Transpose 2D data *)
  If[ndims == 2, data = Transpose[data]];

  (* We need to Transpose 3D data *)
  If[ndims == 3, data = Transpose[data, {3, 2, 1}]];

  (* ArrayPlot is backwards! *)
  If[plotFunction === ArrayPlot, data = Reverse[data]];

  plotFunction[data, args, DataRange -> dataRange]
];


(**********************************************************)
(* Coordinate                                             *)
(**********************************************************)

Coordinate[d_DataRegion, dir_:Automatic] :=
 Module[{origin, spacing, ndims, dims, max, dim, ca, dm, dp, res},
  ndims   = ArrayDepth[d];
  dims    = Dimensions[d];
  origin  = MinCoordinates[d];
  spacing = CoordinateSpacings[d];
  max     = MaxCoordinates[d];

  If[dir === Automatic,
    If[ndims =!= 1,
      Error["Dimension not specified for "<>ToString[ndims]<>"-dimensional DataRegion"];
    ];
    dim = 1;
  ,
    dim = dir;
  ];

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

  ToDataRegion[res, origin, spacing, "VariableName" -> VariableName[d]]
];


(**********************************************************)
(* NDerivative                                            *)
(**********************************************************)

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

(* TODO: These forms are deprecated and differ from DataTable *)
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

getTime[DataRegion[h_, _]] := "Time" /. h;

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
  sorted = SortBy[dr, getTime];

  (* Check spacing is uniform *)
  times = getTime/@sorted;
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
  attr=replaceRules[attributes[deriv], {"Time"-> newTime, "VariableName" -> "dt_"<>variable[[1]]}];

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
      MakeDataRegion[newData, GetVariableName[d], {}, x1, dx, GetTime[d]],

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
  DataRegion[{"VariableName" -> name, "Origin" -> origin, "Spacing" -> spacing, "Time" -> time},
             Developer`ToPackedArray[Transpose[data, Reverse[Range[ArrayDepth[data]]]]]];

SliceData[v:DataRegion[h_, __], dim_Integer, coord_:0] :=
 Module[{index, newOrigin, newSpacing, origin, spacing, dims, range, slice, ndims, data},
  origin = GetOrigin[v];
  spacing = GetSpacing[v];
  dims = GetDimensions[v];
  ndims = GetNumDimensions[v];
  data = GetData[v];

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
  slice = Sequence @@ Reverse[Join[ConstantArray[All,dim-1],{index},ConstantArray[All,ndims-dim]]];
  MakeDataRegion[data[[slice]], GetVariableName[v], {}, newOrigin, newSpacing, GetTime[v]]
];

SliceData[v_DataRegion, dims_List, coords_:0] := 
  Fold[SliceData[#, Sequence@@#2]&, v, Reverse[SortBy[Thread[{dims, coords}], First]]];

GetData[d_DataRegion] := Transpose[data[d], Reverse[Range[GetNumDimensions[d]]]];
GetAttributes[d_DataRegion] := attributes[d];
GetDimensions[d_DataRegion] := Reverse[Dimensions[GetData[d]]];
GetNumDimensions[d_DataRegion] := ArrayDepth[data[d]];

GetTime[DataRegion[h_, _]] := 
  Which[
    MemberQ[First/@h, "Time"], "Time" /. h,
    MemberQ[First/@h, Time], Time /. h,
    True, Error["No time available in DataRegion"]];

GetOrigin[DataRegion[h_, _]] := 
  Which[
    MemberQ[First/@h, "Origin"], "Origin" /. h,
    MemberQ[First/@h, Origin], Origin /. h,
    True, Error["No origin available in DataRegion"]];

GetSpacing[DataRegion[h_, _]] := 
  Which[
    MemberQ[First/@h, "Spacing"], "Spacing" /. h,
    MemberQ[First/@h, Spacing], Spacing /. h,
    True, Error["No spacing available in DataRegion"]];

GetVariableName[DataRegion[h_, _]] := 
  Which[
    MemberQ[First/@h, "VariableName"], "VariableName" /. h,
    MemberQ[First/@h, VariableName], VariableName /. h,
    True, Error["No variable name available in DataRegion"]];

GetDataRange[v_DataRegion] :=
  Module[{origin, spacing, dimensions, min, max},
    origin = GetOrigin[v];
    spacing = GetSpacing[v];
    dimensions = GetDimensions[v];
    min = origin;
    max = origin + spacing * (dimensions - 1);
    MapThread[List, {min, max}]];

DataRegionPart[d:DataRegion[h_, __], s_]:=
 Module[{ndims, spacing, origin, dataRange, data, newS, indexrange, newOrigin, h2, newData},
  ndims = GetNumDimensions[d];
  spacing = GetSpacing[d];
  origin = GetOrigin[d];
  dataRange = GetDataRange[d];
  data = GetData[d];

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

  MakeDataRegion[newData, GetVariableName[d], {}, newOrigin, spacing, GetTime[d]]
];

DataRegionPart[d_DataRegion, s_Span] := DataRegionPart[d, {s}];

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
  Module[{data, data2, origin, spacing, d2},
    data = GetData[d];
    data2 = Take[data, Apply[Sequence, Map[{#+1,-(#+1)}&, Reverse[n]]]];
    origin = GetOrigin[d];
    spacing = GetSpacing[d];
    d2 = MakeDataRegion[data2, GetVariableName[d], {}, (origin + n * spacing), spacing, GetTime[d]]];

Strip[d_DataRegion, n_List, m_List] :=
  Module[{data, data2, origin, spacing, d2},
    data = GetData[d];
	 data2 = Take[data, Apply[Sequence, MapThread[{#1+1,-(#2+1)}&, {Reverse[n], Reverse[m]}]]];
    origin = GetOrigin[d];
    spacing = GetSpacing[d];
    d2 = MakeDataRegion[data2, GetVariableName[d], {}, (origin + n * spacing), spacing, GetTime[d]]];

MapDataRegion[f_, d_DataRegion] :=
 Module[{dim, data},
  dim = GetNumDimensions[d];
  data = Map[f, GetData[d], {dim}];
  MakeDataRegion[data, GetVariableName[d], {}, GetOrigin[d], GetSpacing[d], GetTime[d]]];

MapThreadDataRegion[f_, drs_] :=
  Module[{datas, dr, newData},
    datas = GetData /@ drs;
    dr = drs[[1]];
    newData = MapThread[f, datas, GetNumDimensions[First[drs]]];
    MakeDataRegion[newData, GetVariableName[dr], {}, GetOrigin[dr], GetSpacing[dr], GetTime[dr]]];

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
   MakeDataRegion[Map[Last, sorted],
    "table", {}, {origin}, {spacing}, 0],
   (* else *)
   subregions =
     Map[TableToDataRegion, Map[Drop[#, {d}] &, split, {2}]];
   s = First[subregions];
   subdims = GetDimensions[s];
   suborigin = GetOrigin[s];
   subspacing = GetSpacing[s];
   MakeDataRegion[Map[GetData, subregions], "table",
    {}, Append[suborigin, origin],
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
    MakeDataRegion[res, GetVariableName[d], {}, origin, spacing, GetTime[d]]
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
 ProfileCall["MergeDataRegions",
 Module[{ndims, origins, dims, spacings, spacing, spacingDiffs,
    X1, X2s, X2, n, dat},
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
  MakeDataRegion[Developer`ToPackedArray[dat], GetVariableName[regions[[1]]], {}, X1, GetSpacing[regions[[1]]], GetTime[regions[[1]]]]]
];

resampled[ds:{DataRegion[__]...}, p_:3] :=
  Module[{x1, x2, dx, grid},
    x1 = Max /@ Transpose[MinCoordinates /@ ds];
    x2 = Min /@ Transpose[MaxCoordinates /@ ds];
    dx = Min /@ Transpose[CoordinateSpacings /@ ds];
    grid = Transpose[{x1, x2, dx}];

    If[Or@@Negative[x2 - x1], Error["Intersection of boxes is empty"]];

    Map[Resampled[#, grid, InterpolationOrder -> p] &, ds]
];

(**********************************************************)
(* dropWhile                                              *)
(**********************************************************)

dropWhile[l_, crit_] :=
 Drop[l, LengthWhile[l, crit]]

(**********************************************************)
(* commonPoints                                           *)
(**********************************************************)

(* TODO: add checks to this function to abort in case of problems *)

commonPoints[{d1_DataRegion, d2_DataRegion}, dim_] :=
 Module[{ox1, ox2, dx1, dx2, rho, m1, m2, eq, soln, nn1, nn2, n1, n2, 
   dims, result, resDims, nPoints1, nPoints2, nPoints},
  dims = Length[Dimensions[d1]];
  ox1 = MinCoordinates[d1][[dim]];
  ox2 = MinCoordinates[d2][[dim]];
  dx1 = CoordinateSpacings[d1][[dim]];
  dx2 = CoordinateSpacings[d2][[dim]];
  rho = Rationalize[dx1/dx2,10^-8.];
  If[! MatchQ[Head[rho], Integer | Rational], 
   Print["rho = ",rho];
   Error["Cannot rationalize grid spacings " <> ToString[dx1] <> 
     " and " <> ToString[dx2]]];
  m1 = Denominator[rho];
  m2 = Numerator[rho];
  eq = Rationalize[(ox1 - ox2)/dx2, 10^-8] == n2 - n1 rho;
  soln = Solve[{eq, n1 >= 0, n2 >= 0}, {n1, n2}, Integers];
  If[Length[soln] === 0, Print["ox1=", ox1]; Print["ox2=", ox2]; 
   Print["dx1=", dx1]; Print["dx2=", dx2]; Print["rho=", rho]; 
   Print["eq=", eq]; Error["No solution"]];
  {nn1, nn2} = {n1, n2} /. 
    First[dropWhile[First[soln] /. Table[{C[1] -> i}, {i, -20, 20}], 
      Cases[#, Undefined, Infinity] =!= {} &]];
  nPoints1 = Floor[(Dimensions[d1][[dim]] - nn1)/m1];
  nPoints2 = Floor[(Dimensions[d2][[dim]] - nn2)/m2];
  nPoints = Min[nPoints1, nPoints2];
  result = {Downsample[
     d1[[Sequence @@ ConstantArray[All, dim - 1], 
       nn1 + 1 ;; (nn1 + nPoints m1), 
       Sequence @@ ConstantArray[All, dims - dim]]], {Sequence @@ 
       ConstantArray[1, dim - 1], m1, 
      Sequence @@ ConstantArray[1, dims - dim]}],
    Downsample[
     d2[[Sequence @@ ConstantArray[All, dim - 1], 
       nn2 + 1 ;; (nn2 + nPoints m2), 
       Sequence @@ ConstantArray[All, dims - dim]]], {Sequence @@ 
       ConstantArray[1, dim - 1], m2, 
      Sequence @@ ConstantArray[1, dims - dim]}]};
  resDims = Dimensions[#][[dim]] & /@ result;
  If[! SameQ @@ resDims,
   Print["m1=", m1];
   Print["m2=", m2];
   Print["nn1=", nn1];
   Print["nn2=", nn2];
   Print["origins=", MinCoordinates[#][[dim]] & /@ result];
   Print[resDims];
   $debug = result;
   Error["Results of commonPoints do not have a common grid"]];
  result];

commonPoints[{d1_DataRegion, d2_DataRegion}] :=
 Fold[commonPoints, {d1, d2}, Range[1, Length[Dimensions[d1]]]];

CommonPoints[ds : {_DataRegion ..}] :=
 Module[{minimal},
  (* Construct a DataRegion with the points common to all *)
  minimal = Fold[commonPoints[{#1, #2}][[1]] &, First[ds], Rest[ds]];
  Map[commonPoints[{minimal, #}][[2]] &, ds]];

(**********************************************************)
(* StripNone                                              *)
(**********************************************************)

StripNone[d_DataRegion, dim_Integer, pos_List] :=
 Module[{origin, delta, ns, parts, data, nPos, nBefore, nAfter, 
   newParts},
  origin = MinCoordinates[d];
  delta = CoordinateSpacings[d];
  ns = Round[(pos - origin)/delta] + 1;
  parts = ReplacePart[ns, dim -> All];
  data = ToListOfData[d][[Sequence @@ parts]];
  nPos = ns[[dim]];
  If[data[[nPos]] === None, Error["None at given position"]];
  nBefore = 
   LengthWhile[Reverse[data[[1 ;; nPos - 1]]], # =!= None &];
  nAfter = LengthWhile[data[[nPos + 1 ;; All]], # =!= None &];
  newParts = 
   ReplacePart[ConstantArray[All, ArrayDepth[d]], 
    dim -> (nPos - nBefore ;; nPos + nAfter)];
  d[[Sequence @@ newParts]]];

StripNone[d_DataRegion, pos_List] :=
 Fold[StripNone[#1, #2, pos] &, d, Range[1, ArrayDepth[d]]];

End[];

EndPackage[];
