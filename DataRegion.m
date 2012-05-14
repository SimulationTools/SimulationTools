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

Slab::usage = "Slab[d, {{x1min, x1max}, ...}] gives the hyperslab of specified by the coordinates the coordinates {x1min, x1max}, ....";


(* TODO: Add Metadata function and user-defined metadata *)
(* TODO: rationalise plotting functions *)

(* TODO: Use the built-in List function names.  Associate definitions with DataRegion. Provide undocumented workaround functions for the SaveDefinitions/Protected issue? *)
DataRegionDensityPlot::usage = "DataRegionDensityPlot[d, args] generates a DensityPlot of the data in a 2D DataRegion d. The arguments are the same as for DensityPlot.  The DataRange option is unnecessary as it is determined automatically from the DataRegion.";
DataRegionArrayPlot::usage = "DataRegionArrayPlot[d, args] generates an ArrayPlot of the data in a 2D DataRegion d. The arguments are the same as for ArrayPlot.  The DataRange option is unnecessary as it is determined automatically from the DataRegion.";
(* Remove all Dynamic* plotting functions, and replace with a generic Zoomable in Plotting *)
DynamicDataRegionArrayPlot::usage = "DynamicDataRegionArrayPlot[d,args] generates a zoomable version of DataRegionArrayPlot[d,args] which adds the ability to zoom into a region of the plot by dragging a box with the mouse.  Double-click to return to the previous zoom level and right-click to show a button-bar with various options including resetting the plot range to All or Automatic, joining the points in the plot, or using a log scale.";
DataRegionMatrixPlot::usage = "DataRegionMatrixPlot[d, args] generates a MatrixPlot of the data in a 2D DataRegion d. The arguments are the same as for MatrixPlot.  The DataRange option is unnecessary as it is determined automatically from the DataRegion.";
DataRegionPlot3D::usage = "DataRegionPlot3D[d, args] generates a ListPlot3D of the data in a 2D DataRegion d. The arguments are the same as for ListPlot3D.  The DataRange option is unnecessary as it is determined automatically from the DataRegion.";
DataRegionPlot::usage = "DataRegionPlot[d, args] generates a ListPlot of the data in a 1D DataRegion d. The arguments are the same as for ListPlot.  The DataRange option is unnecessary as it is determined automatically from the DataRegion.";

(* TODO: these should be moved into Plotting and probably changed or fixed or deleted *)
ScaledColorFunction::usage = "ScaledColorFunction[colorscheme, {min, max}] returns a function on the domain [min,max] giving a color in colorscheme.  colorscheme can be any Mathematica named color scheme, for example \"ThermometerColors\", \"TemperatureMap\" etc.";
ColorMapLegend::usage = "ColorMapLegend[colorfunction, {min, max, dx}] returns a graphical legend labeled from min to max in steps of dx (which defaults to Automatic) using the color function (e.g. from ScaledColorFunction) to determine the colors.";

(* TODO: remove this function and make it as easy to use the other functions *)
QuickSlicePlot::usage = "QuickSlicePlot[d, {min, max}, colorscheme, opts] generates an array plot of a DataRegion d using the color scheme colorscheme scaled to run between min and max.  The plot includes a legend for the colors.  opts (which is optional) is passed to DataRegionArrayPlot.";
(* TODO: rename as CoordinateOutline *)
Outline::usage = "Outline[d] generates a graphical representation of the outline of a DataRegion d";
(* TODO: Rename as Stripped *)
Strip::usage = "Strip[d,n] removes n points from each face of a DataRegion d.  n is either a list corresponding to the dimension of d or an integer, in which case the same number of points is removed in each direction.
Strip[d,n,m] removes n points from each lower face and m points from each upper face of d.  Here, n and m must be lists.";
(* TODO: add this to ToDataRegion *)
MergeDataRegions::usage = "MergeDataRegions[regions] returns a DataRegion formed from merging the content of the DataRegions in the list regions.  If the regions do not cover a rectangular domain, any missing points will have value None.  All the DataRegions must have the same spacing and their origins must be separated by multiples of their spacing.";
(* TODO: rename this as Coordinates *)
GetCoordinate::usage = "GetCoordinate[d, i] returns a DataRegion of the same shape as the DataRegion d whose data is the i coordinate of d.";
(* TODO: Rename this as MapData *)
(* TODO: Add a MapCoordinates function which maps over the coordinates *)
MapDataRegion::usage = "MapDataRegion[f,d] returns a DataRegion of the same shape as the DataRegion d whose data is f applied to the data in d.";
(* TODO: Move away and undocument *)
FilterNaNs::usage = "FilterNaNs[d] replaces any NaN (Not a Number) values in the DataRegion d with Missing[], which is Mathematica's notation for missing data.";
(* TODO: move this to another package *)
NaNQ::usage = "NaNQ[x] returns True if x is a NaN (Not a Number) value and False if it is not.  Mathematica deals strangely with NaN values imported from other programs.  This function was developed for use with the h5mma package for reading HDF5 data.";
DataRegionContourPlot::usage = "DataRegionContourPlot[d, args] generates a ContourPlot of the data in a 2D DataRegion d. The arguments are the same as for ContourPlot.  The DataRange option is unnecessary as it is determined automatically from the DataRegion.";
(* TODO: rename this as MapThreadData and add a MapThreadCoordinates *)
MapThreadDataRegion::usage = "MapThreadDataRegion[f, {d1, d2, ...}] returns a DataRegion of the same shape as the DataRegions {d1, d2, ...} whose data is f[v1, v2, ...] where {v1, v2, ...} are the data values from {d1, d2, ...}.";
NDerivative::usage = "NDerivative[derivs][d] returns a numerical derivative of the DataRegion d. The derivs argument should be of the same form a
s in the first argument of Derivative.";
(* TODO: don't support this - put it in NRMMA`Experimental *)
TimeDerivative::usage = "TimeDerivative[{d1, d2,...}, center] returns a numerical time derivative computed from DataRegions d1, d2, ... . The derivative is computed using finite differencing, the order of which is determined by the number of DataRegions given. The optional center argument specifies the number of timesteps from the first DataRegion at which to compute derivatives (using lop-sided differencing, if necessary), with the default value being half-way between the first and last times.";
(* TODO: rename this to Resampled and merge with the next one *)
ResampleDataRegion::usage = "ResampleDataRegion[d, {x1, x2, dx}, p] returns a new DataRegion of the same dimensionality as the DataRegion d but with points in the range x1 to x2 with spacing dx.  The data is interpolated at order p onto the new grid.  x1, x2 and dx are lists of length equal to the dimensionality of d.  NOTE: currently only 2-dimensional DataRegions are supported.";
ResampleDataRegions::usage = "ResampleDataRegions[{d1, d2, ...}, p] returns a list of DataRegions {D1, D2, ...} which are resampled versions of the DataRegions {d1, d2, ...} over the intersection of their bounding boxes using interpolation of order p.  The new DataRegions contain the same points.";
(* TODO: Add WithResampling (and WithResampling[order]) which evaluate their argument allowing resampling for algebraic operations.  Use InheritedBlock for this *)
(* TODO: move this to the NRMMA context.  Think about another way to do this *)
Global`Sub::usage = "Sub[d1,d2,p] returns a DataRegion whose data is the subtraction of d1 and d2 after they have been resampled at order p onto the intersection of their bounding boxes.  p is optional and defaults to 3.  Mathematica's infix notation, where a binary function can be written as an infix operator, is useful with this function.  For example, d = d1 ~Sub~ d2.";
(* TODO: Add this to ToDataRegion *)
TableToDataRegion::usage = "TableToDataRegion[list] takes a list of elements each of the form {x,y,..., f} and converts it to a DataRegion. It is assumed (and not checked) that the data grid is regular.";
(* TODO: Rename this as GridNorm *)
NormL2::usage = "NormL2[d] returns the L2,dx norm of the points in the DataRegion d.  This is the discrete approximation to the L2 norm.";
(* TODO: change this to ToDataRegion *)
EvaluateOnDataRegion::usage = "EvaluateOnDataRegion[expr,{t,x,y,z},d] creates a new DataRegion with the same coordinates as the DataRegion d with data computed by evaluating expr assuming that (x,y,z) are the coordinates of d and t is the time of d.  NOTE: this function is only currently implemented for DataRegions of dimension 2 and 3.";
(* TODO: make ToDataRegion more flexible about creating DataRegions from expressions - make it like DataRegionTable *)
(* TODO: sort out context of Downsample *)
(* TODO: add a CoordinateGrid[d] which returns {{min,max,d},...} *)

(* DEPRECATED *)

MakeDataRegion::usage = "MakeDataRegion[data, name, dims, origin, spacing, time] creates a DataRegion object from the passed data.  data is an N-dimensional array (nested list) of numbers.  name is a name to give to the object for display purposes.  dims is a list of length N consisting of the dimensions of data (dims = Reverse[Dimensions[data]]).  origin is a list of length N giving the coordinates of the first point in each direction.  spacing is a list of length N giving the spacing between points in each direction.  time is a number which will be stored with the DataRegion which can be useful for labeling simulation time. DEPRECATED";

SliceData::usage = "SliceData[d, dim, coord] slices the DataRegion d through the dimension dim at the coordinate location coord. The result is a DataRegion with dimensionality 1 lower than that of d. If coord is not given, it uses a default value of 1.";

GetData::usage = "GetData[d] returns the N-dimensional array of data in DataRegion d";
GetAttributes::usage = "GetAttributes[d] returns the list of key -> value attributes of DataRegion d.";
GetDimensions::usage = "GetDimensions[d] returns the number of points in each dimension of a DataRegion d.";
GetNumDimensions::usage = "GetNumDimensions[d] returns the dimensionality of a DataRegion d.";
GetDataRange::usage = "GetDataRange[d] returns the data range of a DataRegion.  This is a list of {min,max} pairs, each corresponding to one dimension of d.";
GetOrigin::usage = "GetOrigin[d] returns a list of length N giving the coordinates of the first point in each direction in the N-dimensional DataRegion d.";
GetSpacing::usage = "GetSpacing[d] returns a list of length N giving the spacing of the data in each dimension of the N-dimensional DataRegion d.";
GetTime::usage = "GetTime[d] returns the time attribute of a DataRegion d";
GetVariableName::usage = "GetVariableName[d] returns the variable name in DataRegion d.";
DataRegionPart::usage = "DataRegionPart[d, {a;;b, c;;d, ...}] gives the part of d which lies between the coordinates a;;b, c;;d, etc.";


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
  name = GetVariableName[d] /. Undefined -> "<<unnamed>>";
  dims = GetDimensions[d];
  range = GetDataRange[d];

  TagBox[
   RowBox[
    {"DataRegion",
     "[",
     RowBox[
      {name,
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
  dims    = GetNumDimensions[d];
  origin  = GetOrigin[d];
  spacing = GetSpacing[d];

  coords = ToListOfData[#, Flatten -> False] & /@
    (GetCoordinate[d, #] & /@ Range[dims]);
  coordList = Transpose[coords, Reverse[Range[dims + 1]]];

  If[OptionValue[Flatten],
    Flatten[coordList, dims-1],
    coordList
  ]
]


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

  list = Transpose[{coords, data}, RotateRight[Range[dims + 1]]];

  If[OptionValue[Flatten],
    Flatten[list, dims-1],
    list
  ]
];

(**********************************************************)
(* Functions which should just see a regular data List    *)
(**********************************************************)
$DataFunctions =
  {ArrayDepth, Dimensions};

DataRegion /: f_Symbol[x___, d_DataRegion, y___] :=
 DataRegion[attributes[d], f[x, ToListOfData[d, Flatten -> False], y]] /;
  (MemberQ[$DataFunctions, f] || MemberQ[Attributes[f], NumericFunction]) &&
  MemberQ[Attributes[f], Listable];

DataRegion /: f_Symbol[x___, d_DataRegion, y___] :=
 f[x, ToListOfData[d, Flatten -> False], y] /;
  MemberQ[$DataFunctions, f] || MemberQ[Attributes[f], NumericFunction]

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
 Module[{slabSpec, spacing, origin, newOrigin, newData},
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

(* DataRegion high-level interface; no assumptions should be made
   about the structure of a DataRegion object here. All access should
   be by the preceding functions.*)


replaceRule[list_List, key_ -> newValue_] :=
  list /. (key -> _) :> (key -> newValue);  

replaceRules[list_List, replacements_List] :=
  If[Length[replacements] == 0, 
     list, 
     replaceRules[replaceRule[list, First[replacements]], Drop[replacements, 1]]];


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
  Return[DataRegion[attrs2, Developer`ToPackedArray[dat]]]]];

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

MapDataRegion[f_, d_DataRegion] :=
 Module[{dim, header, data},
  dim = GetNumDimensions[d];
  header = attributes[d];
  data = Map[f, GetData[d], {dim}];
  DataRegion[header, data]];

FilterNaNs[d_DataRegion] :=
 MapDataRegion[If[NaNQ[#], Missing[], #] &, d];

NaNQ[x_] :=
 Round[x] == -2147483648;

MapThreadDataRegion[f_, drs_] :=
  Module[{attrs, datas, newData},
    attrs = attributes[First[drs]];
    datas = GetData /@ drs;
    newData = MapThread[f, datas, GetNumDimensions[First[drs]]];
    DataRegion[attrs, newData]];

ResampleDataRegion[d_DataRegion, {x1_List, x2_List, dx_List}, p_] :=
  Module[{dFn, newData},
    dFn = Interpolation[d, InterpolationOrder->p];
    If[GetNumDimensions[d] === 2,
      newData = Table[dFn[x,y], {x, x1[[1]], x2[[1]], dx[[1]]},
                                {y, x1[[2]], x2[[2]], dx[[2]]}];
      ToDataRegion[newData, x1, dx, VariableName -> GetVariableName[d], Time -> GetTime[d]],

      Error["Cannot resample DataRegions of dimension other than 2"]]
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

DataRegion/:
Global`Sub[d1_DataRegion, d2_DataRegion,p_:3] :=
  Apply[Subtract, ResampleDataRegions[{d1, d2},p]];

(*******************************************************************************************)
(* Redefine various built-in Mathematica functions to work on DataRegions                  *)
(*******************************************************************************************)

DataRegion /: Abs[d_DataRegion] := MapDataRegion[Abs, d];
DataRegion /: Sqrt[d_DataRegion] := MapDataRegion[Sqrt, d];
 
DataRegion /: Re[d_DataRegion] := MapDataRegion[Re, d];
DataRegion /: Im[d_DataRegion] := MapDataRegion[Im, d];
DataRegion /: Conjugate[d_DataRegion] := MapDataRegion[Conjugate, d];

DataRegion /: Log[d_DataRegion] := MapDataRegion[Log, d];
DataRegion /: Log[b_, d_DataRegion] := MapDataRegion[Log[b,#] &, d];
DataRegion /: Log2[d_DataRegion] := MapDataRegion[Log2, d];
DataRegion /: Log10[d_DataRegion] := MapDataRegion[Log10, d];
DataRegion /: Exp[d_DataRegion] := MapDataRegion[Exp, d];

DataRegion /: Sin[d_DataRegion] := MapDataRegion[Sin, d];
DataRegion /: Cos[d_DataRegion] := MapDataRegion[Cos, d];
DataRegion /: Tan[d_DataRegion] := MapDataRegion[Tan, d];
DataRegion /: Csc[d_DataRegion] := MapDataRegion[Csc, d];
DataRegion /: Sec[d_DataRegion] := MapDataRegion[Sec, d];
DataRegion /: Cot[d_DataRegion] := MapDataRegion[Cot, d];

DataRegion /: ArcSin[d_DataRegion] := MapDataRegion[ArcSin, d];
DataRegion /: ArcCos[d_DataRegion] := MapDataRegion[ArcCos, d];
DataRegion /: ArcTan[d_DataRegion] := MapDataRegion[ArcTan, d];
DataRegion /: ArcCsc[d_DataRegion] := MapDataRegion[ArcCsc, d];
DataRegion /: ArcSec[d_DataRegion] := MapDataRegion[ArcSec, d];
DataRegion /: ArcCot[d_DataRegion] := MapDataRegion[ArcCot, d];
DataRegion /: ArcTan[x_DataRegion, y_DataRegion] := MapThreadDataRegion[ArcTan, {x, y}];

DataRegion /: Sinh[d_DataRegion] := MapDataRegion[Sinh, d];
DataRegion /: Cosh[d_DataRegion] := MapDataRegion[Cosh, d];
DataRegion /: Tanh[d_DataRegion] := MapDataRegion[Tanh, d];
DataRegion /: Csch[d_DataRegion] := MapDataRegion[Csch, d];
DataRegion /: Sech[d_DataRegion] := MapDataRegion[Sech, d];
DataRegion /: Coth[d_DataRegion] := MapDataRegion[Coth, d];

DataRegion /: ArcSinh[d_DataRegion] := MapDataRegion[ArcSinh, d];
DataRegion /: ArcCosh[d_DataRegion] := MapDataRegion[ArcCosh, d];
DataRegion /: ArcTanh[d_DataRegion] := MapDataRegion[ArcTanh, d];
DataRegion /: ArcCsch[d_DataRegion] := MapDataRegion[ArcCsch, d];
DataRegion /: ArcSech[d_DataRegion] := MapDataRegion[ArcSech, d];
DataRegion /: ArcCoth[d_DataRegion] := MapDataRegion[ArcCoth, d];

DataRegion /: Sinc[d_DataRegion] := MapDataRegion[Sinc, d];
DataRegion /: Haversine[d_DataRegion] := MapDataRegion[Haversine, d];
DataRegion /: InverseHaversine[d_DataRegion] := MapDataRegion[InverseHaversine, d];
DataRegion /: Gudermannian[d_DataRegion] := MapDataRegion[Gudermannian, d];
DataRegion /: InverseGudermannian[d_DataRegion] := MapDataRegion[InverseGudermannian, d];

DataRegion /: Total[DataRegion[_,data_]]:=Total[data];
DataRegion /: Max[DataRegion[_,data_]]:=Max[data];
DataRegion /: Min[DataRegion[_,data_]]:=Min[data];
DataRegion /: Mean[DataRegion[_,data_]]:=Mean[data];

DataRegion /: Times[a_, DataRegion[h_,data_]] := DataRegion[h, a data];
DataRegion /: Times[DataRegion[h1_,data1_], DataRegion[h2_,data2_]]:=DataRegion[h1, data1*data2]
DataRegion /: Power[DataRegion[h_,data_], a_] := DataRegion[h, data^a];
DataRegion /: Power[a_, DataRegion[h_,data_]] := DataRegion[h, a^data];
DataRegion /: Power[DataRegion[h1_,data1_], DataRegion[h2_,data2_]] := DataRegion[h1, data1^data2];
DataRegion /: Plus[DataRegion[h1_,data1_], DataRegion[h2_,data2_]]:= DataRegion[h1, data1+data2]
DataRegion /: Plus[DataRegion[h1_,data1_], a_] := DataRegion[h1, data1+a]
DataRegion /: Mod[d_DataRegion, n_] := MapDataRegion[Mod[#, n]&, d];


DataRegion/:Position[DataRegion[h1_,data1_], pattern_, opts___] := Position[data1, pattern, opts];

DataRegion/:Extract[DataRegion[h1_,data1_], positions_List] := Extract[data1, positions];

DataRegion /: Interpolation[v_DataRegion, opts___] :=
  Module[{data = GetData[v], ndims = GetNumDimensions[v]},
    ListInterpolation[Transpose[data,Reverse[Range[ndims]]], GetDataRange[v], opts]
];

NormL2[d_DataRegion] :=
 Sqrt[Times@@GetSpacing[d] * Plus @@ Flatten[GetData[d^2]]];

NDerivative[d:DataRegion[h_,_], dir_Integer] :=
 Module[{ndims},
   ndims   = GetNumDimensions[d];
   (NDerivative@@UnitVector[ndims, dir])[d]
];

NDerivative[d_DataRegion] :=
 Module[{ndims, result},
  ndims = GetNumDimensions[d];

  If[ndims == 1,
   result = NDerivative[d, 1];,
   Error["Must specify a direction when applying NDerivative to a DataRegion of dimension greater than 1."];
   result = $Failed;
  ];

  result
];

NDerivative[derivs__][d:DataRegion[h_,_], opts___] :=
 Module[{origin, spacing, dimensions, grid, data, deriv, newh},
  origin  = GetOrigin[d];
  spacing = GetSpacing[d];
  dimensions = GetDimensions[d];

  (* Get the grid in the form {{x1, ..., xn}, {y1, ..., yn}, ...} *)
  grid = origin + spacing (Range /@ dimensions-1);
  data = GetData[d];

  deriv = NDSolve`FiniteDifferenceDerivative[Reverse[Derivative[derivs]], grid, data, opts];

  newh = replaceRules[h, {VariableName -> "dx"<>ToString[derivs]<>"_"<>GetVariableName[d]}];

  DataRegion[newh, deriv]
];

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

(* We cannot use upvalues here, as the DataRegion appears too deep in
the expression *)

Unprotect[ListLinePlot];

ListLinePlot[ds:List[DataRegion[___]..], opts___] :=
  ListLinePlot[ToList /@ ToDataTable /@ ds, opts];

ListLinePlot[d:DataRegion[___], opts___] :=
   ListLinePlot[{d}, opts];

Protect[ListLinePlot];

Unprotect[ListPlot];

ListPlot[ds:List[DataRegion[___]..], opts___] :=
  ListPlot[ToList /@ ToDataTable /@ ds, opts];

ListPlot[d:DataRegion[___], opts___] :=
   ListPlot[{d}, opts];

Protect[ListPlot];



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
GetDimensions[d_DataRegion] := Dimensions[d];
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

End[];

EndPackage[];
