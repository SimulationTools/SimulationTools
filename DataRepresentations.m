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

BeginPackage["SimulationTools`DataRepresentations`",
 {
  "SimulationTools`Error`",
  "SimulationTools`"
 }];

DataRepresentationQ::usage = "DataRepresentationQ[d] returns True if d is a DataRepresentation type.";

ToList::usage = "ToList[d] returns the contents of d as a List.";
ToListOfData::usage = "ToListOfData[d] returns a List of the data part of d.";
ToListOfCoordinates::usage = "ToListOfCoordinates[d] a List of the coordinates part of d.";

(* TODO: Maybe add MapCoordinates, MapThreadCoordinates, MapThreadList *)
MapList::usage = "MapList[f, d] maps f over the coordinates and values in d.";

CoordinateSpacing::usage = "CoordinateSpacings[d] gives the spacing of the coordinates in d.";
CoordinateSpacings::usage = "CoordinateSpacings[d] gives a List of the spacing of the coordinates in each dimension of d.";
CoordinateRange::usage = "CoordinateRange[d] gives the coordinates of the first and last points of d.";
CoordinateRanges::usage = "CoordinateRanges[d] gives the coordinates of the edges of d. This is a list of {min,max} pairs, each corresponding to one dimension of d.";
MinCoordinate::usage = "MinCoordinate[d] returns the coordinate of the first point in in d.";
MinCoordinates::usage = "MinCoordinates[d] returns a list of the coordinates of the first point in each direction in d.";
MaxCoordinate::usage = "MaxCoordinate[d] returns the coordinate of the last point in in d.";
MaxCoordinates::usage = "MaxCoordinates[d] returns a list of the coordinates of the last point in each direction in d.";
CoordinateOutline::usage = "CoordinateOutline[d] generates a graphical representation of the outline of d";
Coordinate::usage = "Coordinate[d, i] returns a data representation of the same shape as d whose data is the i coordinate of d.";
SameGridQ::usage = "SameGridQ[d1, d2] returns True if d1 and d2 are defined on the same coordinate grid.";
Shifted::usage = "Shifted[d, delta] returns a copy of d with the coordinates shifted by delta.";

CoordinateAtMax::usage = "CoordinateAtMax[d] finds the coordinate at which a maximum occurs in d. This is guaranteed to coincide with a data point.";
CoordinatesAtMax::usage = "CoordinatesAtMax[d] finds a list of the coordinates at which the maximum occurs in d. This is guaranteed to coincide with a data point.";

Resampled::usage = "Resampled[d, {{x0, x1, dx}, {y0, y1, dy}, ...}] resamples d to produce a data representation with coordinate ranges {x0, x1}, {y0, y1}, ... and spacings {dx, dy, ...}.\n"<>
  "Resampled[d, {dx, dy, ...}] resamples d onto a grid of the same extent, but with constant spacing dx, dy, ....\n"<>
  "Resampled[d1, d2] resamples d1 onto the coordinate grid of d2.\n"<>
  "Resampled[{d1, d2, ...}, grid] returns a list of resampled data representations all onto the coordinate grid specified by grid.";
WithResampling::usage = "WithResampling[method, expr] evaluates expr with automatic resampling enabled.";
Slab::usage = "Slab[d, x1min ;; x1max, ...] gives the hyperslab of d over the coordinate ranges [x1min, x1max], ....";
(* TODO: Slab should check the types of its range arguments. At the moment, it returns invalid datatables *)

GridNorm::usage = "GridNorm[d] returns the L2,dx norm of d. This is the discrete approximation to the L2 norm.";

NDerivative::usage = "NDerivative[derivs][d] returns a numerical derivative of d. The derivs argument should be of the same form as in the first argument of Derivative.";

Phase::usage = "Phase[d] gives the phase of the complex variable in d.  The resulting phase will be continuous for sufficiently smooth input data.";
Frequency::usage = "Frequency[d] returns the first derivative of the complex phase of d.";

If[$VersionNumber < 9,
  Downsample::usage = "Downsample[d, n] returns a version of d with only every nth element.\n"<>
    "Downsample[d, {n1, n2, ...nk}] returns a version of d with only every {n1, n2, ...}-th element in the direction k."
];

(* Experimental *)
CommonInterval;
UnwrapPhaseVector;

(* Exceptions *)
CoordinateRangeDimensions;

(* Deprecated *)
Add;
Div;
Mul;
Sub;
Downsampled;

Begin["`Private`"];

DataRepresentationQ[_] = False;


(**********************************************************)
(* Add                                                    *)
(**********************************************************)

SyntaxInformation[Add] =
 {"ArgumentsPattern" -> {_, _, ___}};

Add[d1_SimulationTools`DataRegion`DataRegion, d2_SimulationTools`DataRegion`DataRegion, p_:3] :=
  Apply[Plus, SimulationTools`DataRegion`Private`resampled[{d1, d2}, p]];

Add[d1_SimulationTools`DataTable`DataTable, d2_SimulationTools`DataTable`DataTable, p_:3] :=
  Apply[Plus, SimulationTools`DataTable`Private`resampled[{d1, d2}, p]];

Add[a_?NumberQ, b_?NumberQ] := a+b;


(**********************************************************)
(* Coordinate                                             *)
(**********************************************************)

SyntaxInformation[Coordinate] =
 {"ArgumentsPattern" -> {_, ___}};


(****************************************************************)
(* CoordinateAtMax                                              *)
(****************************************************************)

SyntaxInformation[CoordinateAtMax] =
 {"ArgumentsPattern" -> {_}};

CoordinateAtMax[d_?DataRepresentationQ] :=
 Module[{coord},
  coord = CoordinatesAtMax[d];

  If[Length[coord] =!= 1,
    Error["Multiple maxima found."];
  ];
  First[coord]
]


(****************************************************************)
(* CoordinatesAtMax                                              *)
(****************************************************************)

SyntaxInformation[CoordinatesAtMax] =
 {"ArgumentsPattern" -> {_}};

CoordinatesAtMax[d_?DataRepresentationQ] :=
  Extract[ToListOfCoordinates[d], Position[d, Max[d]]];


(**********************************************************)
(* CoordinateOutline                                      *)
(**********************************************************)

SyntaxInformation[CoordinateOutline] =
 {"ArgumentsPattern" -> {_}};

CoordinateOutline[d_?DataRepresentationQ] :=
 Module[{ndims, coords, shapes},
  ndims = ArrayDepth[d];

  If[ndims > 3,
    Error["Dimension "<>ToString[ndims]<>" of data representation not supported by Outline."]
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
(* CoordinateRange                                        *)
(**********************************************************)

SyntaxInformation[CoordinateRange] =
 {"ArgumentsPattern" -> {_}};

CoordinateRange[d_?DataRepresentationQ] :=
 Module[{ndim},
  ndim = ArrayDepth[d];
  If[ndim =!= 1,
    Error[CoordinateRangeDimensions, 
          "CoordinateRange should only be used with 1 dimensional data representations."];
  ];

  First[CoordinateRanges[d]]
];


(**********************************************************)
(* CoordinateRanges                                       *)
(**********************************************************)

SyntaxInformation[CoordinateRanges] =
 {"ArgumentsPattern" -> {_}};


(**********************************************************)
(* CoordinateSpacings                                     *)
(**********************************************************)

SyntaxInformation[CoordinateSpacings] =
 {"ArgumentsPattern" -> {_}};


(**********************************************************)
(* CoordinateSpacing                                      *)
(**********************************************************)

SyntaxInformation[CoordinateSpacing] =
 {"ArgumentsPattern" -> {_}};


(**********************************************************)
(* Div                                                    *)
(**********************************************************)

If[$VersionNumber < 9,

SyntaxInformation[Div] =
 {"ArgumentsPattern" -> {_, _, ___}};

Div[d1_SimulationTools`DataRegion`DataRegion, d2_SimulationTools`DataRegion`DataRegion, p_:3] :=
  Apply[Divide, SimulationTools`DataRegion`Private`resampled[{d1, d2}, p]];

Div[d1_SimulationTools`DataTable`DataTable, d2_SimulationTools`DataTable`DataTable, p_:3] :=
  Apply[Divide, SimulationTools`DataTable`Private`resampled[{d1, d2}, p]];

Div[a_?NumberQ, b_?NumberQ] := a/b;

]

(**********************************************************)
(* Downsample                                             *)
(**********************************************************)

downsample[d_, n_Integer] :=
 Module[{ndims},
  ndims = ArrayDepth[d];
  downsample[d, ConstantArray[n, ndims]]
];

downsample[d_, n_List] :=
  Take[d, Apply[Sequence, Transpose[{ConstantArray[1, ArrayDepth[d]], Dimensions[d], n}]]];

SimulationTools`DataRegion`DataRegion /:
 Downsample[d_SimulationTools`DataRegion`DataRegion, n_] :=
  Module[{nn},
  nn = If[ListQ[n] && $SimulationToolsCompatibilityVersion < 1, Reverse[n], n];
  downsample[d, nn]
];

SimulationTools`DataTable`DataTable /:
  Downsample[d_SimulationTools`DataTable`DataTable, n_] := downsample[d, n];

If[$VersionNumber < 9,
Downsample[l_List, n_Integer] :=
  Take[l, {1, Length[l], n}]];

(**********************************************************)
(* First                                                  *)
(**********************************************************)

Unprotect[First];

First[d_?DataRepresentationQ] := d[[1]];

Protect[First];


(****************************************************************)
(* Frequency                                                    *)
(****************************************************************)

SyntaxInformation[Frequency] =
 {"ArgumentsPattern" -> {_}};

(* TODO: Since this uses NDerivative, we should be able to specify the method and order of accuracy *)
Frequency[d_?DataRepresentationQ] :=
  If[$SimulationToolsCompatibilityVersion < 1, NDerivative[Phase[d]], NDerivative[1][Phase[d]]];


(**********************************************************)
(* GridNorm                                               *)
(**********************************************************)

SyntaxInformation[GridNorm] =
 {"ArgumentsPattern" -> {_}};

GridNorm[d_?DataRepresentationQ] :=
 Sqrt[Times @@ CoordinateSpacings[d] * Plus @@ Flatten[ToListOfData[Abs[d]^2]]];


(**********************************************************)
(* Last                                                   *)
(**********************************************************)

Unprotect[Last];

Last[d_?DataRepresentationQ] := d[[-1]];

Protect[Last];


(****************************************************************)
(* MapList *)
(****************************************************************)

SyntaxInformation[MapList] =
 {"ArgumentsPattern" -> {_, _}};


(**********************************************************)
(* MinCoordinate                                          *)
(**********************************************************)

SyntaxInformation[MinCoordinate] =
 {"ArgumentsPattern" -> {_}};

MinCoordinate[d_?DataRepresentationQ] :=
 Module[{},
  If[ArrayDepth[d] =!= 1,
    Error["MinCoordinate can only be used with 1-dimensional data representations."];
  ];

  First[MinCoordinates[d]]
];


(**********************************************************)
(* MinCoordinates                                         *)
(**********************************************************)

SyntaxInformation[MinCoordinates] =
 {"ArgumentsPattern" -> {_}};


(**********************************************************)
(* MaxCoordinate                                          *)
(**********************************************************)

SyntaxInformation[MaxCoordinate] =
 {"ArgumentsPattern" -> {_}};

MaxCoordinate[d_?DataRepresentationQ] :=
 Module[{},
  If[ArrayDepth[d] =!= 1,
    Error["MaxCoordinate can only be used with 1-dimensional data representations."];
  ];

  First[MaxCoordinates[d]]
];


(**********************************************************)
(* MaxCoordinates                                         *)
(**********************************************************)

SyntaxInformation[MaxCoordinates] =
 {"ArgumentsPattern" -> {_}};


(**********************************************************)
(* Mul                                                    *)
(**********************************************************)

SyntaxInformation[Mul] =
 {"ArgumentsPattern" -> {_, _, ___}};

Mul[d1_SimulationTools`DataRegion`DataRegion, d2_SimulationTools`DataRegion`DataRegion, p_:3] :=
  Apply[Times, SimulationTools`DataRegion`Private`resampled[{d1, d2}, p]];

Mul[d1_SimulationTools`DataTable`DataTable, d2_SimulationTools`DataTable`DataTable, p_:3] :=
  Apply[Times, SimulationTools`DataTable`Private`resampled[{d1, d2}, p]];

Mul[a_?NumberQ, b_?NumberQ] := a b;


(**********************************************************)
(* NDerivative                                             *)
(**********************************************************)

SyntaxInformation[NDerivative] =
 {"ArgumentsPattern" -> {_, ___}};

SimulationTools`ArgumentChecker`StandardDefinition[NDerivative] = True;


(****************************************************************)
(* Phase                                                        *)
(****************************************************************)

SyntaxInformation[Phase] =
 {"ArgumentsPattern" -> {_}};

Phase[d_?DataRepresentationQ] :=
  UnwrapPhaseVector[Arg[d]];

Phase[d_?DataRepresentationQ] /; ArrayDepth[d] > 1 :=
  Error["Cannot compute the phase of a data representation with dimension greater than 1."];


(****************************************************************)
(* UnwrapPhaseVector                                            *)
(****************************************************************)

(* The current implementation is an optimisation of the following *)
(* With thanks to Sascha Husa for pointing this out *)
(* Juergen Tischer, http://forums.wolfram.com/mathgroup/archive/1998/May/msg00105.html *)
(*
UnwrapPhaseVector[data_List] :=
  FoldList[Round[(#1 - #2)/(2 Pi)] 2 Pi + #2 &,
           First[data], 
           Rest[data]];
*)

(* Only compile this when it is first used. This both speeds up the package
   loading and avoids problems caused by our argument checker. *)
unwrapPhaseVector := unwrapPhaseVector = Compile[{{data, _Real, 1}},
 Module[{diffs, corr, cumulcorr},
  (* Compute the differences between successive points *)
  diffs = Differences[data];

  (* Add a jump of 2 Pi each time the difference is between
     successive points is greater than Pi *)
  corr = Round[diffs/(2 Pi)];
  cumulcorr = (- 2 Pi) Accumulate[corr];

  (* Add the corrections to the original data *)
  Join[data[[{1}]], data[[2 ;; -1]] + cumulcorr]
 ], CompilationTarget -> "C", RuntimeOptions -> "Speed"];

UnwrapPhaseVector[data_List] := unwrapPhaseVector[data];


(**********************************************************)
(* Plotting functions with Lists of DataRepresentations   *)
(**********************************************************)

(* We cannot use UpValues as the DataRepresentations appear
   too deep. We also need to ensure that our new definitions
   appear ahead of built-in ones *)
sortedOverload[sym_] :=
 Module[{tmp},
  SimulationTools`ArgumentChecker`StandardDefinition[tmp] = True;
  tmp[ds:List[(_?DataRepresentationQ)..], opts___] := sym[ToList /@ ds, opts];
  ClearAttributes[sym, {Protected, ReadProtected}];
  DownValues[sym] = Join[DownValues[tmp] /. tmp -> sym, DownValues[sym]];
  SetAttributes[sym, {Protected, ReadProtected}];
]

$1DPlotFunctions = {ListPlot, ListLinePlot, ListLogPlot, ListLogLogPlot};

Scan[sortedOverload, $1DPlotFunctions];


(**********************************************************)
(* Resampled                                              *)
(**********************************************************)

SyntaxInformation[Resampled] =
 {"ArgumentsPattern" -> {_, ___, OptionsPattern[]}};

Options[Resampled] = {"InterpolationOrder" -> 8};

DocumentationBuilder`OptionDescriptions["Resampled"] = {
  InterpolationOrder -> "The order of interpolation to use. This may be take value "<>
    "supported by the Interpolation function. The default is 8."
};

(* Resample a list of data representations onto the same grid *)
Resampled[ds:{(_?DataRepresentationQ)...}, grid_, opts___] /; SameQ[Head/@ds] :=
  Map[Resampled[#, grid, opts]&, ds];

(* Resample onto a grid with the same range, but with new grid spacings *)
Resampled[d_?DataRepresentationQ, dxs:{(_?NumericQ)...}, opts___] :=
 Module[{coords, grids},
  coords = CoordinateRanges[d];
  grids = MapThread[Append, {coords, dxs}];
  Resampled[d, grids, opts]
];

(* Resample onto the grid of another data representation *)
Resampled[d_?DataRepresentationQ, onto_?DataRepresentationQ, OptionsPattern[]] :=
 Module[{f},
  f = Interpolation[d, InterpolationOrder -> OptionValue["InterpolationOrder"]];
  MapList[Apply[f, Drop[#, -1]]&, onto]
];

(* Resample onto a grid specified by {x_min, x_max, dx} in each direction *)
Resampled[d_?DataRepresentationQ, grid:{{_?NumericQ, _?NumericQ, _?NumericQ}...}, OptionsPattern[]] :=
 Module[{dims, interp, vars, tmp, iterators, data, res},
  dims = Dimensions[grid];
  If[dims =!= {ArrayDepth[d], 3},
  	Error["Expected a list of triples for each dimension in the data representation."];
  ];
  interp = Interpolation[d, InterpolationOrder -> OptionValue["InterpolationOrder"]];
  vars = tmp /@ Range[ArrayDepth[d]];
  iterators = MapThread[Join[{#1}, #2] &, {vars, grid}];

  (* TODO: Make this not depend on the type of data representation. *)
  Switch[Head[d],
   SimulationTools`DataRegion`DataRegion,
    data = Table[interp @@ vars, Evaluate[Sequence @@ iterators]];
    res = SimulationTools`DataRegion`ToDataRegion[data, grid[[All, 1]], grid[[All, 3]], "VariableName" -> SimulationTools`DataRegion`VariableName[d]];,
   SimulationTools`DataTable`DataTable,
    data = Table[Join[vars, {interp @@ vars}], Evaluate[Sequence @@ iterators]];
    res = SimulationTools`DataTable`ToDataTable[data];,
   _,
    Error["Can not resample an object of type "<>ToString[Head[d]]<>"."];
  ];

  res
];


If[!ValueQ[$ResamplingMethod], $ResamplingMethod = None];
Resampled[ds:{(_?DataRepresentationQ)...}, opts:OptionsPattern[]] /; SameQ[Head/@ds] :=
 Module[{onto},
  Switch[$ResamplingMethod,
   None,
    Return[$Failed];
    Error["Operation requires automatic resampling but a resampling method is not set"];,
   _?DataRepresentationQ,
    onto = Slab[$ResamplingMethod, Sequence@@(Span@@@CommonInterval[ds])];,
   "First",
    onto = Slab[First[ds], Sequence@@(Span@@@CommonInterval[ds])];,
   "Last",
    onto = Slab[Last[ds], Sequence@@(Span@@@CommonInterval[ds])];,
   _String,
    Error["Unknown resampling method: "<>$ResamplingMethod];,
   _,
    onto = $ResamplingMethod[ds];
  ];

  Resampled[ds, onto, opts]
];


(**********************************************************)
(* SameGridQ                                              *)
(**********************************************************)

SyntaxInformation[SameGridQ] =
 {"ArgumentsPattern" -> {_, __}};


(**********************************************************)
(* Slab                                                   *)
(**********************************************************)

SyntaxInformation[Slab] =
 {"ArgumentsPattern" -> {_, __}};

Slab[d_SimulationTools`DataTable`DataTable, s__] :=
  If[!SimulationTools`DataTable`UniformSpacingQ[d], slabnu[d, s], slab[d, s]];

Slab[d_SimulationTools`DataRegion`DataRegion, s__] := slab[d, s];

slab[d_?DataRepresentationQ, s__]:=
 Module[{slabSpec, valid, spacing, origin, endpoints, indexrange},
  spacing   = CoordinateSpacings[d];
  origin    = MinCoordinates[d];
  endpoints = MaxCoordinates[d];

  slabSpec = PadRight[{s}, ArrayDepth[d], All];

  valid = Apply[And, Map[((#===All)||(Head[#]===Span)||(NumericQ[#])||MatchQ[#, {_?NumericQ}])&, slabSpec]];
  If[!valid,
    Error["Coordinate ranges must be specified using either Span[x1, x2] "<>
     "(equivalently x1;;x2), All, a single coordinate or a single element list."];
  ];

  (* Convert coordinate range to index range *)
  indexrange = MapThread[(#1 /.{x_?NumericQ :> Round[(x-#2)/#3] + 1})&, {slabSpec, origin, spacing}]; (* FIXME: Add conversion from Span[..,All] *)

  (* Get the relevant part of the data *)
  Part[d, Sequence@@indexrange]
];

slabnu[d_SimulationTools`DataTable`DataTable, s__] /; !SimulationTools`DataTable`UniformSpacingQ[d] :=
 Module[{t, spacings, tleft, tright},
  t = ToListOfCoordinates[d]; (* FIXME: Add conversion from Span[..,All] *)

  (* We allow a tolerance of half the local grid spacing *)
  spacings = 0.5 Differences[t];
  tright = t - ArrayPad[spacings, {1, 0}, spacings[[1]]];
  tleft = t + ArrayPad[spacings, {0, 1}, spacings[[-1]]];

  Pick[d, Sign[tleft - s[[1]]] + Sign[s[[2]] - tright], 2]
];


(****************************************************************)
(* Shifted                                                      *)
(****************************************************************)

SyntaxInformation[Shifted] =
 {"ArgumentsPattern" -> {_, _}};


(**********************************************************)
(* Sub                                                    *)
(**********************************************************)

SyntaxInformation[Sub] =
 {"ArgumentsPattern" -> {_, _, ___}};

Sub[d1_SimulationTools`DataRegion`DataRegion, d2_SimulationTools`DataRegion`DataRegion, p_:3] :=
  Apply[Subtract, SimulationTools`DataRegion`Private`resampled[{d1, d2}, p]];

Sub[d1_SimulationTools`DataTable`DataTable, d2_SimulationTools`DataTable`DataTable, p_:3] :=
  Apply[Subtract, SimulationTools`DataTable`Private`resampled[{d1, d2}, p]];

Sub[a_?NumberQ, b_?NumberQ] := a-b;

(**********************************************************)
(* ToList                                                 *)
(**********************************************************)

Options[ToList] = {"Flatten" -> False};

DocumentationBuilder`OptionDescriptions["ToList"] = {
  "Flatten" -> "When set to True, ToList operating on data representations of "<>
    "dimension greater than one will produce a flattened list of values."
};

SyntaxInformation[ToList] =
 {"ArgumentsPattern" -> {_, OptionsPattern[]}};


(**********************************************************)
(* ToListOfCoordinates                                    *)
(**********************************************************)

Options[ToListOfCoordinates] = {"Flatten" -> False};

DocumentationBuilder`OptionDescriptions["ToListOfCoordinates"] = {
  "Flatten" -> "When set to True, ToListOfCoordinates operating on data representations of "<>
    "dimension greater than one will produce a flattened list of values."
};

SyntaxInformation[ToListOfCoordinates] =
 {"ArgumentsPattern" -> {_, OptionsPattern[]}};


(**********************************************************)
(* ToListOfData                                           *)
(**********************************************************)

SyntaxInformation[ToListOfData] =
 {"ArgumentsPattern" -> {_}};


(**********************************************************)
(* WithResampling                                         *)
(**********************************************************)

SyntaxInformation[WithResampling] =
 {"ArgumentsPattern" -> {_, ___}};

SetAttributes[WithResampling, HoldAll];
WithResampling[method_, expr_] :=
 Block[{$ResamplingMethod = method}, expr];

 WithResampling[expr_] :=
  Block[{$ResamplingMethod = "First"}, expr];


(**********************************************************)
(* Deprecated                                             *)
(**********************************************************)

Downsampled[d_?DataRepresentationQ, n_] := downsample[d, n];


End[];
EndPackage[];
