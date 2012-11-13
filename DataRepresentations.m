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

Resampled::usage = "Resampled[d, {{x0, x1, dx}, {y0, y1, dy}, ...}] resamples d to produce a data representation with coordinate ranges {x0, x1}, {y0, y1}, ... and spacings {dx, dy, ...}."<>
  "Resampled[d, {dx, dy, ...}] resamples d onto a grid of the same extent, but with constant spacing dx, dy, ...."<>
  "Resampled[d1, d2] resamples d1 onto the coordinate grid of d2."<>
  "Resampled[{d1, d2, ...}, grid] returns a list of resampled data representations all onto the coordinate grid specified by grid.";
$ResamplingMethod::usage = "$ResamplingMethod is a variable which controls the type of automatic resampling which should be done.";
Downsampled::usage = "Downsampled[d, n] returns a version of d with only every nth element.\n"<>
  "Downsampled[d, {n1, n2, ...nk}] returns a version of d with only every {n1, n2, ...}-th element in the direction k."
Slab::usage = "Slab[d, x1min ;; x1max, ...] gives the hyperslab of d over the coordinate ranges [x1min, x1max], ....";

GridNorm::usage = "GridNorm[d] returns the L2,dx norm of d. This is the discrete approximation to the L2 norm.";

NDerivative::usage = "NDerivative[derivs][d] returns a numerical derivative of d. The derivs argument should be of the same form as in the first argument of Derivative.";

Phase::usage = "Phase[d] gives the phase of the complex variable in DataTable d.  The resulting phase will be continuous for sufficiently smooth input data.";
Frequency::usage = "Frequency[d] returns the first derivative of the complex phase of d.";

(* Exceptions *)
CoordinateRangeDimensions;

(* Deprecated *)
Add;
Div;
Mul;
Sub;

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

SyntaxInformation[Div] =
 {"ArgumentsPattern" -> {_, _, ___}};

Div[d1_SimulationTools`DataRegion`DataRegion, d2_SimulationTools`DataRegion`DataRegion, p_:3] :=
  Apply[Divide, SimulationTools`DataRegion`Private`resampled[{d1, d2}, p]];

Div[d1_SimulationTools`DataTable`DataTable, d2_SimulationTools`DataTable`DataTable, p_:3] :=
  Apply[Divide, SimulationTools`DataTable`Private`resampled[{d1, d2}, p]];

Div[a_?NumberQ, b_?NumberQ] := a/b;


(**********************************************************)
(* Downsampled                                            *)
(**********************************************************)

SyntaxInformation[DownSampled] =
 {"ArgumentsPattern" -> {_, _}};


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

(* TODO: check the continuity algorithm and see if it can be improved *)

phase[tb:List[{_, _}...]] :=
  phase[Map[{#[[1]],{Re[#[[2]]], Im[#[[2]]]}} &, tb]];

phase[tb:{{_, {_, _}}...}] :=
  Module[{phaseTb,x,y,t,previousPhase, i, currentPhase = 0, cycles =
          0, nPoints},
  nPoints = Length[tb];
  phaseTb = Table[i, {i, 1, nPoints}];
  For[i = 1, i <= nPoints, i++,
   t = tb[[i, 1]];
   x = tb[[i, 2, 1]];
   y = tb[[i, 2, 2]];
   currentPhase = If[!(x==0 && y ==0), ArcTan[x, y], 0];
   If[currentPhase - previousPhase > Pi, cycles--];
   If[currentPhase - previousPhase < -Pi, cycles++];
   previousPhase = currentPhase;
   phaseTb[[i]] = {t, 2 Pi cycles + currentPhase}];
  Return[phaseTb]];

(* TODO: clean up Phase implementation *)
(* Phase[d_?DataRepresentationQ] := *)
(*   UnwrapPhaseVector[Arg[d]]; *)

(****************************************************************)
(* UnwrapPhaseVector                                            *)
(****************************************************************)

(* With thanks to Sascha Husa for pointing this out *)
UnwrapPhaseVector[data_List] :=
  (* Juergen Tischer, http://forums.wolfram.com/mathgroup/archive/1998/May/msg00105.html *)
  FoldList[Round[(#1 - #2)/(2 Pi)] 2 Pi + #2 &,
           First[data], 
           Rest[data]];

(**********************************************************)
(* Resampled                                              *)
(**********************************************************)

SyntaxInformation[Resampled] =
 {"ArgumentsPattern" -> {_, ___, OptionsPattern[]}};

Options[Resampled] = {InterpolationOrder -> 8};

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
 Module[{onto, x1, x2, dx},
  Switch[$ResamplingMethod,
   None,
    Return[$Failed];
    Error["Operation requires automatic resampling but $ResamplingMethod is not set"];,
   _?DataRepresentationQ,
    onto = Slab[$ResamplingMethod, Span@@SimulationTools`DataTable`CommonInterval[ds]];,
   "First",
    onto = Slab[First[ds], Span@@SimulationTools`DataTable`CommonInterval[ds]];,
   "Last",
    onto = Slab[Last[ds], Span@@SimulationTools`DataTable`CommonInterval[ds]];,
   "Finest",
    x1 = Max /@ Transpose[MinCoordinates /@ ds];
    x2 = Min /@ Transpose[MaxCoordinates /@ ds];
    If[Or@@Negative[x2 - x1], Error["Intersection of boxes is empty"]];
    (* TODO: this is a bit hackish. There should be a way to do it without knowing
             the data representation type *)
    Switch[Head[ds[[1]]],
     SimulationTools`DataRegion`DataRegion,
      dx = Min /@ Transpose[CoordinateSpacings /@ ds];,
     SimulationTools`DataTable`DataTable,
      dx = {Min[SimulationTools`DataTable`MinCoordinateSpacing /@ ds]};
    ];
    onto = Transpose[{x1, x2, dx}];,
   "Coarsest",
    x1 = Max /@ Transpose[MinCoordinates /@ ds];
    x2 = Min /@ Transpose[MaxCoordinates /@ ds];
    If[Or@@Negative[x2 - x1], Error["Intersection of boxes is empty"]];
    (* TODO: this is a bit hackish. There should be a way to do it without knowing
             the data representation type *)
    Switch[Head[ds[[1]]],
     SimulationTools`DataRegion`DataRegion,
      dx = Max /@ Transpose[CoordinateSpacings /@ ds];,
     SimulationTools`DataTable`DataTable,
      dx = {Max[SimulationTools`DataTable`MaxCoordinateSpacing /@ ds]};
    ];
    onto = Transpose[{x1, x2, dx}];,
   _String,
    Error["Unknown $ResamplingMethod preset: "<>$ResamplingMethod];,
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

Options[Slab] = {
  "Tolerance" -> 0.
};

DocumentationBuilder`OptionDescriptions["Slab"] = {
  "Tolerance" -> "The Tolerance option is currently ignored."
};

SyntaxInformation[Slab] =
 {"ArgumentsPattern" -> {_, __, OptionsPattern[]}};

(* TODO: Implement Tolerance support *)
Slab[d_?DataRepresentationQ, s__, OptionsPattern[]]:=
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
  indexrange = MapThread[(#1 /.{x_?NumericQ :> Round[(x-#2)/#3] + 1})&, {slabSpec, origin, spacing}];

  (* Get the relevant part of the data *)
  Part[d, Sequence@@indexrange]
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


End[];
EndPackage[];
