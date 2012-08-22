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

BeginPackage["DataRepresentations`",
 {
  "Error`",
  "Utils`"
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

(* TODO: Add WithResampling (and WithResampling[order]) which evaluate their argument allowing resampling for algebraic operations.  Use InheritedBlock for this *)
Resampled::usage = "Resampled[d, {{x0, x1, dx}, {y0, y1, dy}, ...}] resamples d to produce a DataRegion with coordinate ranges {x0, x1}, {y0, y1}, ... and spacings {dx, dy, ...}.";
Downsampled::usage = "Downsampled[d, n] returns a version of d with only every nth element.\n"<>
  "Downsampled[d, {n1, n2, ...nk}] returns a version of d with only every {n1, n2, ...}-th element in the direction k."
Slab::usage = "Slab[d, {x1min, x1max}, ...] gives the hyperslab of d specified by the coordinates {x1min, x1max}, ....";

(* TODO: Is there a better system?  These are abbreviations. *)
Add::usage = "Add[d1, d2] adds d1 and d2 after they have been resampled onto the intersection of their bounding boxes.";
Div::usage = "Div[d1, d2] divides d1 by d2 after they have been resampled onto the intersection of their bounding boxes.";
Mul::usage = "Mul[d1, d2] multiplies d1 by d2 after they have been resampled onto the intersection of their bounding boxes.";
Sub::usage = "Sub[d1, d2] subtracts d2 from d1 after they have been resampled onto the intersection of their bounding boxes.";

GridNorm::usage = "GridNorm[d] returns the L2,dx norm of d. This is the discrete approximation to the L2 norm.";

NDerivative::usage = "NDerivative[derivs][d] returns a numerical derivative of d. The derivs argument should be of the same form as in the first argument of Derivative.";

Phase::usage = "Phase[d] gives the phase of the complex variable in DataTable d.  The resulting phase will be continuous for sufficiently smooth input data.";
Frequency::usage = "Frequency[d] returns the first derivative of the complex phase of d.";

(* Exceptions *)
CoordinateRangeDimensions;

Begin["`Private`"];

DataRepresentationQ[_] = False;


(**********************************************************)
(* Add                                                    *)
(**********************************************************)

SyntaxInformation[Add] =
 {"ArgumentsPattern" -> {_, _, ___}};

Add[d1_?DataRepresentationQ, d2_?DataRepresentationQ, p_:3] /; SameQ[Head[d1], Head[d2]] :=
  Apply[Plus, Resampled[{d1, d2}, p]];

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

Div[d1_?DataRepresentationQ, d2_?DataRepresentationQ, p_:3] /; SameQ[Head[d1], Head[d2]] :=
  Apply[Divide, Resampled[{d1, d2}, p]];

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
  If[$NRMMACompatibilityVersion < 1, NDerivative[Phase[d]], NDerivative[1][Phase[d]]];


(**********************************************************)
(* GridNorm                                               *)
(**********************************************************)

SyntaxInformation[GridNorm] =
 {"ArgumentsPattern" -> {_}};

GridNorm[d_?DataRepresentationQ] :=
 Sqrt[Times @@ CoordinateSpacings[d] * Plus @@ Flatten[ToListOfData[d^2]]];


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

Mul[d1_?DataRepresentationQ, d2_?DataRepresentationQ, p_:3] /; SameQ[Head[d1], Head[d2]] :=
  Apply[Times, Resampled[{d1, d2}, p]];

Mul[a_?NumberQ, b_?NumberQ] := a b;


(**********************************************************)
(* NDerivative                                             *)
(**********************************************************)

SyntaxInformation[NDerivative] =
 {"ArgumentsPattern" -> {_, ___}};

Global`StandardDefinition[NDerivative] = True;


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


(**********************************************************)
(* Resampled                                              *)
(**********************************************************)

SyntaxInformation[Resampled] =
 {"ArgumentsPattern" -> {_, ___}};


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

SyntaxInformation[Slab] =
 {"ArgumentsPattern" -> {_, __, OptionsPattern[]}};

(* TODO: Implement Tolerance support *)
Slab[d_?DataRepresentationQ, s__, OptionsPattern[]]:=
 Module[{slabSpec, spacing, origin, endpoints, indexrange},
  spacing   = CoordinateSpacings[d];
  origin    = MinCoordinates[d];
  endpoints = MaxCoordinates[d];

  slabSpec = PadRight[{s}, ArrayDepth[d], All];

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

Sub[d1_?DataRepresentationQ, d2_?DataRepresentationQ, p_:3] /; SameQ[Head[d1], Head[d2]] :=
  Apply[Subtract, Resampled[{d1, d2}, p]];

Sub[a_?NumberQ, b_?NumberQ] := a-b;

(**********************************************************)
(* ToList                                                 *)
(**********************************************************)

Options[ToList] = {"Flatten" -> False};

SyntaxInformation[ToList] =
 {"ArgumentsPattern" -> {_, OptionsPattern[]}};


(**********************************************************)
(* ToListOfCoordinates                                    *)
(**********************************************************)

Options[ToListOfCoordinates] = {"Flatten" -> False};

SyntaxInformation[ToListOfCoordinates] =
 {"ArgumentsPattern" -> {_, OptionsPattern[]}};


(**********************************************************)
(* ToListOfData                                           *)
(**********************************************************)

SyntaxInformation[ToListOfData] =
 {"ArgumentsPattern" -> {_}};


End[];
EndPackage[];
