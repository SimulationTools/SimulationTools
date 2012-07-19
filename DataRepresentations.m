(* Copyright (C) 2012 Ian Hinder and Barry Wardell *)

BeginPackage["DataRepresentations`",
 {
  "Error`"
 }];

DataRepresentationQ::usage = "DataRepresentationQ[d] returns True if d is a DataRepresentation type.";

ToList::usage = "ToList[d] returns the contents of d as a List.";
ToListOfData::usage = "ToListOfData[d] returns a List of the data part of d.";
ToListOfCoordinates::usage = "ToListOfCoordinates[d] a List of the coordinates part of d.";

(* TODO: Maybe add MapCoordinates, MapThreadCoordinates, MapThreadList *)
(* TODO: Is MapList the right name? *)
MapList::usage = "MapList[f, d] maps f over the coordinates and values in d.";

(* TODO: Maybe add CoordinateSpacing? *)
CoordinateSpacing::usage = "CoordinateSpacings[d] gives the spacing of the coordinates in d.";
CoordinateSpacings::usage = "CoordinateSpacings[d] gives a List of the spacing of the coordinates in each dimension of d.";
CoordinateRanges::usage = "CoordinateRanges[d] gives the coordinates of the edges of d. This is a list of {min,max} pairs, each corresponding to one dimension of d.";
MinCoordinates::usage = "MinCoordinates[d] returns a list of the coordinates of the first point in each direction in d.";
MaxCoordinates::usage = "MaxCoordinates[d] returns a list of the coordinates of the last point in each direction in d.";
Endpoints::usage = "Endpoints[d] gives the coordinates of the first and last points of d.";
CoordinateOutline::usage = "CoordinateOutline[d] generates a graphical representation of the outline of d";
SameGridQ::usage = "SameGridQ[d1, d2] returns True if d1 and d2 are defined on the same coordinate grid.";
Shifted::usage = "Shifted[d, delta] returns a copy of d with the coordinates shifted by delta."; (* TODO: What's the sign convention on delta? *)

(* TODO: Add WithResampling (and WithResampling[order]) which evaluate their argument allowing resampling for algebraic operations.  Use InheritedBlock for this *)
Resampled::usage = "Resampled[d, {{x0, x1, dx}, {y0, y1, dy}, ...}] resamples d to produce a DataRegion with coordinate ranges {x0, x1}, {y0, y1}, ... and spacings {dx, dy, ...}.";
Downsampled::usage = "Downsampled[d, n] returns a version of d with only every nth element.\n"<>
  "Downsampled[d, {n1, n2, ...nk}] returns a version of d with only every {n1, n2, ...}-th element in the direction k."

(* TODO: Is there a better system?  These are abbreviations. *)
Add::usage = "Add[d1, d2] adds d1 and d2 after they have been resampled onto the intersection of their bounding boxes.";
Div::usage = "Div[d1, d2] divides d1 by d2 after they have been resampled onto the intersection of their bounding boxes.";
Mul::usage = "Mul[d1, d2] multiplies d1 by d2 after they have been resampled onto the intersection of their bounding boxes.";
Sub::usage = "Sub[d1, d2] subtracts d2 from d1 after they have been resampled onto the intersection of their bounding boxes.";

GridNorm::usage = "GridNorm[d] returns the L2,dx norm of d. This is the discrete approximation to the L2 norm.";

NDerivative::usage = "NDerivative[derivs][d] returns a numerical derivative of d. The derivs argument should be of the same form as in the first argument of Derivative.";

Begin["`Private`"];

DataRepresentationQ[_] = False;


(**********************************************************)
(* Endpoints                                              *)
(**********************************************************)

SyntaxInformation[Endpoints] =
 {"ArgumentsPattern" -> {_}};

Endpoints[d_?DataRepresentationQ] :=
 Module[{ndim},
  ndim = ArrayDepth[d];
  If[ndim =!= 1,
  	Error["Endpoints should only be used with 1 dimensional data representations."];
  ];

  First[CoordinateRanges[d]]
];

(**********************************************************)
(* Add                                                    *)
(**********************************************************)

SyntaxInformation[Add] =
 {"ArgumentsPattern" -> {_, _, ___}};

Add[d1_?DataRepresentationQ, d2_?DataRepresentationQ, p_:3] /; SameQ[Head[d1], Head[d2]] :=
  Apply[Plus, Resampled[{d1, d2}, p]];

Add[a_?NumberQ, b_?NumberQ] := a+b;


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
(* MinCoordinates                                         *)
(**********************************************************)

SyntaxInformation[MinCoordinates] =
 {"ArgumentsPattern" -> {_}};


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
