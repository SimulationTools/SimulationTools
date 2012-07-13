(* Copyright (C) 2012 Ian Hinder and Barry Wardell *)

BeginPackage["DataRepresentations`"];

DataRepresentationQ::usage = "DataRepresentationQ[d] returns True if d is a DataRepresentation type.";

ToList::usage = "ToList[d] returns the contents of d as a List.";
ToListOfData::usage = "ToListOfData[d] returns a List of the data part of d.";
ToListOfCoordinates::usage = "ToListOfCoordinates[d] a List of the coordinates part of d.";

(* TODO: Maybe add CoordinateSpacing? *)
CoordinateSpacings::usage = "CoordinateSpacings[d] gives a List of the spacing of the coordinates in each dimension of d.";
CoordinateRanges::usage = "CoordinateRanges[d] gives the coordinates of the edges of d. This is a list of {min,max} pairs, each corresponding to one dimension of d.";
MinCoordinates::usage = "MinCoordinates[d] returns a list of the coordinates of the first point in each direction in d.";
MaxCoordinates::usage = "MaxCoordinates[d] returns a list of the coordinates of the last point in each direction in d.";
Endpoints::usage = "Endpoints[d] gives the coordinates of the first and last points of d.";

(* TODO: Add WithResampling (and WithResampling[order]) which evaluate their argument allowing resampling for algebraic operations.  Use InheritedBlock for this *)
Resampled::usage = "Resampled[d, {{x0, x1, dx}, {y0, y1, dy}, ...}] resamples d to produce a DataRegion with coordinate ranges {x0, x1}, {y0, y1}, ... and spacings {dx, dy, ...}.";
Downsampled::usage = "Downsampled[d, n] returns a version of d with only every nth element.\n"<>
  "Downsampled[d, {n1, n2, ...nk}] returns a version of d with only every {n1, n2, ...}-th element in the direction k."

GridNorm::usage = "GridNorm[d] returns the L2,dx norm of d. This is the discrete approximation to the L2 norm.";

NDerivative::usage = "NDerivative[derivs][d] returns a numerical derivative of d. The derivs argument should be of the same form as in the first argument of Derivative.\n";

Begin["`Private`"];

DataRepresentationQ[_] = False;


(**********************************************************)
(* Endpoints                                              *)
(**********************************************************)

SyntaxInformation[Endpoints] =
 {"ArgumentsPattern" -> {_}};

Endpoints[d_?DataRepresentationQ] := First[CoordinateRanges[d]];


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
(* Downsampled                                            *)
(**********************************************************)

SyntaxInformation[DownSampled] =
 {"ArgumentsPattern" -> {_, _}};


(**********************************************************)
(* GridNorm                                               *)
(**********************************************************)

SyntaxInformation[GridNorm] =
 {"ArgumentsPattern" -> {_}};

GridNorm[d_?DataRepresentationQ] :=
 Sqrt[Times @@ CoordinateSpacings[d] * Plus @@ Flatten[ToListOfData[d^2]]];


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
