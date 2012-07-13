(* Copyright (C) 2012 Ian Hinder and Barry Wardell *)

BeginPackage["DataRepresentations`"];

ToList::usage = "ToList[d] returns the contents of d as a List.";
ToListOfData::usage = "ToListOfData[d] returns a List of the data part of d.";
ToListOfCoordinates::usage = "ToListOfCoordinates[d] a List of the coordinates part of d.";

(* TODO: Maybe add CoordinateSpacing? *)
CoordinateSpacings::usage = "CoordinateSpacings[d] gives a List of the spacing of the coordinates in each dimension of d.";
CoordinateRanges::usage = "CoordinateRanges[d] gives the coordinates of the edges of d. This is a list of {min,max} pairs, each corresponding to one dimension of d.";

Downsampled::usage = "Downsampled[d, n] returns a version of d with only every nth element.\n"<>
  "Downsampled[d, {n1, n2, ...nk}] returns a version of d with only every {n1, n2, ...}-th element in the direction k."

NDerivative::usage = "NDerivative[derivs][d] returns a numerical derivative of d. The derivs argument should be of the same form as in the first argument of Derivative.\n";

Begin["`Private`"];

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
(* NDerivative                                             *)
(**********************************************************)

SyntaxInformation[NDerivative] =
 {"ArgumentsPattern" -> {_, ___}};

Global`StandardDefinition[NDerivative] = True;


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
