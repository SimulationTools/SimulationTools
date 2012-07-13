(* Copyright (C) 2012 Ian Hinder and Barry Wardell *)

BeginPackage["DataRepresentations`"];

ToList::usage = "ToList[d] returns the contents of d as a List.";
ToListOfData::usage = "ToListOfData[d] returns a List of the data part of d.";
ToListOfCoordinates::usage = "ToListOfCoordinates[d] a List of the coordinates part of d.";

(* TODO: Maybe add CoordinateSpacing? *)
CoordinateSpacings::usage = "CoordinateSpacings[d] gives a List of the spacing of the coordinates in each dimension of d.";

Downsampled::usage = "Downsampled[d, n] returns a version of d with only every nth element.\n"<>
  "Downsampled[d, {n1, n2, ...nk}] returns a version of d with only every {n1, n2, ...}-th element in the direction k."

NDerivative::usage = "NDerivative[derivs][d] returns a numerical derivative of d. The derivs argument should be of the same form as in the first argument of Derivative.\n";

Begin["`Private`"];

(**********************************************************)
(* CoordinateSpacings                                     *)
(**********************************************************)

SyntaxInformation[CoordinateSpacings] =
 {"ArgumentsPattern" -> {_}};


End[];
EndPackage[];
