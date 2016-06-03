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

BeginPackage["SimulationTools`ReadHDF5`",
 {
  "SimulationTools`Error`",
  "SimulationTools`FileDependencies`"
 }];

ReadHDF5(*::usage = "ReadHDF5[file] provides a wrapper around ImportHDF5 if h5mma is available and falls back to the built-in Import otherwise."*); 
$EnableBuiltInHDF5Reader;
$ReadHDF5Status;
$ReadHDF5ErrorOnFailure = False;

Begin["`Private`"];

(* If the h5mma is not found, then just use Mathematica's built-in HDF5 support *)
$h5mma = If[Quiet[Get["h5mma`"], {Needs::nocont, Get::noopen}]===$Failed, False, True];
If[$h5mma, SetOptions[ImportHDF5, Turbo->True]];

ensureh5mma[] :=
  Module[{links},
    links = Links["*h5mma*"];
    Replace[Length[links], {
      0 :> (Print["Reloading h5mma due to missing link"];
        Get["h5mma`"];
        Print["h5mma reloaded"]),
      i_Integer /; i > 1 :> (Print["ensureh5mma: Found more than one h5mma link; closing all"]; LinkClose/@links; ensureh5mma[]),
      1 :> True,
      _ :> Error["Unrecognised h5mma links expression: "<>ToString[Length[links]]]}]];

ReadHDF5[file_String, opts_:"Datasets"] :=
Module[{result, linkClosed, x, count},
  If[!$h5mma && !($EnableBuiltInHDF5Reader === True),
    Error["The required h5mma package has not been loaded. Make sure it is installed and functioning correctly."];
  ];
  $ReadHDF5Status = {file, opts};
  result = If[$EnableBuiltInHDF5Reader===True, Import[file,opts],

    ensureh5mma[];
    count = 0;
    While[MatchQ[x = CheckAbort[Check[ImportHDF5[file, opts, If[!FreeQ[Options[ImportHDF5], AbortOnMessages], AbortOnMessages -> False, Unevaluated[Sequence[]]]],
      linkClosed,
      {LinkObject::linkn,LinkObject::linkd}], $Aborted], linkClosed|$Aborted],
      Print["x = ", x];

      If[x === linkClosed,
        Print["Link to h5mma may have closed; waiting 10s and retrying"];
        Get["h5mma`"],
        If[x === $Aborted,
          Print["HDF5 read aborted; waiting 10s and retrying"],
          Error["Internal error"]]];
      count = count + 1;
      Pause[10]];

    If[x === linkClosed,
      Print["Unable to read from h5mma after 3 attempts; aborting"];
      $Failed,
      x]];

  If[result === $Failed && $ReadHDF5ErrorOnFailure === True,
    Error["Error importing " <> ToString[opts]<>" from "<>file]];
  DeclareFileDependency[file];
  result
];

End[];
EndPackage[];

(* Add h5mma to $ContextPath since Get[] inside `Private` does not do so. *)
If[SimulationTools`ReadHDF5`Private`$h5mma,
  If[!MemberQ[$ContextPath, "h5mma`"], AppendTo[$ContextPath, "h5mma`"]];
];
