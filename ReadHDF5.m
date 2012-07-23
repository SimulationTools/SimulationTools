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

BeginPackage["ReadHDF5`",
 {
  "Error`"
 }];

ReadHDF5(*::usage = "ReadHDF5[file] provides a wrapper around ImportHDF5 if h5mma is available and falls back to the built-in Import otherwise."*); 

Begin["`Private`"];

(* If the h5mma is not found, then just use Mathematica's built-in HDF5 support *)
$h5mma = If[Quiet[Get["h5mma`"], {Get::noopen}]===$Failed, False, True];
If[$h5mma, SetOptions[ImportHDF5, Turbo->True]];

ReadHDF5[file_String, opts_:"Datasets"] :=
Module[{result, dsIndices},
  If[$h5mma,
    result = ImportHDF5[file, opts];
  ,
    (* Deal with the fact that Mathematica requires a dataset index rather than name for Annotations and Dimensions *)
    If[MatchQ[opts, {"Annotations"|"Dimensions",_}],
      dsIndices = dsNamesToIndices[file, opts[[2]]];
      result = Import[file, {opts[[1]], dsIndices}];
    ,
      result = Import[file, opts];
    ];
  ];

  If[result == $Failed, Error["Error importing " <> ToString[opts]<>" from "<>file]];

  result
];

dsNamesToIndices[file_, dsNames_] := Module[{dsList, datasets, indices},
  dsList = If[!ListQ[dsNames], {dsNames}, dsNames];
  datasets = ReadHDF5[file];

  indices = Map[Position[Import[file],#,1,1][[1,1]]&, dsList];
  indices
];

End[];
EndPackage[];

(* Add h5mma to $ContextPath since Get[] inside `Private` does not do so. *)
If[ReadHDF5`Private`$h5mma,
  If[!MemberQ[$ContextPath, "h5mma`"], AppendTo[$ContextPath, "h5mma`"]];
];
