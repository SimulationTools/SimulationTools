(* ::Package:: *)

BeginPackage["ReadHDF5`", {"Error`"}];

ReadHDF5::usage = "ReadHDF5[file] provides a wrapper around ImportHDF5 if h5mma is available and falls back to the built-in Import otherwise." 
ShowHDF5Progress::usage = "ShowHDF5Progress is a boolean variable indicating whether the progress of HDF5 operations should be shown in a progress indicator. This feature requires h5mma."

Begin["`Private`"];

(* If the h5mma is not found, then just use Mathematica's built-in HDF5 support *)
$h5mma = If[Quiet[Get["h5mma`"], {Get::noopen}]===$Failed, False, True];
If[$h5mma, SetOptions[ImportHDF5, Turbo->True]];

ShowHDF5Progress = False;

ReadHDF5[file_String, opts_:"Datasets"] :=
Module[{tempCell, result, dsIndices},
  If[$h5mma,
    result = ImportHDF5[file, opts];

    If[ShowHDF5Progress === True, NotebookDelete[tempCell]];
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
