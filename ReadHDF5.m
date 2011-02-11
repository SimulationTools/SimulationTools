(* ::Package:: *)

BeginPackage["ReadHDF5`"];

ReadHDF5::usage = "ReadHDF5[file] provides a wrapper around ImportHDF5 if h5mma is available and falls back to the built-in Import otherwise." 
ShowHDF5Progress::usage = "ShowHDF5Progress is a boolean variable indicating whether the progress of HDF5 operations should be shown in a progress indicator. This feature requires h5mma."

Begin["`Private`"];

(* If the h5mma is not found, then just use Mathematica's built-in HDF5 support *)
$h5mma = If[Quiet[Get["h5mma`"]]===$Failed, False, True];
If[$h5mma, SetOptions[ImportHDF5, Turbo->True]];

ShowHDF5Progress = False;

ReadHDF5[file_String, opts___] :=
Module[{tempCell, result},	
  If[$h5mma,
    If[ShowHDF5Progress === True,
      h5mma`ReadDatasetsProgress = 0;
      tempCell = PrintTemporary[Dynamic[Column[{"Scanning " <> Last@FileNameSplit[file],
                   ProgressIndicator[h5mma`ReadDatasetsProgress]}]]]];

    result = ImportHDF5[file, opts];

    If[ShowHDF5Progress === True, NotebookDelete[tempCell]];
  ,
  result = Import[file, opts];
  ];

  If[result == $Failed, Throw["Error importing " <> ToString[{x}]]];

  result
];

End[];
EndPackage[];

(* Add h5mma to $ContextPath since Get[] inside `Private` does not do so. *)
If[ReadHDF5`Private`$h5mma,
  If[!MemberQ[$ContextPath, "h5mma`"], AppendTo[$ContextPath, "h5mma`"]];
];
