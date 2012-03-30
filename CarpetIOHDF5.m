BeginPackage["CarpetIOHDF5`", {"RunFiles`"}];

Begin["`Private`"];

(* We will have "Read*" and "Default*" provided functions.  The
   "Default*" functions are optional, but if they are not provided,
   then Automatic is not allowed.  The Default* functions are given
   the run and are allowed to call runfiles, as is ToFileName, but all
   the other provided functions are given the filename from the start.
   The conversion from Automatic to the default is then done by the
   high level interface by calling the Default* provided function. *)

CarpetIOHDF5`GridFunctions`ToFileName[var_String, dims_List, opts:OptionsPattern[]] :=
  Module[
    {map},
	map = Switch[OptionValue[Map],
		 _Integer, "."<>ToString[OptionValue[Map]],
		 None, "",
		 All, "(\.\d){0,1}",
		 _, Error["Unrecognised map "<>ToString[map,InputForm]]];

    var <> map <> StringJoin[dims] <> ".h5"
];

Options[CarpetIOHDF5`GridFunctions`ReadData] = {
    "Iteration" 	  -> Undefined,
    "Map" 			  -> Undefined,
    "RefinementLevel" -> Undefined,
    "TimeLevel" 	  -> Undefined,
    "Variable" 		  -> Undefined,
    "StripGhostZones" -> True
  };

CarpetIOHDF5`GridFunctions`ReadData[fileName_String, opts:OptionsPattern[]] :=
  ReadCarpetHDF5Variable[filename, OptionValue[Variable],
    OptionValue[Iteration], OptionValue[RefinementLevel], OptionValue[Map],
    FilterRules[{opts}, Options[ReadCarpetHDF5Variable]]];

Options[CarpetIOHDF5`GridFunctions`ReadIterations] =
  FilterRules[Options[CarpetIOHDF5`GridFunctions`ReadData], Except["Iteration"]];
CarpetIOHDF5`GridFunctions`ReadIterations[fileName_String, opts:OptionsPattern[]] :=
  datasetAttribute[file, 2];

Options[CarpetIOHDF5`GridFunctions`ReadMaps] =
  FilterRules[Options[CarpetIOHDF5`GridFunctions`ReadData], Except["Map"]];
DefineMemoFunction[CarpetIOHDF5`GridFunctions`ReadMaps[fileName_String, opts:OptionsPattern[]],
   datasetAttribute[file, 6]
];

(***************************************************************************************)
(* Defaults                                                                            *)
(***************************************************************************************)

CarpetIOHDF5`GridFunctions`DefaultIteration[its_List] := 0;

CarpetIOHDF5`GridFunctions`DefaultMap[maps_List] :=
  Module[
    {maps, map},
    maps = Map[StringCases[#,var~~"."~~map_~~"."<>StringJoin[dims]<>".h5" :> map] &,
      DeleteDuplicates[FindRunFilesFromPattern[run,var<>"\.*\."<>StringJoin[dims]<>"\.h5"]]];
    map = onlyOrError[maps]
];

CarpetIOHDF5`GridFunctions`DefaultTimeLevel[run_, var_] := 0;

(***************************************************************************************)
(* Private functions                                                                   *)
(***************************************************************************************)


(***************************************************************************************)
(* Low-level interface for determining what's available in a file *)
(***************************************************************************************)

DefineMemoFunction[datasetAttributes[file_],
  Module[{attributeRules, datasets, dsattrs},
    Profile["datasetAttributes[file]",

    (* Convert dataset name strings into rules *)
    datasets = Profile["Datasets", Datasets[file]];

    Profile["datasetAttributes/StringCases",
     attributeRules = StringCases[datasets,
     {"it=" ~~ x : NumberString :> ("it" -> ToExpression[x]),
      "tl=" ~~ x : NumberString :> ("tl" -> ToExpression[x]),
      "rl=" ~~ x : NumberString :> ("rl" -> ToExpression[x]),
      "c=" ~~ x : NumberString :> ("c" -> ToExpression[x]),
      "m=" ~~ x : NumberString :> ("m" -> ToExpression[x]),
      StartOfString ~~ "/" ~~ Shortest[x : __] ~~ " it=" :> ("var" -> x)},
      Overlaps -> True]];

    Profile["datasetAttributes/Replace1",
    dsattrs = {"var", "it", "tl", "rl", "c", "m"} /. attributeRules];

    (* If an attribute isn't found for a dataset, set it to None *)
    Profile["datasetAttributes/Replace2",
    dsattrs = dsattrs /. {"var" -> None, "it" -> None, "tl" -> None,
                          "rl" -> None, "c" -> None, "m" -> None}];

    dsattrs]
  ]
];

datasetsWith[datasets_List, attr_Rule] := 
  Select[datasets, #[[attr[[1]]]] == attr[[2]] &];

datasetsWith[file_String, attr_Rule] := 
  datasetsWith[datasetAttributes[file], attr];

datasetsWith[file_String, attr_List] :=
  Module[{attr2, pattern, w},
    attr2 = attr /. ((x_->y_) :> (w[x] -> y));
    pattern = Table[w[i], {i, 1, 6}] /. attr2 /. w[_] -> _;
    Cases[datasetAttributes[file], pattern]];

datasetAttribute[datasets_List, attr_] :=
  Sort[Cases[DeleteDuplicates[datasets[[All, attr]]], Except[None]]];

datasetAttribute[file_String, attr_] :=
  datasetAttribute[datasetAttributes[file], attr];

DefineMemoFunction[Datasets[file_],
  ReadHDF5[file, "Datasets"]
];

Annotations[file_, ds_] := Profile["Annotations", ReadHDF5[file, {"Annotations", ds}]];
Dims[file_, ds_]        := ReadHDF5[file, {"Dimensions", ds}];
HDF5Data[file_, ds_]    := Profile["HDF5Data", ReadHDF5[file, {"Datasets", ds}]];

End[];

EndPackage[];
