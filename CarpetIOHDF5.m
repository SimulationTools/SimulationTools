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

BeginPackage["SimulationTools`CarpetIOHDF5`",
 {
  "SimulationTools`DataRegion`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`Error`",
  "SimulationTools`Memo`",
  "SimulationTools`Profile`",
  "SimulationTools`ReadHDF5`",
  "SimulationTools`RunFiles`"
 }];

Begin["`Private`"];

(****************************************************************)
(* HaveData *)
(****************************************************************)

SimulationTools`CarpetIOHDF5`GridFunctions`HaveData[___] := True; (* This needs to be tidied up in Providers *)

(****************************************************************)
(* ReadData *)
(****************************************************************)

Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadData] = {
    "Iteration"       -> Null,
    "Map"             -> Null,
    "RefinementLevel" -> Null,
    "TimeLevel"       -> Null,
    "Variable"        -> Null,
    "StripGhostZones" -> True
  };

SimulationTools`CarpetIOHDF5`GridFunctions`ReadData[file_String, opts:OptionsPattern[]] :=
  ToDataRegion[ReadCarpetIOHDF5Components[file,
    OptionValue[Variable],
    OptionValue[Iteration],
    OptionValue[RefinementLevel],
    OptionValue[TimeLevel],
    OptionValue[Map],
    Sequence@@FilterRules[{opts}, Options[ReadCarpetIOHDF5Components]]
  ]];

(****************************************************************)
(* ToFileName *)
(****************************************************************)

Options[SimulationTools`CarpetIOHDF5`GridFunctions`ToFileName] =
  FilterRules[Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadData], Except["StripGhostZones"]];

SimulationTools`CarpetIOHDF5`GridFunctions`ToFileName[var_String, dims:(_List|All), opts:OptionsPattern[]] :=
  Module[
    {map, filename, dimspattern},
    map = Switch[OptionValue[Map],
		 _Integer, "."<>ToString[OptionValue[Map]],
		 None, "",
		 All, "(\\.\\d){0,1}",
		 _, Error["Unrecognised map "<>ToString[OptionValue[Map],InputForm]]];

    dimspattern = Which[
      SameQ[dims,All],
      "(file_0){0,1}",

      And@@Map[StringQ,dims],
      If[SameQ[StringJoin[dims], "xyz"],
         "("<>StringJoin[dims]<>"|(file_0){0,1})",
         StringJoin[dims]],

      True,
      Error["Unrecognised dimensions "<>ToString[dims,InputForm]]];

    filename = var <> map <> "." <> dimspattern <> ".h5";

    If[map =!= "" || dims === All || dimspattern =!= StringJoin[dims],
      Return[RegularExpression[filename]],
      Return[filename]]];

(****************************************************************)
(* ReadIterations *)
(****************************************************************)

Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadIterations] =
  FilterRules[Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadData], Except["Iteration"]];
SimulationTools`CarpetIOHDF5`GridFunctions`ReadIterations[file_String, opts:OptionsPattern[]] :=
  datasetAttribute[datasetsWith[file,
    attributeNamesToNumbers[FilterRules[{opts},Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadIterations]]]
  ],2];

(****************************************************************)
(* ReadMaps *)
(****************************************************************)

Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadMaps] =
  FilterRules[Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadData], Except["Map"]];
SimulationTools`CarpetIOHDF5`GridFunctions`ReadMaps[file_String, opts:OptionsPattern[]] :=
  datasetAttribute[datasetsWith[file,
    attributeNamesToNumbers[FilterRules[{opts},Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadMaps]]]
  ],6];

(****************************************************************)
(* ReadRefinementLevels *)
(****************************************************************)

Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadRefinementLevels] =
  FilterRules[Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadData], Except["RefinementLevel"]];
SimulationTools`CarpetIOHDF5`GridFunctions`ReadRefinementLevels[file_String, opts:OptionsPattern[]] :=
  datasetAttribute[datasetsWith[file,
    attributeNamesToNumbers[FilterRules[{opts},Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadRefinementLevels]]]
  ],4];

(****************************************************************)
(* ReadTimeLevels *)
(****************************************************************)

Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadTimeLevels] =
  FilterRules[Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadData], Except["TimeLevel"]];
SimulationTools`CarpetIOHDF5`GridFunctions`ReadTimeLevels[file_String, opts:OptionsPattern[]] :=
  datasetAttribute[datasetsWith[file,
    attributeNamesToNumbers[FilterRules[{opts},Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadTimeLevels]]]
  ],3];

(****************************************************************)
(* ReadVariables *)
(****************************************************************)

Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadVariables] =
  FilterRules[Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadData], Except["Variable"]];
SimulationTools`CarpetIOHDF5`GridFunctions`ReadVariables[file_String, opts:OptionsPattern[]] :=
  datasetAttribute[datasetsWith[file,
    attributeNamesToNumbers[FilterRules[{opts},Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadVariables]]]
  ],1];

(****************************************************************)
(* ReadTime *)
(****************************************************************)

Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadTime] =
  Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadData];
SimulationTools`CarpetIOHDF5`GridFunctions`ReadTime[file_String, opts:OptionsPattern[]] :=
  Module[
    {varNames, varName, dsName, annots},

    (* TODO: This function should be accelerated assuming that the
       relationship between time and iteration is linear.  We should
       also think about what to do about the RefinementLevel argument
       to this function. *)

    varNames = datasetAttribute[
      datasetsWith[file,
                   attributeNamesToNumbers[
                     FilterRules[{opts},Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadVariables]]]], 1];

    If[Length[varNames] === 0, Error["Cannot find dataset with "<>ToString[{opts}]<>" in HDF5 file "<>file]];
    varName = First[varNames];
    dsName = CarpetIOHDF5DatasetName[varName, OptionValue[Iteration],
                                     OptionValue[Map], OptionValue[RefinementLevel],
                                     OptionValue[TimeLevel], 0];
    annots = Annotations[file, dsName];
    "time" /. annots];

(****************************************************************)
(* FindGridFunctions *)
(****************************************************************)

SimulationTools`CarpetIOHDF5`GridFunctions`FindGridFunctions[sim_String] :=
 Module[{pattern, h5Files, vars, leafnames, basenames, varnames, 
   var},
  pattern = 
   var__ ~~ 
    "." ~~ ("x" | "y" | "z" | "xy" | "xz" | 
      "yz" | ("file_" ~~ (DigitCharacter ..))) ~~ ".h5";
  h5Files = FindSimulationFiles["bbh", pattern];
  leafnames = FileNameTake[#, -1] & /@ h5Files;
  varnames = Union[First[StringSplit[#, "."]] & /@ leafnames]];

(****************************************************************)
(* ReadGridFunctionDimensions *)
(****************************************************************)

SimulationTools`CarpetIOHDF5`GridFunctions`ReadGridFunctionDimensions[sim_String, varName_String] :=
  Module[{pattern, h5Files, leafnames, dimensions, dimStrings, 
   dimRules, x},
  pattern = 
   varName ~~ 
    "." ~~ ("x" | "y" | "z" | "xy" | "xz" | 
      "yz" | ("file_" ~~ (DigitCharacter ..))) ~~ ".h5";
  h5Files = FindSimulationFiles["bbh", pattern];
  leafnames = FileNameTake[#, -1] & /@ h5Files;
  dimStrings = StringSplit[#, "."][[2]] & /@ leafnames;
  dimRules =
   {(x_String /; StringMatchQ[x, "file_" ~~ (DigitCharacter ..)]) -> 
     "xyz"};
  Union[dimStrings /. dimRules]];

(***************************************************************************************)
(* Defaults                                                                            *)
(***************************************************************************************)

SimulationTools`CarpetIOHDF5`GridFunctions`DefaultIteration[its_List] := 0;

SimulationTools`CarpetIOHDF5`GridFunctions`DefaultMap[maps_List] := None;

SimulationTools`CarpetIOHDF5`GridFunctions`DefaultRefinementLevel[run_, var_] := 0;

SimulationTools`CarpetIOHDF5`GridFunctions`DefaultTimeLevel[run_, var_] := 0;

(***************************************************************************************)
(* Private functions                                                                   *)
(***************************************************************************************)

(****************************************************************)
(* CarpetIOHDF5DatasetName *)
(****************************************************************)

(* Generate a dataset name string *)
CarpetIOHDF5DatasetName[var_String, it_Integer, m:(_Integer|None), rl:(_Integer|None), tl:(_Integer|None), c:(_Integer|None)] :=
 Module[{map="", component="", reflevel="", timelevel=""},
  If[m =!= None, map=" m="<>ToString[m]];
  If[c =!= None, component=" c="<>ToString[c]];
  If[rl =!= None, reflevel=" rl="<>ToString[rl]];
  If[tl =!= None, timelevel=" tl="<>ToString[tl]];
  "/" <> var <> " it=" <> ToString[it] <> timelevel <> map <> reflevel <> component];

(****************************************************************)
(* ReadCarpetIOHDF5Datasets *)
(****************************************************************)

Options[ReadCarpetIOHDF5Datasets] = {"StripGhostZones" -> True};

(* Read a list of datasets into DataRegions *)
ReadCarpetIOHDF5Datasets[file_String, {}, opts:OptionsPattern[]] := {};
ReadCarpetIOHDF5Datasets[file_String, ds_List, opts:OptionsPattern[]] :=
 Profile["ReadCarpetIOHDF5Datasets",
 Module[{data, annots, dims, order, origin, spacing, name, dr, ghosts, time},
  If[Apply[Or,Map[(!StringQ[#])&, ds]],
    Error["ReadCarpetIOHDF5Datasets: expected a string dataset name, but instead got " <>ToString[ds]]];

  If[FileType[file] === None,
    Error["File " <> file <> " not found"]];

  data = HDF5Data[file, ds];

  annots = Annotations[file, ds];

  dims = Reverse /@ Dims[file, ds];

  (* Data from CarpetIOHDF5 is in Fortran column-major format. Transpose to get Mathematica row-major format *)
  order  = Reverse /@ Range /@ ArrayDepth /@ data;
  data = MapThread[Transpose, {data, order}];

  origin = "origin" /. annots /. "origin" -> Null;
  spacing = "delta" /. annots /. "delta" -> Null;
  name = "name" /. annots /. "name" -> Null;

  ghosts = ("cctk_nghostzones" /. annots) /. "cctk_nghostzones" -> 0;

  time = "time" /. annots /. "time" -> Null;
  dr = MapThread[ToDataRegion[#1,#2,#3,"VariableName"->#4,"Attributes"->{"Time"->#5}] &,
                 {data, origin, spacing, name, time}];

  If[OptionValue[StripGhostZones]==True,
    dr = MapThread[Take[#1, Sequence@@Transpose[{#2, -#2}]] &, {dr, ghosts+1}]];

  dr
]];

(****************************************************************)
(* CarpetIOHDF5Components *)
(****************************************************************)

(* Get a list of all components in a file *)
CarpetIOHDF5Components[file_, it_, rl_] := 
  Profile["CarpetIOHDF5Components",
  datasetAttribute[datasetsWith[file, {2 -> it, 4 -> rl}], 5]];

(****************************************************************)
(* ReadCarpetIOHDF5Components *)
(****************************************************************)

(* Read all datasets for a variable *)
Options[ReadCarpetIOHDF5Components] = {"StripGhostZones" -> True};

ReadCarpetIOHDF5Components[file_String, var_String, it_Integer, rl_Integer, tl_Integer, map_, opts:OptionsPattern[]] :=
  Module[{fileNames, datasets, pattern, components, names, varNames, varName, directory, leaf, leafPrefix},
    If[FileType[file] === None,
      Error["File " <> file <> " not found in ReadCarpetIOHDF5Components"]];

    (* Support the one file per process scheme, with filenames var.file_n.h5 *)
    If[StringMatchQ[file, RegularExpression[".*\\.file_0\\.h5"]],
      directory = DirectoryName[file];
      leaf = FileNameTake[file];
      leafPrefix = StringReplace[leaf, ".file_0.h5" -> ""];
      pattern = leafPrefix ~~ ".file_" ~~ DigitCharacter.. ~~ ".h5";
      fileNames = Profile["FileNames", FileNames[pattern, {directory}]];
    ,
      fileNames = {file};
    ];

    (* Get a list of components in each file *)
    components = Map[CarpetIOHDF5Components[#, it, rl]&, fileNames];
    components = components /. {} -> {None};

    (* Figure out what the variable is called inside the file *)
    varNames = SimulationTools`CarpetIOHDF5`GridFunctions`ReadVariables[file,
      "Iteration" -> it, "Map"-> map, "RefinementLevel" -> rl, "TimeLevel" -> tl];

    varName = First[Select[varNames, StringMatchQ[#, ___ ~~ var ~~ ___] &]];

    (* Construct a list of dataset names *)
    names = Map[CarpetIOHDF5DatasetName[varName, it, map, rl, tl, #] &, components, {2}];

    (* Read the data *)
    datasets = Flatten[
      MapThread[
        ReadCarpetIOHDF5Datasets[#1, #2, 
                                 FilterRules[{opts},Options[ReadCarpetIOHDF5Datasets]]] &,
                                 {fileNames, names}]];

    datasets
];


(***************************************************************************************)
(* Low-level interface for determining what's available in a file *)
(***************************************************************************************)

(****************************************************************)
(* datasetAttributes *)
(****************************************************************)

datasetAttributes[file_] :=
  Module[{timestamp, attributeRules, datasets, dsattrs},
    timestamp = FileDate[file];
    If[cache[file]["timestamp"] === timestamp,
      Return[cache[file]["data"]];
    ];

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

    (* Cache the result *)
    cache[file]["timestamp"] = timestamp;
    cache[file]["data"] = dsattrs;

    dsattrs]
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

attributeNamesToNumbers[expr_] := expr /. {"Iteration" -> 2, "TimeLevel" -> 3,
  "RefinementLevel" -> 4, "Map" -> 6, "Variable" -> 7};

Datasets[file_]         := ReadHDF5[file, "Datasets"];
Annotations[file_, ds_] := Profile["Annotations", ReadHDF5[file, {"Annotations", ds}]];
Dims[file_, ds_]        := ReadHDF5[file, {"Dimensions", ds}];
HDF5Data[file_, ds_]    := Profile["HDF5Data", ReadHDF5[file, {"Datasets", ds}]];

End[];

EndPackage[];
