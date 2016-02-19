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
  "SimulationTools`Error`",
  "SimulationTools`ProfileCall`",
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
    "StripGhostZones" -> True,
    "FileName"        -> Null
  };

SimulationTools`CarpetIOHDF5`GridFunctions`ReadData[file_String, opts:OptionsPattern[]] :=
  MergedDataRegion[ReadCarpetIOHDF5Components[file,
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
    {filename, map, pattern},
    If[OptionValue[FileName] =!= Null, Return[OptionValue[FileName]]];
    map = Switch[OptionValue[Map],
		 _Integer, "."<>ToString[OptionValue[Map]],
		 None, "",
		 All, "(.\\d)?",
		 _, Error["Unrecognised map "<>ToString[OptionValue[Map],InputForm]]];

    pattern = Which[
      SameQ[dims,All],
      "(.file_0)?",

      SameQ[StringJoin[dims], "xyz"],
      "(" <> map <> ".xyz|.file_0)?",

      And@@Map[StringQ,dims],
      map <> "."<>StringJoin[dims],

      True,
      Error["Unrecognised dimensions "<>ToString[dims,InputForm]]];
    filename = var <> pattern <> ".h5";
    If[OptionValue[Map] === All || SameQ[dims,All] || SameQ[StringJoin[dims], "xyz"],
      Return[RegularExpression[StringReplace[filename, "." -> "\\."]]],
      Return[filename]]];

(****************************************************************)
(* ReadIterations *)
(****************************************************************)

Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadIterations] =
  FilterRules[Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadData], Except["Iteration"]];
SimulationTools`CarpetIOHDF5`GridFunctions`ReadIterations[file_String, opts:OptionsPattern[]] :=
  Union@Flatten[Table[datasetAttribute[datasetsWith[f,
    attributeNamesToNumbers[FilterRules[{opts},Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadIterations]]]
  ],2], {f, FileNames[StringReplace[file,".file_0.h5"->".file_*.h5"]]}],1];

(****************************************************************)
(* ReadMaps *)
(****************************************************************)

Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadMaps] =
  FilterRules[Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadData], Except["Map"]];
SimulationTools`CarpetIOHDF5`GridFunctions`ReadMaps[file_String, opts:OptionsPattern[]] :=
  Module[
    {attrs, maps},
    attrs = DeleteCases[
      FilterRules[{opts},
                  Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadMaps]], 
      ("Iteration"|"Map"|"TimeLevel"|"RefinementLevel") -> _];
    maps = datasetAttribute[datasetsWith[file, attributeNamesToNumbers[attrs]],6];
    
    If[StringMatchQ[file,"*.file_0.h5"],
       Module[
         {allFiles = DeleteCases[
           FileNames[StringReplace[file,"file_0.h5"->"file_*.h5"]], file]},
         
         maps = Union[maps, Flatten[Map[SimulationTools`CarpetIOHDF5`GridFunctions`ReadMaps[#, opts] &, allFiles]]]]];

    maps];

(****************************************************************)
(* ReadRefinementLevels *)
(****************************************************************)

Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadRefinementLevels] =
  FilterRules[Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadData], Except["RefinementLevel"]];
SimulationTools`CarpetIOHDF5`GridFunctions`ReadRefinementLevels[file_String, opts:OptionsPattern[]] :=
  Module[
    {attrs, attrNames, datasets, rls},

    (* Don't restrict to iteration 0, map None, and timelevel 0 when
       searching for refinement levels *)
    attrs = DeleteCases[
      FilterRules[{opts},
                  Options[SimulationTools`CarpetIOHDF5`GridFunctions`ReadRefinementLevels]], 
      ("Iteration"|"Map"|"TimeLevel") -> _];

    attrNames = attributeNamesToNumbers[attrs];
    datasets = datasetsWith[file, attrNames];
    rls = datasetAttribute[datasets,4];
    rls];

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

filePattern=
  Repeated[("." ~~ (DigitCharacter ..)), {0, 1}] ~~
  Repeated["." ~~ ("x" | "y" | "z" | "xy" | "xz" | "yz" |
           ("file_" ~~ (DigitCharacter ..))), {0, 1}] ~~
  ".h5";

(* Warning: this could be slow for large simulations *)
carpetIOHDF5FileQ[filename_String] :=
  MemberQ[ReadHDF5[filename, {"Datasets"}],
          ("/Parameters and Global Attributes" |
           "/Parameters and Global Attributes/All Parameters")];

SimulationTools`CarpetIOHDF5`GridFunctions`FindGridFunctions[sim_String] :=
 Module[{pattern, h5Files, leafnames, varnames, 
   var},
  pattern = var__ ~~ filePattern;
  h5Files = Select[FindSimulationFiles[sim, pattern], carpetIOHDF5FileQ];
  leafnames = FileNameTake[#, -1] & /@ h5Files;
  varnames = Union[First[StringSplit[#, "."]] & /@ leafnames]];

(****************************************************************)
(* ReadGridFunctionDimensions *)
(****************************************************************)

parseDimensionsFromFileName[fileName_String] :=
 Module[{split, dims},
 split = StringSplit[fileName, "."];
 If[Length[split] === 2 || StringMatchQ[split[[-1]], "file_" ~~ (DigitCharacter ..)],
   dims = "xyz",
   dims = split[[-2]]
 ];
 dims
]

SimulationTools`CarpetIOHDF5`GridFunctions`ReadGridFunctionDimensions[sim_String, varName_String] :=
  Module[{pattern, h5Files, leafnames, dimStrings, 
   dimRules, x},
  pattern = varName ~~ filePattern;
  h5Files = FindSimulationFiles[sim, pattern];
  leafnames = FileNameTake[#, -1] & /@ h5Files;
  dimStrings = parseDimensionsFromFileName /@ leafnames;
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
 ProfileCall["ReadCarpetIOHDF5Datasets",
 Module[{data, annots, dims, order, origin, spacing, name, dr, ghosts, time},
  If[Apply[Or,Map[(!StringQ[#])&, ds]],
    Error["ReadCarpetIOHDF5Datasets: expected a string dataset name, but instead got " <>ToString[ds]]];

  If[FileType[file] === None,
    Error["File " <> file <> " not found"]];

  data = HDF5Data[file, ds];

  annots = Annotations[file, ds];

  dims = Reverse /@ Dims[file, ds];
  
  nDims = Length[Dimensions[data]] - 1;
  (* Data from CarpetIOHDF5 is in Fortran column-major format. Transpose to get Mathematica row-major format *)
  order  = Reverse /@ Range /@ ArrayDepth /@ data;
  data = MapThread[Transpose, {data, order}];

  origin = "origin" /. annots /. "origin" -> ("iorigin" /. annots);
  spacing = "delta" /. annots /. "delta" -> ConstantArray[1.,nDims];
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
CarpetIOHDF5Components[file_, it_, rl_, map_] := 
  ProfileCall["CarpetIOHDF5Components",
  datasetAttribute[datasetsWith[file, {2 -> it, 4 -> rl, 6 -> map}], 5]];

(****************************************************************)
(* ReadCarpetIOHDF5Components *)
(****************************************************************)

(* Read all datasets for a variable *)
Options[ReadCarpetIOHDF5Components] = {"StripGhostZones" -> True};

ReadCarpetIOHDF5Components[file_String, var_String, it_Integer, rl_, tl_Integer, map_, opts:OptionsPattern[]] :=
  Module[{fileNames, datasets, pattern, components, names, varNames, varName, directory, leaf,
          leafPrefix, haveComp, possibleVarNames},
    If[FileType[file] === None,
      Error["File " <> file <> " not found in ReadCarpetIOHDF5Components"]];

    (* Support the one file per process scheme, with filenames var.file_n.h5 *)
    If[StringMatchQ[file, RegularExpression[".*\\.file_0\\.h5"]],
      directory = DirectoryName[file];
      leaf = FileNameTake[file];
      leafPrefix = StringReplace[leaf, ".file_0.h5" -> ""];
      pattern = leafPrefix ~~ ".file_" ~~ DigitCharacter.. ~~ ".h5";
      fileNames = ProfileCall["FileNames", FileNames[pattern, {directory}]];
    ,
      fileNames = {file};
    ];

    (* Get a list of components in each file *)
    components = Map[CarpetIOHDF5Components[#, it, rl, map]&, fileNames];

    (* Figure out what the variable is called inside the file *)
    varNames = Flatten[Table[SimulationTools`CarpetIOHDF5`GridFunctions`ReadVariables[f,
      "Iteration" -> it, "Map"-> map, "RefinementLevel" -> rl, "TimeLevel" -> tl], {f, fileNames}]];

    (* Choose a variable from the file whose name is a superstring of the passed variable name *)
    (* TODO: why is it done like this? *)
    possibleVarNames = Select[varNames, StringMatchQ[#, ___ ~~ var ~~ ___] &];
    If[possibleVarNames === {},
      Error["Cannot find a variable named "<>var<>" in files "<>ToString[fileNames]<>
        "; variables found are "<>ToString[varNames]]];
    varName = First[possibleVarNames];
    haveComp = Map[# =!= {} &, components];

    (* Construct a list of dataset names *)
    names = Map[CarpetIOHDF5DatasetName[varName, it, map, rl, tl, #] &, Pick[components,haveComp], {2}];

    (* Read the data *)
    datasets = Flatten[
      MapThread[
        ReadCarpetIOHDF5Datasets[#1, #2, 
                                 FilterRules[{opts},Options[ReadCarpetIOHDF5Datasets]]] &,
                                 {Pick[fileNames,haveComp], names}]];

    datasets
];


(***************************************************************************************)
(* Low-level interface for determining what's available in a file *)
(***************************************************************************************)

(* NB: These functions have been carefully optimised; where something
   is done in a specific way, which may appear odd, this is likely for
   performance reasons. *)

(* Terminology:

We use "attribute" here in a sense distrinct from that used in HDF5.
Here, "attributes" are the values encoded in CarpetIOHDF5 dataset
names, e.g. it=256 c=4 m=0 rl=9.

Dataset Attributes Table (dsAttrTable): 

   List of lists of attribute values for the HDF5 file.  There is one
   row per dataset, and the columns are: varname, iteration,
   timelevel, refinementlevel, component, map

Attribute index (attrIndex):

   The column number of the attribute in the dataset attributes table.
   e.g. the attribute index for iteration is 2.

Attribute value (attrVal):

   The value of a specific attribute; eg. the value of the iteration
   attribute for a given dataset might be 128.

Attribute selector (attrSel):

   An object used for searching for datasets where a given attribute
   has a given value.  Of the form attrIndex -> attrVal.  e.g. 2 ->
   128 would be an attribute selector which looked for datasets with
   iteration = 128.

Attribute name (attrName):

   A name given to each of the attributes.

*)

(****************************************************************)
(* datasetAttributesTable *)
(****************************************************************)

(* 
   This information is determined from the dataset name only (reading
   the attributes is much more expensive).  If a value is not present
   in the dataset name (e.g. the map for a single-map simulation), it
   is represented as None.  The file attributes table is cached, and
   only reread if the datestamp of the HDF5 file is changed. *)

datasetAttributesTable[h5filename_String] :=
  Module[{timestamp, attributeRules, datasets, dsattrs},
    timestamp = FileDate[h5filename];
    If[cache[h5filename]["timestamp"] === timestamp,
      Return[cache[h5filename]["data"]];
    ];

    ProfileCall["datasetAttributesTable[file]",

    (* Convert dataset name strings into rules *)
    datasets = ProfileCall["Datasets", Datasets[h5filename]];

    (* Remove datasets which are not data; performance of this has not
       been tested, but not doing this leads to None entries in the
       table, which are incorrectly interpreted *)
    (* datasets = Flatten[StringCases[datasets, ___~~"="~~___]]; *)
    datasets = Select[datasets, StringMatchQ[#, "*=*"] &];

    ProfileCall["datasetAttributesTable/StringCases",
     attributeRules = StringCases[datasets,
     {"it=" ~~ x : NumberString :> ("it" -> ToExpression[x]),
      "tl=" ~~ x : NumberString :> ("tl" -> ToExpression[x]),
      "rl=" ~~ x : NumberString :> ("rl" -> ToExpression[x]),
      "c=" ~~ x : NumberString :> ("c" -> ToExpression[x]),
      "m=" ~~ x : NumberString :> ("m" -> ToExpression[x]),
      StartOfString ~~ "/" ~~ Shortest[x : __] ~~ " it=" :> ("var" -> x)},
      Overlaps -> True]];

    ProfileCall["datasetAttributesTable/Replace1",
    dsattrs = {"var", "it", "tl", "rl", "c", "m"} /. attributeRules];

    (* If an attribute isn't found for a dataset, set it to None *)
    ProfileCall["datasetAttributesTable/Replace2",
    dsattrs = dsattrs /. {"var" -> None, "it" -> None, "tl" -> None,
                          "rl" -> None, "c" -> None, "m" -> None}];

    (* Cache the result *)
    cache[h5filename]["timestamp"] = timestamp;
    cache[h5filename]["data"] = dsattrs;

    dsattrs]
];

(* Given a list of dataset attribute lists (i.e. rows of the 2D
   dataset attribute table), and a rule of the form attrindex -> val,
   return only those dataset attribute lists where the given attribute
   has the given value.  The attrindex is the column number of the
   attribute in the attributes table. *)

datasetsWith[dsAttrTable_List, attrsel:(attrIndex_Integer -> attrVal_)] := 
 (Error["This function was thought to be unused"];
  Select[dsAttrTable, #[[attrIndex]] == attrVal &]);

(* Given the name of an HDF5 file, and a rule of the form attrindex ->
   val, return only those dataset attribute lists where the given
   attribute has the given value.  The attrindex is the column number
   of the attribute in the attributes table of the file. *)

datasetsWith[h5filename_String, attrsel:(attrIndex_Integer -> attrVal_)] := 
 (Error["This function was thought to be unused"];
  datasetsWith[datasetAttributesTable[h5filename], attrsel]);

(* Given the name of an HDF5 file, and a list of rules of the form
   attrindex -> val, return only those dataset attribute lists where
   the given attributes have all the given values.  The attrindexes
   are the column numbers of the attribute in the attributes table of
   the file.  The odd use of patterns and Cases is for performance. *)

datasetsWith[h5filename_String, attrsels_List] :=
  Module[{attrsels2, pattern, w},
    attrsels2 = attrsels /. ((x_->y_) :> (w[x] -> y));
    pattern = Table[w[i], {i, 1, 6}] /. attrsels2 /. w[_] -> _;
    Cases[datasetAttributesTable[h5filename], pattern]];

(* Given a dataset attributes table, and an attribute index, return a
   list of the values that that attribute takes across all the
   datasets. *)

datasetAttribute[dsAttrTable_List, attrIndex_Integer] :=
  Sort[DeleteDuplicates[dsAttrTable[[All, attrIndex]]]];

(* Given the name of an HDF5 file and an attribute index, return a
   list of the values that that attribute takes across all the
   datasets in the file. *)

datasetAttribute[h5filename_String, attrIndex_Integer] :=
 (Error["This function was thought to be unused"];
  datasetAttribute[datasetAttributesTable[h5filename], attrIndex]);

(* Convert the dataset attribute names to attribute indices as used in
   the 2D dataset attributes table *)

attributeNamesToNumbers[expr_] := expr /. {"Iteration" -> 2, "TimeLevel" -> 3,
  "RefinementLevel" -> 4, "Map" -> 6, "Variable" -> 7};

(* Return a list of the names of the datasets in an HDF5 file *)
Datasets[h5filename_] :=
  ReadHDF5[h5filename, "Datasets"];

(* Return a list of the attributes of the datasets in an HDF5 file.
   The lists of attributes are in the same order as the dataset names
   in Datasets[file]. *)
Annotations[h5filename_String, datasetName:(_String|_List)] :=
  ProfileCall["Annotations",
    ReadHDF5[h5filename, {"Annotations", datasetName}]];

(* Return the dimensions of the dataset with the given name; a list such as {nx, ny, nz} *)
Dims[h5filename_String, datasetName:(_String|_List)] :=
  ReadHDF5[h5filename, {"Dimensions", datasetName}];

(* Return the data of the dataset with the given name or list of names *)
HDF5Data[h5filename_String, datasetName:(_String|_List)] :=
  ProfileCall["HDF5Data", ReadHDF5[h5filename, {"Datasets", datasetName}]];

End[];

EndPackage[];
