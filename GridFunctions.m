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

BeginPackage["GridFunctions`",
 {
  "DataRepresentations`",
  "DataTable`",
  "Error`",
  "Memo`",
  "Providers`",
  "RunFiles`"
 }];

ReadGridFunction::usage = "ReadGridFunction[run, var, dims] reads variable var from run and returns it as a DataRegion object.  dims is a list of the dimensions to read, and these dimensions must correspond to available output.  Dimensions in dims can be given either as coordinate names (\"x\", \"y\", \"z\") or as numbers (1, 2, 3).  The order of dims is not important.  Optional arguments: Map -> Automatic | mapnum specifies the map for multipatch data files, StripGhostZones -> True|False determines whether the ghost zones are removed from the variable before it is returned.";
ReadIterations::usage = "ReadIterations[run, var, dims] reads the iterations present for the grid function var in the directory run.";
ReadMaps::usage = "ReadMaps[run, var, dims] reads the multipatch maps present  for the grid function var in the directory run.";
ReadRefinementLevels::usage = "ReadRefinementLevels[run, var, dims] reads the refinement levels present for the grid function var in the directory run.";
ReadTimeLevels::usage = "ReadTimeLevels[run, var, dims] reads the timelevels present for the grid function var in the directory run.";

ReadTime::usage = "ReadTime[run, var, it, rl] reads the time associated with the iteration it on refinement level rl of the CarpetHDF5 file named var in the directory run.";

StripGhostZones::usage = "StripGhostZones is a boolean option to various CarpetHDF5 functions which indicates that ghost zones should be removed";


Begin["`Private`"];

DimsPattern = _List|_String;

parseDims[s_String] :=
  parseDims[Characters[s]];

parseDims[dims:{(1|2|3)...}] := parseDims[dims /. {1->"x",2->"y",3->"z"}];

parseDims[dims:{_String...}] := dims;

(* ApplyDefaults[opts, provider] := *)
(*   Do[Which[ *)
(*       OptionValue[opt] == Automatic && HaveOpt[opt] && HaveDefault[opt], *)
(*       OptionValue[opt] = Default[opt], *)

(*       OptionValue[opt] != Automatic && HaveOpt[opt], *)
(*       OptionValue[opt] = OptionValue[opt], *)

(*       OptionValue[opt] == Automatic && HaveOpt[opt] && !HaveDefault[opt], *)
(*       Error["Automatic selected, but no default available"], *)
      
(*       OptionValue[opt] == Automatic && !HaveOpt[opt] && HaveDefault[opt], *)
(*       Error["Default specified for non-existent option"], *)

(*       OptionValue[opt] != Automatic && !HaveOpt[opt], *)
(*       Error["Unknown option specified"], *)

(*       OptionValue[opt] == Automatic && !HaveOpt[opt] && !HaveDefault[opt], *)
(*       Continue[] *)
(*     ], {opt, options}]; *)

ApplyDefaults[run_String, var_String, opts_List] :=
  Module[
    {providerOptions, options},

    providerOptions = {
      "Iteration"       -> CallProvidedFunction["GridFunctions","DefaultIteration", {{}}],
      "Map"             -> CallProvidedFunction["GridFunctions","DefaultMap", {{}}],
      "RefinementLevel" -> CallProvidedFunction["GridFunctions","DefaultRefinementLevel", {run,var}],
      "TimeLevel"       -> CallProvidedFunction["GridFunctions","DefaultTimeLevel", {run,var}]};

    options = Map[(#[[1]] -> If[OptionValue[ReadGridFunction, opts, #[[1]]] === Automatic,
                                #[[1]] /. providerOptions,
                                OptionValue[ReadGridFunction, opts, #[[1]]]]) &,
                  Options[ReadGridFunction]];

    options];


Options[ReadGridFunction] = {
    "Iteration"       -> Automatic,
    "Map"             -> Automatic,
    "RefinementLevel" -> Automatic,
    "TimeLevel"       -> Automatic,
    "StripGhostZones" -> True,
    "Variable"        -> Automatic (* Only used by old interface *)
  };

ReadGridFunction[run_String, var_String, dims1:DimsPattern, opts:OptionsPattern[]] :=
  Module[
    {options, fileName, dims = parseDims[dims1]},
    options = ApplyDefaults[run, var, {opts}];
    fileName = getFileOfIt[run, getLeafName[var, dims, options], OptionValue[ReadGridFunction, options, Iteration], options];
    If[fileName === None,
       notFound[run,var,dims,options]];
    CallProvidedFunction["GridFunctions", "ReadData", {fileName, "Variable" -> var, options}]
];

Options[notFound] = Options[ReadGridFunction];
notFound[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  Error[StringJoin[ToString/@
                   Flatten@{"Error: ",
                            Riffle[
                              Map[{#, " ", OptionValue[#]} &, 
                                  Select[{"Iteration", "RefinementLevel", "Map"}, OptionValue[#] =!= Automatic &]],
                              ", "],
                            " of variable ", var,
                            " with dimensions ", dims,
                            " not found in run ", run}]];

Options[fileUnion] = Options[ReadGridFunction];
fileUnion[fn_String, run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  Module[
    {files = FindRunFiles[run, getLeafName[var, dims, opts]]},
    If[files === {},
       notFound[run,var,dims,opts]];
    Union@@Map[CallProvidedFunction["GridFunctions", fn, {#, "Variable" -> var, opts}] &,
               files]];

Options[ReadIterations] = FilterRules[Options[ReadGridFunction], Except["Iteration"]];
ReadIterations[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  fileUnion["ReadIterations", run, var, parseDims[dims], DeleteCases[ApplyDefaults[run, var, {opts}],
                                                          "Iteration" -> _]];

Options[ReadMaps] = FilterRules[Options[ReadGridFunction], Except["Map"]];
ReadMaps[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  fileUnion["ReadMaps", run, var, parseDims[dims], 
            Append[DeleteCases[ApplyDefaults[run, var, {opts}],"Map" -> _, Infinity],
                   "Map" -> All]];

Options[ReadRefinementLevels] = FilterRules[Options[ReadGridFunction], Except["RefinementLevel"]];
ReadRefinementLevels[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  fileUnion["ReadRefinementLevels", run, var, parseDims[dims], DeleteCases[ApplyDefaults[run, var, {opts}],
                                                                "RefinementLevel" -> _]];

Options[ReadTimeLevels] = FilterRules[Options[ReadGridFunction], Except["TimeLevel"]];
ReadTimeLevels[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  fileUnion["ReadTimeLevels", run, var, parseDims[dims], DeleteCases[ApplyDefaults[run, var, {opts}],
                                                                "TimeLevel" -> _]];

Options[ReadTime] = Options[ReadGridFunction];
ReadTime[run_String, var_String, dims1:DimsPattern, opts:OptionsPattern[]] :=
  Module[
    {options, fileName, dims = parseDims[dims1]},
    options = ApplyDefaults[run, var, {opts}];
    fileName = getFileOfIt[run, getLeafName[var, dims, options], OptionValue[ReadGridFunction, options, Iteration]];
    If[fileName === None,
       notFound[run,var,dims,options]];
    CallProvidedFunction["GridFunctions", "ReadTime", {fileName, "Variable" -> var, options}]
];

Options[getFileOfIt] = Options[ReadGridFunction];
getFileOfIt[run_String, leafName:(_String|_RegularExpression), it_Integer, opts:OptionsPattern[]] :=
  Module[{files, itss, haveIts},
    files = FindRunFiles[run, leafName];
    itss = Select[Map[{#, getFileIts[#,opts]} &, files], Length[#[[2]]] > 0 &];
    haveIts = Select[itss, it >= First[#[[2]]] && it <= Last[#[[2]]] &];
    If[Length[haveIts] === 0,
       None,
       haveIts[[1,1]]]];

getFileIts[fileName_, opts:OptionsPattern[]] :=
  CallProvidedFunction["GridFunctions","ReadIterations",{fileName,opts}];

getLeafName[var_String, dims:DimsPattern, options_List] :=
  CallProvidedFunction["GridFunctions", "ToFileName", {var, dims, FilterRules[options, Except["StripGhostZones"]]}];

(***************************************************************************************)
(* Backward compatibility *)
(***************************************************************************************)

(* toVarDimsMap[varFile_String] := *)
(*   Module[{comps, var, mapDimsList, dims, map, isDims, isMap}, *)
(*   (\* phi.file_0.h5 *)
(*      phi.x.h5 *)
(*      phi.xy.h5 *)
(*      phi.h5 *)
(*      phi.0.h5 *)
(*      phi.0.xy.h5 *\) *)
(*   comps = StringSplit[varFile,"."]; *)
(*   If[Length[comps] === 1,  *)
(*      Error["File name "<>varFile<>" does not have an extension"]]; *)

(*   If[Length[comps] > 4,  *)
(*      Error["File name "<>varFile<>" has too many \".\" characters"]]; *)

(*   If[Last[comps] =!= "h5", *)
(*      Error["File name "<>varFile<>" does not have the extension \"h5\""]]; *)

(*   var = First[comps]; *)
(*   mapDimsList = Drop[Drop[comps,-1],1]; *)

(*   (\* xy *)
(*      0 *)
(*      0.xy *\) *)

(*   isDims[s_String] :=  *)
(*     Complement[Characters[s], {"x","y","z","d"}] === {}; *)

(*   isMap[s_String] := *)
(*     StringMatchQ[s, DigitCharacter]; *)

(*   dims = All; *)
(*   map = None; *)

(*   (\* We don't enforce the ordering of the components as there is no ambiguity *\) *)
(*   Scan[Which[isDims[#], dims = toDims[#], *)
(*              isMap[#], map = ToExpression[#], *)
(*              True, Error["Unrecognised dimensions or map in "<>varFile]] &, mapDimsList]; *)

(*   {var,dims,map}]; *)

(* ReadGridFunction[run_String, varFile_String, it_Integer, rl:(_Integer|Automatic):Automatic,  *)
(*                  opts:OptionsPattern[]] := *)
(*   Module[ *)
(*     {var,dims,map}, *)
(*     {var,dims,map} = toVarDimsMap[varFile]; *)
(*     ReadGridFunction[run, var, dims, Iteration -> it, RefinementLevel -> rl, Map -> map, *)
(*                     FilterOptions[{opts}, {StripGhostZones}]]]; *)

End[];

EndPackage[];
