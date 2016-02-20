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

BeginPackage["SimulationTools`GridFunctions`",
 {
  "SimulationTools`Error`",
  "SimulationTools`Providers`",
  "SimulationTools`RunFiles`",
  "SimulationTools`"
 }];

ReadGridFunction::usage = "ReadGridFunction[sim, var, dims] reads variable var from sim and returns it as a DataRegion object. dims is a list of the dimensions to read, and these dimensions must correspond to available output. Dimensions in dims can be given either as coordinate names (x, y, z) or as numbers (1, 2, 3).  The order of dims is not important.  Optional arguments: Map -> Automatic | mapnum specifies the map for multipatch data files, StripGhostZones -> True|False determines whether the ghost zones are removed from the variable before it is returned.";
ReadIterations::usage = "ReadIterations[sim, var, dims] reads the iterations present for the grid function var in sim.";
ReadMaps::usage = "ReadMaps[sim, var, dims] reads the multipatch maps present  for the grid function var in sim.";
ReadRefinementLevels::usage = "ReadRefinementLevels[sim, var, dims] reads the refinement levels present for the grid function var in sim.";
ReadTimeLevels::usage = "ReadTimeLevels[sim, var, dims] reads the timelevels present for the grid function var in sim.";
FindGridFunctions::usage = "FindGridFunctions[sim] gives the names of gridfunctions with data available in a simulation.";
ReadGridFunctionDimensions::usage = "ReadGridFunctionDimensions[sim, var] gives the available dimensions of a gridfunction in a simulation.";

(****************************************************************)
(* Experimental                                                 *)
(****************************************************************)


(****************************************************************)
(* Deprecated                                                   *)
(****************************************************************)

ReadTime;

Begin["`Private`"];

DimsPattern = _List|_String|All;

parseDims[s_String] :=
  parseDims[Characters[s]];

parseDims[dims:{(1|2|3)...}] := parseDims[dims /. {1->"x",2->"y",3->"z"}];

parseDims[dims:{_String...}] := dims;

parseDims[dims:All] := All;

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
      "TimeLevel"       -> CallProvidedFunction["GridFunctions","DefaultTimeLevel", {run,var}],
      "FileName"        -> Null};

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
    "StripGhostZones" :> If[$SimulationToolsCompatibilityVersion < 1, True, False],
    "Variable"        -> Automatic (* Only used by old interface *),
    "FileName"        -> Automatic
  };

DocumentationBuilder`OptionDescriptions["ReadGridFunction"] = {
    "Iteration"       -> "The iteration to read from.",
    "Map"             -> "The map to read from.",
    "RefinementLevel" -> "The refinement level to read from.",
    "TimeLevel"       -> "The time level to read from.",
    "StripGhostZones" -> "Whether to remove ghost zones from the data.",
    "Variable"        -> "The variable to read. This option is no longer used."
  };

DocumentationBuilder`SymbolDescription["ReadGridFunction"] =
  "read data for a grid function at a specific iteration";


ReadGridFunction[run_String, var_String, dims1:DimsPattern, opts:OptionsPattern[]] :=
  Module[
    {options, fileName, leafName, dims = parseDims[dims1]},
    options = ApplyDefaults[run, var, {opts}];
    leafName = getLeafName[var, dims, options];
    fileName = getFileOfIt[run, leafName, OptionValue[ReadGridFunction, options, Iteration], options];
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
    {files = FindSimulationFiles[run, getLeafName[var, dims, opts]]},
    If[files === {},
       notFound[run,var,dims,opts]];
    Union@@Map[CallProvidedFunction["GridFunctions", fn, {#, "Variable" -> var, opts}] &,
               files]];

Options[ReadIterations] = FilterRules[Options[ReadGridFunction], Except["Iteration"]];
DocumentationBuilder`OptionDescriptions["ReadIterations"] = {
    "Map"             -> "The map to read from.",
    "RefinementLevel" -> "The refinement level to read from.",
    "TimeLevel"       -> "The time level to read from.",
    "StripGhostZones" -> "Whether to remove ghost zones from the data.",
    "Variable"        -> "The variable to read. This option is no longer used."
  };
ReadIterations[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  fileUnion["ReadIterations", run, var, parseDims[dims], DeleteCases[ApplyDefaults[run, var, {opts}],
                                                          "Iteration" -> _]];

Options[ReadMaps] = FilterRules[Options[ReadGridFunction], Except["Map"]];
DocumentationBuilder`OptionDescriptions["ReadMaps"] = {
    "Iteration"       -> "The iteration to read from.",
    "RefinementLevel" -> "The refinement level to read from.",
    "TimeLevel"       -> "The time level to read from.",
    "StripGhostZones" -> "Whether to remove ghost zones from the data.",
    "Variable"        -> "The variable to read. This option is no longer used."
  };

ReadMaps[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  fileUnion["ReadMaps", run, var, parseDims[dims], 
            Append[DeleteCases[ApplyDefaults[run, var, {opts}],"Map" -> _, Infinity],
                   "Map" -> All]];

Options[ReadRefinementLevels] = FilterRules[Options[ReadGridFunction], Except["RefinementLevel"]];
DocumentationBuilder`OptionDescriptions["ReadRefinementLevels"] = {
    "Iteration"       -> "The iteration to read from.",
    "Map"             -> "The map to read from.",
    "TimeLevel"       -> "The time level to read from.",
    "StripGhostZones" -> "Whether to remove ghost zones from the data.",
    "Variable"        -> "The variable to read. This option is no longer used."
  };

ReadRefinementLevels[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  fileUnion["ReadRefinementLevels", run, var, parseDims[dims], DeleteCases[ApplyDefaults[run, var, {opts}],
                                                                "RefinementLevel" -> _]];

Options[ReadTimeLevels] = FilterRules[Options[ReadGridFunction], Except["TimeLevel"]];
DocumentationBuilder`OptionDescriptions["ReadTimeLevels"] = {
    "Iteration"       -> "The iteration to read from.",
    "Map"             -> "The map to read from.",
    "RefinementLevel" -> "The refinement level to read from.",
    "StripGhostZones" -> "Whether to remove ghost zones from the data.",
    "Variable"        -> "The variable to read. This option is no longer used."
  };

ReadTimeLevels[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  fileUnion["ReadTimeLevels", run, var, parseDims[dims], DeleteCases[ApplyDefaults[run, var, {opts}],
                                                                "TimeLevel" -> _]];

Options[ReadTime] = Options[ReadGridFunction];
ReadTime[run_String, var_String, dims1:DimsPattern, opts:OptionsPattern[]] :=
  Module[
    {options, fileName, dims = parseDims[dims1]},
    options = ApplyDefaults[run, var, {opts}];
    fileName = getFileOfIt[run, getLeafName[var, dims, options], OptionValue[ReadGridFunction, options, Iteration],options];
    If[fileName === None,
       notFound[run,var,dims,options]];
    CallProvidedFunction["GridFunctions", "ReadTime", {fileName, "Variable" -> var, options}]
];

FindGridFunctions[sim_String] :=
  CallProvidedFunction["GridFunctions", "FindGridFunctions", {sim}];

ReadGridFunctionDimensions[sim_String, varName_String] :=
  CallProvidedFunction["GridFunctions", "ReadGridFunctionDimensions", {sim, varName}];

Options[getFileOfIt] = Options[ReadGridFunction];
getFileOfIt[run_String, leafName:(_String|_RegularExpression), it_Integer, opts:OptionsPattern[]] :=
  Module[{files, itss, haveIts},
    files = FindSimulationFiles[run, leafName];
    itss = Select[Map[{#, getFileIts[#,opts]} &, files], Length[#[[2]]] > 0 &];
    haveIts = Select[itss, it >= First[#[[2]]] && it <= Last[#[[2]]] &];
    If[Length[haveIts] === 0,
       None,
       haveIts[[-1,1]]]];

getFileIts[fileName_, opts:OptionsPattern[]] :=
  CallProvidedFunction["GridFunctions","ReadIterations",{fileName,opts}];

getLeafName[var_String, dims:DimsPattern, options_List] :=
  CallProvidedFunction["GridFunctions", "ToFileName", {var, dims, FilterRules[options, Except["StripGhostZones"]]}];

(***************************************************************************************)
(* Multipatch *)
(***************************************************************************************)

(* Suitable for use with ListDensityPlot. Example:

ListDensityPlot[
 ReadMultipatchGridFunction[run, "Psi4r", "xy", Log10[Abs[#]] &, 
  Iteration -> 0], PlotRange -> All, ColorFunctionScaling -> False, 
  ColorFunction -> cf, AspectRatio -> Automatic]

Requires output of data and x and y coordinates in xy and yz plane.
xy only needed for map 0, and yz only needed for maps 1 to 4.

Useful, but not the end goal.  Eventually, we probably want to attach
coordinates to DataRegions as additional attributes and read these in
modified plotting functions.  We would also need a way to represent
several patches as a single object. *)

ReadMultipatchGridFunction[run_, var_, "xy", fn_, opts___] :=
 Module[{fs, xs, ys, gs},
  fs = MapThread[
    fn@ReadGridFunction[run, var, #1, Map -> #2, opts] &, {{"xy", 
      "yz", "yz", "yz", "yz"}, {0, 1, 2, 3, 4}}];
  xs = Join[{GetCoordinate[fs[[1]], 1]}, 
    Table[ReadGridFunction[run, "x", "yz", Map -> m, opts], {m, 1, 
      4}]];
  ys = Join[{GetCoordinate[fs[[1]], 2]}, 
    Table[ReadGridFunction[run, "y", "yz", Map -> m, opts], {m, 1, 
      4}]];
  gs = Join @@ 
    MapThread[
     Flatten[MapThread[List, ToListOfData /@ {#1, #2, #3}, 2], 
       1] &, {xs, ys, fs}]];

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
