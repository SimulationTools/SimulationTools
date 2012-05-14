
BeginPackage["GridFunctions`", {"RunFiles`", "DataTable`", "Memo`", "Providers`", "Error`"}];

ReadGridFunction::usage = "ReadGridFunction[run, var, dims] reads variable var from run and returns it as a DataRegion object.  dims is a list of the dimensions to read, and these dimensions must correspond to available output.  Dimensions in dims can be given either as coordinate names (\"x\", \"y\", \"z\") or as numbers (1, 2, 3).  The order of dims is not important.  Optional arguments: Map -> Automatic | mapnum specifies the map for multipatch data files, StripGhostZones -> True|False determines whether the ghost zones are removed from the variable before it is returned.";
ReadIterations::usage = "ReadIterations[run, var, dims] reads the iterations present for the grid function var in the directory run.";
ReadMaps::usage = "ReadMaps[run, var, dims] reads the multipatch maps present  for the grid function var in the directory run.";
ReadRefinementLevels::usage = "ReadRefinementLevels[run, var, dims] reads the refinement levels present for the grid function var in the directory run.";
ReadTimeLevels::usage = "ReadTimeLevels[run, var, dims] reads the timelevels present for the grid function var in the directory run.";
ReadVariables::usage = "ReadVariables[run, var, dims] reads the variable names present for the grid function var in the directory run.";


ReadTime::usage = "ReadTime[run, var, it, rl] reads the time associated with the iteration it on refinement level rl of the CarpetHDF5 file named var in the directory run.";

StripGhostZones::usage = "StripGhostZones is a boolean option to various CarpetHDF5 functions which indicates that ghost zones should be removed";


Begin["`Private`"];

DimsPattern = _List;

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

    Print["options = ", options];
    options];


Options[ReadGridFunction] = {
    "Iteration"       -> Automatic,
    "Map"             -> Automatic,
    "RefinementLevel" -> Automatic,
    "TimeLevel"       -> Automatic,
    "StripGhostZones" -> True
  };

ReadGridFunction[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  Module[
    {options, fileName},
    options = ApplyDefaults[run, var, {opts}];
    fileName = getFileOfIt[run, getLeafName[var, dims, options], OptionValue[ReadGridFunction, options, Iteration]];
    CallProvidedFunction["GridFunctions", "ReadData", {fileName, "Variable" -> var, options}]
];

Options[ReadIterations] = FilterRules[Options[ReadGridFunction], Except["Iteration"]];
ReadIterations[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  Module[
    {options},
    options = ApplyDefaults[run, var, {opts}];
    Union@@Map[CallProvidedFunction["GridFunctions", "ReadIterations", {#, options}] &,
        FindRunFiles[run, getLeafName[var, dims, options]]]
];

Options[ReadMaps] = FilterRules[Options[ReadGridFunction], Except["Map"]];
ReadMaps[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  Module[
    {options},
    options = ApplyDefaults[run, var, {opts}];
    Union@@Map[CallProvidedFunction["GridFunctions", "ReadMaps", {#, opts}] &,
        FindRunFile[run, getLeafName[var, dims, options]]]
];

Options[ReadRefinementLevels] = FilterRules[Options[ReadGridFunction], Except["RefinementLevel"]];
ReadRefinementLevels[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  Module[
    {options},
    options = ApplyDefaults[run, var, {opts}];
    Union@@Map[CallProvidedFunction["GridFunctions", "ReadRefinementLevels", {#, opts}] &,
        FindRunFile[run, getLeafName[var, dims, options]]]
];

Options[ReadTimeLevels] = FilterRules[Options[ReadGridFunction], Except["TimeLevel"]];
ReadTimeLevels[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  Module[
    {leafName},
    leafName = CallProvidedFunction["GridFunctions", "ToFileName", {var, dims, opts}];
    Union@@Map[CallProvidedFunction["GridFunctions", "ReadTimeLevels", {#, opts}] &,
        FindRunFilesFromPattern[run, leafName]]
];

Options[ReadVariables] = FilterRules[Options[ReadGridFunction], Except["Variable"]];
ReadVariables[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  Module[
    {leafName},
    leafName = CallProvidedFunction["GridFunctions", "ToFileName", {var, dims, opts}];
    Union@@Map[CallProvidedFunction["GridFunctions", "ReadVariables", {#, opts}] &,
        FindRunFilesFromPattern[run, leafName]]
];


getFileOfIt[run_String, leafName:(_String|_RegularExpression), it_Integer, opts:OptionsPattern[]] :=
  Module[{files, itss, haveIts},
    files = FindRunFile[run, leafName];
    itss = Map[{#, getFileIts[#]} &, files];
    haveIts = Select[itss, it >= First[#[[2]]] && it <= Last[#[[2]]] &];
    If[Length[haveIts] === 0,
       Error["Iteration " <> ToString[it] <> " not found in file "<>leafName<>
             " in run " <> run]];
    haveIts[[1,1]]];

getFileIts[fileName_] :=
  CallProvidedFunction["GridFunctions","ReadIterations",{fileName}];

getLeafName[var_String, dims:DimsPattern, options_List] :=
  CallProvidedFunction["GridFunctions", "ToFileName", {var, dims, FilterRules[options, Except["StripGhostZones"]]}];

End[];

EndPackage[];
