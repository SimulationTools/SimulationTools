
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

ApplyDefaults[opts, provider] :=
  Do[Which[
      OptionValue[opt] == Automatic && HaveOpt[opt] && HaveDefault[opt],
      OptionValue[opt] = Default[opt],

      OptionValue[opt] != Automatic && HaveOpt[opt],
      OptionValue[opt] = OptionValue[opt],

      OptionValue[opt] == Automatic && HaveOpt[opt] && !HaveDefault[opt],
      Error["Automatic selected, but no default available"],
      
      OptionValue[opt] == Automatic && !HaveOpt[opt] && HaveDefault[opt],
      Error["Default specified for non-existent option"],

      OptionValue[opt] != Automatic && !HaveOpt[opt],
      Error["Unknown option specified"],

      OptionValue[opt] == Automatic && !HaveOpt[opt] && !HaveDefault[opt],
      Continue[]
    ], {opt, options}];

Options[ReadGridFunction] = {
    "Iteration" 	  -> Automatic,
    "Map" 			  -> Automatic,
    "RefinementLevel" -> Automatic,
    "TimeLevel" 	  -> Automatic,
    "Variable" 		  -> Automatic,
    "StripGhostZones" -> True
  };

ReadGridFunction[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  Module[
    {leafName, fileName, explicitOpts},

    explicitOpts = defaultOpts[opts, provider];
    leafName = CallProvidedFunction["GridFunctions", "ToFileName", {var, dims, explicitOpts}];
    fileName = getFileOfIt[run, leafName, it];
    
    CallProvidedFunction["GridFunctions", "ReadGridFunctionData", {fileName, explicitOpts}]
];

Options[ReadIterations] = FilterRules[Options[ReadGridFunction], Except["Iteration"]];
ReadIterations[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  Module[
    {leafName},
    leafName = CallProvidedFunction["GridFunctions", "ToFileName", {var, dims, opts}];
    Union@@Map[CallProvidedFunction["GridFunctions", "ReadIterations", {#, opts}] &,
        FindRunFilesFromPattern[run, leafName]]
];

Options[ReadMaps] = FilterRules[Options[ReadGridFunction], Except["Map"]];
ReadMaps[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  Module[
    {leafName},
    leafName = CallProvidedFunction["GridFunctions", "ToFileName", {var, dims, Map -> All, opts}];
    Union@@Map[CallProvidedFunction["GridFunctions", "ReadMaps", {#, opts}] &,
        FindRunFilesFromPattern[run, leafName]]
];

Options[ReadRefinementLevels] = FilterRules[Options[ReadGridFunction], Except["RefinementLevel"]];
ReadRefinementLevels[run_String, var_String, dims:DimsPattern, opts:OptionsPattern[]] :=
  Module[
    {leafName},
    leafName = CallProvidedFunction["GridFunctions", "ToFileName", {var, dims, opts}];
    Union@@Map[CallProvidedFunction["GridFunctions", "ReadRefinementLevels", {#, opts}] &,
        FindRunFilesFromPattern[run, leafName]]
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


getFileOfIt[run_String, leafName_String, it_Integer, opts:OptionsPattern[]] :=
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


End[];

EndPackage[];