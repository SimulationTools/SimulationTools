(* ::Package:: *)

BeginPackage["GridFunction`", {"CarpetHDF5`", "RunFiles`"}];

ReadGridFunction;
ReadIterations;
ReadMaps;
ReadRefinementLevels;
ReadTimeLevels;
ReadVariables;
ReadTime;

Begin["`Private`"];

Options[ReadGridFunction] = {Variable -> First, Iteration -> First,
  RefinementLevel -> First, Map -> First, StripGhostZones -> True,
  VerboseRead -> False};
ReadGridFunction[run_, var_, opts:OptionsPattern[]] :=
  ReadCarpetHDF5VariableFromRun[run, var, opts];

Options[ReadIterations] = {RefinementLevel -> First};
ReadIterations[run_, var_, opts:OptionsPattern[]] :=
  CarpetHDF5Iterations[FindFirstRunFile[run, var],
    If[OptionValue[RefinementLevel] != None,
      OptionValue[RefinementLevel],
      First[ReadRefinementLevels[run, var, opts]]]];

Options[ReadMaps] = {};
ReadMaps[run_, var_, opts:OptionsPattern[]] :=
  CarpetHDF5Maps[FindFirstRunFile[run, var]];

Options[ReadRefinementLevels] = {};
ReadRefinementLevels[run_, var_, opts:OptionsPattern[]] :=
  CarpetHDF5RefinementLevels[FindFirstRunFile[run, var]];

Options[ReadTimeLevels] = {};
ReadTimeLevels[run_, var_, opts:OptionsPattern[]] :=
  CarpetHDF5TimeLevels[FindFirstRunFile[run, var]];

Options[ReadVariables] = {};
ReadVariables[run_, var_, opts:OptionsPattern[]] :=
  CarpetHDF5Variables[FindFirstRunFile[run, var]];

Options[ReadTime] = {Variable -> First, Iteration -> First,
  RefinementLevel -> First, Map -> First};
ReadTime[run_, var_, opts:OptionsPattern[]] :=
  CarpetHDF5Time[FindFirstRunFile[run, var],
    If[OptionValue[Variable] != First,
       OptionValue[Variable],
       First[ReadVariables[run, var]]],
    If[OptionValue[Map] != First,
      OptionValue[Map],
      First[ReadMaps[run, var]]],
    If[OptionValue[RefinementLevel] != First,
      OptionValue[RefinementLevel],
      First[ReadRefinementLevels[run, var]]],
    If[OptionValue[Iteration] != First,
      OptionValue[Iteration],
      First[ReadIterations[run, var,
        Sequence@@FilterRules[{opts}, Options[ReadIterations]]]]]];

End[];

EndPackage[];
