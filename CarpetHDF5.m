(* ::Package:: *)

(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["CarpetHDF5`",{"DataRegion`", "Memo`", "Profile`", "RunFiles`", "ReadHDF5`"}];

(* New CarpetHDF5 API *)

ReadGridFunction::usage = "ReadGridFunction[run, var, it, rl] reads a grid function from the CarpetHDF5 file named var in the directory run and returns it as a DataRegion object.  The iteration is given by it, and the refinement level by rl (rl defaults to the first level in the file if it is omitted).  Optional arguments: Variable -> Automatic | varname specifies the variable to read from files containing more than one variable, Map -> Automatic | mapnum specifies the map for multipatch data files, StripGhostZones -> True|False determines whether the ghost zones are removed from the variable before it is returned.";
ReadIterations::usage = "ReadIterations[run, var, rl] reads the iterations present in the CarpetHDF5 file named var in the directory run.  Only those iterations present on refinement level rl are returned.  rl defaults to All if it is omitted.";
ReadMaps::usage = "ReadMaps[run, var] reads the multipatch maps present in the CarpetHDF5 file named var in the directory run.";
ReadRefinementLevels::usage = "ReadRefinementLevels[run, var] reads the refinement levels present in the CarpetHDF5 file named var in the directory run.";
ReadTimeLevels::usage = "ReadTimeLevels[run, var] reads the timelevels present in the CarpetHDF5 file named var in the directory run.";
ReadVariables::usage = "ReadVariables[run, var] reads the variable names present in the CarpetHDF5 file named var in the directory run.";
ReadTime::usage = "ReadTime[run, var, it, rl] reads the time associated with the iteration it on refinement level rl of the CarpetHDF5 file named var in the directory run.";

StripGhostZones::usage = "StripGhostZones is a boolean option to various CarpetHDF5 functions which indicates that ghost zones should be removed";

(* Exported symbols *)

ReadCarpetHDF5;
CarpetHDF5DatasetName;
ReadCarpetHDF5Variable;
ReadCarpetHDF5VariableFromRun;
ReadCarpetHDF5Components;
CarpetHDF5Iterations;
CarpetHDF5Components;
CarpetHDF5Maps;
CarpetHDF5RefinementLevels;
CarpetHDF5TimeLevels;
CarpetHDF5Variables;
CarpetHDF5FileInfo;
CarpetHDF5Attribute;
CarpetHDF5Attributes;
CarpetHDF5Time;

CarpetHDF5Manipulate;
CarpetManipulatePlotFunction;

Begin["`Private`"];

(***************************************************************************************)
(* Private functions *)
(***************************************************************************************)

(* Gather various information about the datasets in a file *)
DefineMemoFunction[datasetAttributes[file_],
  Module[{attributeRules, datasets, dsattrs, tempCell},
    Profile["datasetAttributes[file]",

    (* Convert dataset name strings into rules *)
    Block[{ShowHDF5Progress=True},
      datasets = Profile["Datasets", Datasets[file]]];

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

    (* If an attribute isn't found for a dataset, set it to Null *)
    Profile["datasetAttributes/Replace2",
    dsattrs = dsattrs /. {"var" -> Null, "it" -> Null, "tl" -> Null,
                          "rl" -> Null, "c" -> Null, "m" -> Null}];

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
  Sort[Cases[DeleteDuplicates[datasets[[All, attr]]], Except[Null]]];

datasetAttribute[file_String, attr_] :=
  datasetAttribute[datasetAttributes[file], attr];

DefineMemoFunction[Datasets[file_],
  ReadHDF5[file, "Datasets"]
];

Annotations[file_, ds_]:= Profile["Annotations", ReadHDF5[file, {"Annotations", ds}]];
Dims[file_, ds_]:= ReadHDF5[file, {"Dimensions", ds}];
HDF5Data[file_, ds_] := Profile["HDF5Data", ReadHDF5[file, {"Datasets", ds}]];

firstOrNone[l_List] :=
  If[l === {}, None, First[l]];

(***************************************************************************************)
(* Public functions *)
(***************************************************************************************)

(* Metadata *)

CarpetHDF5Iterations[run_String, var_String, args___] :=
  CarpetHDF5Iterations[FindRunFile[run, var][[1]], args];

CarpetHDF5Iterations[file_String] := datasetAttribute[file, 2];

CarpetHDF5Iterations[file_String, rl_Integer] :=
  datasetAttribute[datasetsWith[file, 4 -> rl], 2];

CarpetHDF5Components[file_] := datasetAttribute[file, 5];

CarpetHDF5Components[file_, it_, rl_] := 
  Profile["CarpetHDF5Components",
  datasetAttribute[datasetsWith[file, {2 -> it, 4 -> rl}], 5]];

DefineMemoFunction[CarpetHDF5Maps[file_], Profile["CarpetHDF5Maps",datasetAttribute[file, 6]]];

DefineMemoFunction[CarpetHDF5RefinementLevels[file_], Profile["CarpetHDF5RefinementLevels", datasetAttribute[file, 4]]];

CarpetHDF5TimeLevels[file_]       := datasetAttribute[file, 3];

DefineMemoFunction[CarpetHDF5Variables[file_], Profile["CarpetHDF5Variables", datasetAttribute[file, 1]]];

CarpetHDF5FileInfo[file_]:=
  Module[{iterations, components, maps, refLevels, timeLevels, vars},
  iterations = CarpetHDF5Iterations[file];
  components = CarpetHDF5Components[file];
  maps = CarpetHDF5Maps[file];
  refLevels = CarpetHDF5RefinementLevels[file];
  timeLevels = CarpetHDF5TimeLevels[file];
  vars = CarpetHDF5Variables[file];
  {Iterations->iterations, Components->components, Maps->maps, RefinementLevels->refLevels, TimeLevels->timeLevels, Vars->vars}
];

CarpetHDF5DatasetName[var_String, it_Integer, m:(_Integer|None), rl:(_Integer|None), c:(_Integer|None)] :=Module[{map="", component="", reflevel=""},
  If[m =!= None, map=" m="<>ToString[m]];
  If[c =!= None, component=" c="<>ToString[c]];
  If[rl =!= None, reflevel=" rl="<>ToString[rl]];
  "/" <> var <> " it=" <> ToString[it] <> " tl=0"<>map<> reflevel <> component];

CarpetHDF5Attributes[file_String, var_String, map:(None | _Integer), rl_Integer, it_Integer] :=
 Module[{dsList, c, dsName, ds},
  Profile["CarpetHDF5Attributes",
  dsList =
   Profile["datasetsWith", datasetsWith[
     file, {1 -> var , 2 -> it, 4 -> rl,
      6 -> (map /. None -> Null) }]];
  If[dsList === {}, Throw["Cannot find dataset in file"]];
  ds = First[dsList];
  c = ds[[5]];
  dsName = CarpetHDF5DatasetName[var, it, map, rl, c];
  Annotations[file, dsName]]];

CarpetHDF5Attribute[file_String, var_String, map_, rl_Integer, it_Integer, attr_String] :=
  Module[{attrs = CarpetHDF5Attributes[file, var, map, rl, it], value},
    value = attr /. attrs;
    If[value === attr,
      Throw["Attribute " <> attr <> " not found in file " <> file <>
            " dataset " <> ToString[{var,map,rl,it}]]];
    Return[value]];

CarpetHDF5Time[file_String, var_String, map_, rl_Integer, it_Integer] :=
  Profile["CarpetHDF5Time",
    ToExpression[CarpetHDF5Attribute[file, var, map, rl, it, "time"]]];

(* Data *)

Options[ReadCarpetHDF5] = {StripGhostZones -> True};
ReadCarpetHDF5[file_String, ds_List, OptionsPattern[]] :=
(* This should be renamed ReadCarpetHDF5Dataset and should be internal *)
 Profile["ReadCarpetHDF5",
 Module[{data, annots, dims, origin, spacing, name, idx, strip, dr, ghosts, posns, allds, time},
  strip = OptionValue[StripGhostZones];

  If[Apply[Or,Map[(!StringQ[#])&, ds]],
    Throw["ReadCarpetHDF5: expected a string dataset name, but instead got " <>ToString[ds]]];

  If[FileType[file] === None,
    Throw["File " <> file <> " not found"]];

  data = HDF5Data[file, ds];

  annots = Annotations[file, ds];

  dims = Reverse /@ Dims[file, ds];

  origin = "origin" /. annots;
  spacing = "delta" /. annots;
  name = "name" /. annots;

  ghosts = ("cctk_nghostzones" /. annots) /. "cctk_nghostzones" -> 0;

  time = "time" /. annots;
  dr = MapThread[MakeDataRegion, {data, name, dims, origin, spacing, time}];
  If[strip, MapThread[Strip, {dr, ghosts}], dr]
]];

ReadCarpetHDF5[file_String, ds_String, opts:OptionsPattern[]] := First[ReadCarpetHDF5[file, {ds}, opts]];

Options[ReadCarpetHDF5Components] = {StripGhostZones -> True};
ReadCarpetHDF5Components[file_, var_, it_, rl_, map_, opts___] :=
  Profile["ReadCarpetHDF5Components",
  Module[{filePrefix, fileNames, datasets, pattern, MultiFile, Filetype1D, Filetype2D, components},
    If[FileType[file] === None,
      Throw["File " <> file <> " not found in ReadCarpetHDF5Components"]];

    Filetype1D = RegularExpression[".*\\.[dxyz]\\.h5"];
    Filetype2D = RegularExpression[".*\\.[xyz]{2}\\.h5"];
    MultiFile = RegularExpression[".*\\.file_\\d+\\.h5"];

    Which[
    StringMatchQ[file, Filetype1D] || StringMatchQ[file, Filetype2D],
      components = CarpetHDF5Components[file, it, rl];
		  If[Length[components]==0,
			  datasets = {ReadCarpetHDF5[file, CarpetHDF5DatasetName[var, it, map, rl, None], opts]};
			,
			  datasets = ReadCarpetHDF5[file, Map[CarpetHDF5DatasetName[var, it, map, rl, #] &, components], opts];
		  ];,

		StringMatchQ[file, MultiFile],
      filePrefix = StringReplace[file, ".file_0.h5" -> ""];
      pattern = StringReplace[file, ".file_0.h5" -> ".file_*.h5"];
      fileNames = Profile["FileNames", FileNames[FileNameTake[pattern], DirectoryName[pattern]]];
      components = Flatten[DeleteDuplicates[StringCases[fileNames,"file_"~~x:DigitCharacter..:>ToExpression[x]]]];
      datasets = Map[ReadCarpetHDF5[filePrefix<>".file_"<>ToString[#]<>".h5",
                       CarpetHDF5DatasetName[var, it, map, rl, #], opts] &, components];,
    True,
      datasets={ReadCarpetHDF5[file, CarpetHDF5DatasetName[var, it, map, rl, None], opts]};
    ];

    datasets
]];

ReadCarpetHDF5Variable[file_String, var_String, it_Integer, rl_Integer, map_:None, opts___]:=
  Profile["ReadCarpetHDF5Variable",
    MergeDataRegions[ReadCarpetHDF5Components[file, var, it, rl, map, opts]]];

Options[ReadCarpetHDF5Variable] = {Iteration -> None, Variable -> None, RefinementLevel -> None, Map -> None};
ReadCarpetHDF5Variable[file_String, opts:OptionsPattern[]]:=
  Module[{it, rl, var, map},
    If[FileType[file] === None, Throw["ReadCarpetHDF5Variable: File " <> file <> " not found"]];
    var = If[OptionValue[Variable] =!= None, OptionValue[Variable], firstOrNone[CarpetHDF5Variables[file]]];
    it = If[OptionValue[Iteration] =!= None, OptionValue[Iteration], firstOrNone[CarpetHDF5Iterations[file]]];
    rl = If[OptionValue[RefinementLevel] =!= None, OptionValue[RefinementLevel], firstOrNone[CarpetHDF5RefinementLevels[file]]];
    map = If[OptionValue[Map] =!= None, OptionValue[Map], firstOrNone[CarpetHDF5Maps[file]]];
    ReadCarpetHDF5Variable[file, var, it, rl, map, Sequence@@FilterRules[{opts}, Options[ReadCarpetHDF5]]]];

ReadCarpetHDF5VariableFromRun[run_String, var_String, opts:OptionsPattern[]] :=
  Module[{},
    ReadCarpetHDF5Variable[Module[{files = FindRunFile[run, var]}, 
      If[files==={},Throw["File "<>var<>" not found in run "<>run]]; files[[1]]], opts]];

(* Data manipulation *)

Options[CarpetHDF5Manipulate] = {CarpetHDF5ManipulatePlotFunction -> DataRegionDensityPlot};
CarpetHDF5Manipulate[file_, var_String, rl_, map_:None, opts:OptionsPattern[]]:=
(* Candidate for deletion? *)
  Module[{data, axesOrigin, numDims, plotType},
  data = Table[ReadCarpetHDF5Variable[file, var, it, rl, map, Sequence@@FilterRules[{opts}, Options[ReadCarpetHDF5Variable]]], {it, CarpetHDF5Iterations[file]}];
  numDims = GetNumDimensions[data[[1]]];

  (*data = SliceData[#, {2,3}]&/@data;*)
  axesOrigin = {Min[GetOrigin/@data], Automatic};

  If[numDims == 1,
    plotType = DataRegionPlot;
  , If[numDims == 2,
    plotType = OptionValue[CarpetManipulatePlotFunction];
  , Throw["CarpetHDF5Manipulate does not support HDF5 data with dimension "<>ToString[numDims]<>"."];
  ]];

  Manipulate[plotType[data[[i]], PlotLabel->"t="<>ToString[GetTime[data[[i]]]] (* ,
                             PlotRange->{Min[data], Max[data]} *), AxesOrigin-> axesOrigin],
             {{i, 1, "Iteration"}, 1, Length[data], 1}]
];

CarpetHDF5Manipulate[file_, opts___]:= Module[{var, rl, maps, map},
(* Candidate for deletion? *)
  var  = First[Vars /. CarpetHDF5FileInfo[file]];
  rl   = 0;
  maps = Maps /. CarpetHDF5FileInfo[file];
  map  = If[Length[maps]>0, First[maps], None];

  CarpetHDF5Manipulate[file, var, rl, map, opts]
];

getRL[run_, var_, rl_] :=
  Switch[rl,
    Automatic, First[ReadRefinementLevels[run, var]],
    All, Unevaluated[Sequence[]],
    _, rl];

getMap[run_,var_,map_] :=
  Module[{m},
    If[map === Automatic,
        m = ReadMaps[run, var];
        If[m === {}, None, First[m]],
      map]];

getVar[run_, var_, variable_] :=
  If[variable === Automatic,
    First[ReadVariables[run, var]],
    variable];

(***************************************************************************************)
(* New API *)
(***************************************************************************************)

Options[ReadGridFunction] = {Variable -> Automatic,
  Map -> Automatic, StripGhostZones -> True};
ReadGridFunction[run_String, var_String, it_Integer, rl:(_Integer|Automatic):Automatic, opts:OptionsPattern[]] :=
  Profile["ReadGridFunction",
  ReadCarpetHDF5Variable[FindFirstRunFile[run, var], getVar[run, var, OptionValue[Variable]],
    it, getRL[run, var, rl], getMap[run, var, OptionValue[Map]],
    FilterRules[{opts}, Options[ReadCarpetHDF5Variable]]]];

Options[ReadIterations] = {};
ReadIterations[run_, var_, rl_:All, opts:OptionsPattern[]] :=
  CarpetHDF5Iterations[FindFirstRunFile[run, var],
    getRL[run, var, rl]];

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

Options[ReadTime] = {Variable -> Automatic, Map -> Automatic};
ReadTime[run_, var_, it_, rl_:Automatic, opts:OptionsPattern[]] :=
  Profile["ReadTime",
    CarpetHDF5Time[FindFirstRunFile[run, var],
    getVar[run, var, OptionValue[Variable]],
    getMap[run, var, OptionValue[Map]],
    getRL[run, var, rl],
    it]];

End[];

EndPackage[];

