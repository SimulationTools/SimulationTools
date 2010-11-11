(* ::Package:: *)

(* This package was originally written by Ian Hinder and modified to support arbitrary dimensional data by Barry Wardell *)

BeginPackage["CarpetHDF5`",{"DataRegion`", "Memo`"}];

(* Exported symbols *)

ReadCarpetHDF5;
CarpetHDF5DatasetName;
ReadCarpetHDF5Variable::usage = "ReadCarpetHDF5Variable[file, var, it, rl, map] reads a variable from a Carpet HDF5 file";
ReadCarpetHDF5Components;
StripGhostZones;
VerboseRead;
PreloadCarpetHDF5Data;
CarpetHDF5Iterations;
CarpetHDF5Components;
CarpetHDF5Maps;
CarpetHDF5RefinementLevels;
CarpetHDF5TimeLevels;
CarpetHDF5Variables;
CarpetHDF5FileInfo;

CarpetHDF5Manipulate;
CarpetManipulatePlotFunction;

Begin["`Private`"];

(* If the h5mma is not found, then just use Mathematica's built-in HDF5 support *)
$h5mma = If[Quiet[Get["h5mma`"]]===$Failed, False, True];

import[x__] := If[$h5mma, ImportHDF5[x], Import[x]];

(* Gather various information about the datasets in a file *)
DefineMemoFunction[datasetAttributes[file_],
  Module[{attributeRules, dsattrs},
    (* Convert dataset name strings into rules *)
    attributeRules = StringCases[
      Datasets[file], {"it=" ~~ x : NumberString :> ("it" -> ToExpression[x]),
      "tl=" ~~ x : NumberString :> ("tl" -> ToExpression[x]),
      "rl=" ~~ x : NumberString :> ("rl" -> ToExpression[x]),
      "c=" ~~ x : NumberString :> ("c" -> ToExpression[x]),
      "m=" ~~ x : NumberString :> ("m" -> ToExpression[x]),
      StartOfString ~~ "/" ~~ Shortest[x : __] ~~ " it=" :> ("var" -> x)},
      Overlaps -> True];

    dsattrs = {"var", "it", "tl", "rl", "c", "m"} /. attributeRules;

    (* If an attribute isn't found for a dataset, set it to Null *)
    dsattrs = dsattrs /. {"var" -> Null, "it" -> Null, "tl" -> Null,
                          "rl" -> Null, "c" -> Null, "m" -> Null};

    dsattrs
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

datasetAttribute[datasets_List, attr_] := Sort[Cases[DeleteDuplicates[datasets[[All, attr]]], Except[Null]]];
datasetAttribute[file_String, attr_] := datasetAttribute[datasetAttributes[file], attr];

CarpetHDF5Iterations[file_] := datasetAttribute[file, 2];

CarpetHDF5Iterations[file_String, rl_Integer:0] :=
  datasetAttribute[datasetsWith[file, 4 -> rl], 2];

CarpetHDF5Components[file_] := datasetAttribute[file, 5];

CarpetHDF5Components[file_, it_, rl_] := 
  datasetAttribute[datasetsWith[file, {2 -> it, 4 -> rl}], 5];

CarpetHDF5Maps[file_]             := datasetAttribute[file, 6];
CarpetHDF5RefinementLevels[file_] := datasetAttribute[file, 4];
CarpetHDF5TimeLevels[file_]       := datasetAttribute[file, 3];
CarpetHDF5Variables[file_]        := datasetAttribute[file, 1];

CarpetHDF5FileInfo[file_]:=
  Module[{iterations, components, maps, refLevels, timeLevels, vars},
  iterations = CarpetHDF5Iterations[file];

  components = CarpetHDF5Components[file];

  maps = CarpetHDF5Maps[file];

  refLevels = CarpetHDF5RefinementLevels[file];

  timeLevels = CarpetHDF5TimeLevels[file];

  vars = CarpetHDF5Variables[file];

  {Iterations->iterations, Components->components, Maps->maps, RefinemeneLevels->refLevels, TimeLevels->timeLevels, Vars->vars}
];

CarpetHDF5DatasetName[var_String, it_Integer, m:(_Integer|None), rl:(_Integer|None), c:(_Integer|None)] :=Module[{map="", component="", reflevel=""},
  If[m =!= None, map=" m="<>ToString[m]];
  If[c =!= None, component=" c="<>ToString[c]];
  If[rl =!= None, reflevel=" rl="<>ToString[rl]];
  "/" <> var <> " it=" <> ToString[it] <> " tl=0"<>map<> reflevel <> component];

Options[ReadCarpetHDF5] = {StripGhostZones -> True, VerboseRead -> False};

DefineMemoFunction[Datasets[file_],
  import[file, "Datasets"]
];

DefineMemoFunction[Annotations[file_, ds_],
  import[file, {"Annotations", ds}]
];

DefineMemoFunction[Dims[file_, ds_],
  import[file, {"Dimensions", ds}]
];

DefineMemoFunction[HDF5Data[file_, dataset_String],
  import[file, {"Datasets", dataset}]
];

PreloadCarpetHDF5Data[file_]:= Module[{allData, data, annots, dims, ds},
  ds = Datasets[file];
  Annotations[file, #] & /@ ds;
  Dimensions[file,  #] & /@ ds;
  HDF5Data[file,  #] & /@ ds;
];

ReadCarpetHDF5[file_String, ds_, OptionsPattern[]] :=
 Module[{dsName, data, annots, dims, origin, spacing, name, idx, strip, verbose, reg, ghosts, posns, allds, time},
  strip = OptionValue[StripGhostZones];
  verbose = OptionValue[VerboseRead];

  If[!StringQ[ds] && !IntegerQ[ds],
    Throw["ReadCarpetHDF5: expected a string or integer dataset specification, but instead got " <>ToString[ds]]];

  If[verbose, Print["Reading File: "<>file]];

  If[IntegerQ[ds], dsName = DataSets[file][[idx]], dsName = ds];

  If[verbose, Print["Reading Data"]];
  data = HDF5Data[file, dsName];

  If[verbose, Print["Reading Annotations"]];
  annots = Annotations[file, dsName];

  If[verbose, Print["Reading Dimensions"]];
  dims = Reverse[Dims[file, dsName]];

  origin = "origin" /. annots;
  spacing = "delta" /. annots;
  name = "name" /. annots;

  ghosts = "cctk_nghostzones" /. annots;
  If[ghosts=="cctk_nghostzones", ghosts=0];

  time = "time" /. annots;
  reg = MakeDataRegion[data, name, dims, origin, spacing, time];
  If[strip, Strip[reg, ghosts], reg]
];

Options[ReadCarpetHDF5Components] = {StripGhostZones -> True};

ReadCarpetHDF5Components[file_, var_, it_, rl_, map_, opts___] :=
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
			  datasets = Map[ReadCarpetHDF5[file, CarpetHDF5DatasetName[var, it, map, rl, #], opts] &, components];
		  ];,

		StringMatchQ[file, MultiFile],
      filePrefix = StringReplace[file, ".file_0.h5" -> ""];
      pattern = StringReplace[file, ".file_0.h5" -> ".file_*.h5"];
      fileNames = FileNames[FileNameTake[pattern], DirectoryName[pattern]];
      components = Flatten[DeleteDuplicates[StringCases[fileNames,"file_"~~x:DigitCharacter..:>ToExpression[x]]]];
      datasets = Map[ReadCarpetHDF5[filePrefix<>".file_"<>ToString[#]<>".h5",
                       CarpetHDF5DatasetName[var, it, map, rl, #], opts] &, components];,
    True,
      datasets={ReadCarpetHDF5[file, CarpetHDF5DatasetName[var, it, map, rl, None], opts]};
    ];

    datasets
];

ReadCarpetHDF5Variable[file_, var_, it_, rl_, map_:None, opts___]:=
  MergeDataRegions[ReadCarpetHDF5Components[file, var, it, rl, map, opts]];

firstOrNone[l_List] :=
  If[l === {}, None, First[l]];

Options[ReadCarpetHDF5Variable] = {Iteration -> None, Variable -> None, RefinementLevel -> None, Map -> None};

ReadCarpetHDF5Variable[file_, opts:OptionsPattern[]]:=
  Module[{it, rl, var, map},
    var = If[OptionValue[Variable] =!= None, OptionValue[Variable], firstOrNone[CarpetHDF5Variables[file]]];
    it = If[OptionValue[Iteration] =!= None, OptionValue[Iteration], firstOrNone[CarpetHDF5Iterations[file]]];
    rl = If[OptionValue[RefinementLevel] =!= None, OptionValue[RefinementLevel], firstOrNone[CarpetHDF5RefinementLevels[file]]];
    map = If[OptionValue[Map] =!= None, OptionValue[Map], firstOrNone[CarpetHDF5Maps[file]]];
    ReadCarpetHDF5Variable[file, var, it, rl, map, Sequence@@FilterRules[{opts}, Options[ReadCarpetHDF5]]]];

Options[CarpetHDF5Manipulate] = {CarpetHDF5ManipulatePlotFunction -> DataRegionDensityPlot};
CarpetHDF5Manipulate[file_, var_String, rl_, map_:None, opts:OptionsPattern[]]:= Module[{data, axesOrigin, numDims, plotType},
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
  var  = First[Vars /. CarpetHDF5FileInfo[file]];
  rl   = 0;
  maps = Maps /. CarpetHDF5FileInfo[file];
  map  = If[Length[maps]>0, First[maps], None];

  CarpetHDF5Manipulate[file, var, rl, map, opts]
];

End[];

EndPackage[];
