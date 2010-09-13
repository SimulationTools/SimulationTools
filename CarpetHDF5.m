(* ::Package:: *)

(* This package was originally written by Ian Hinder and modified to support arbitrary dimensional data by Barry Wardell *)

BeginPackage["CarpetHDF5`",{"h5mma`", "DataRegion`"}];

(* Exported symbols *)

ReadCarpetHDF5;
ClearCarpetHDF5Cache;
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

(* Carpet HDF5 functions *)

(* Gather various information about a file *)
carpetHDF5List[file_, item_]        := Sort[First /@ Select[DeleteDuplicates[StringCases[Datasets[file],item]], Length[#] > 0 &]];
carpetHDF5ListN[file_, item_String] := carpetHDF5List[file, item<>"="~~x:DigitCharacter..:>ToExpression[x]];

datasetNamesWith[file_, items_List] :=
  Module[{patterns, datasetsMatching, allMatches, matchAll},
    patterns = ((___ ~~ (# <> " ") ~~ ___) &) /@ items;
    datasetsMatching[p_] :=
      Select[Datasets[file], StringMatchQ[#, p] &];
    allMatches = datasetsMatching /@ patterns;
    matchAll = Intersection @@ allMatches];

compOf[ds_] :=
  StringCases[ds, "c="~~x:DigitCharacter..:>ToExpression[x]][[1]];

itOf[ds_] :=
  StringCases[ds, "it="~~x:DigitCharacter..:>ToExpression[x]][[1]];

componentsWith[file_, items_List] :=
  Sort[compOf /@ datasetNamesWith[file,items]];

(*componentsWith[file_, items_List] :=
  Module[{patterns, datasetsMatching, allMatches, matchAll},
(*    patterns = (#<>"="~~DigitCharacter..) & /@ items;*)
    patterns = ((___ ~~ (# <> " ") ~~ ___) &) /@ items;
    datasetsMatching[p_] :=
      Select[Datasets[file], StringMatchQ[#, p] &];
    allMatches = datasetsMatching /@ patterns;
    matchAll = Intersection @@ allMatches;
    compOf[ds_]:=StringCases[ds, "c="~~x:DigitCharacter..:>ToExpression[x]][[1]];
    Sort[compOf /@ matchAll]];*)

CarpetHDF5Iterations[file_] := carpetHDF5ListN[file, "it"];

CarpetHDF5Iterations[file_, rl_:0] := 
  Union[itOf/@datasetNamesWith[file, {"rl="<>ToString@rl}]];

CarpetHDF5Components[file_] := carpetHDF5ListN[file, "c"];

CarpetHDF5Components[file_, it_, rl_] := 
  componentsWith[file, {"it=" <> ToString[it], "rl=" <> ToString[rl]}];

CarpetHDF5Maps[file_]             := carpetHDF5ListN[file, "m"];
CarpetHDF5RefinementLevels[file_] := carpetHDF5ListN[file, "rl"];
CarpetHDF5TimeLevels[file_]       := carpetHDF5ListN[file, "tl"];
CarpetHDF5Variables[file_]        := carpetHDF5List [file, RegularExpression["^/([^ ]+) it="]:>"$1"];

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

CarpetHDF5DatasetName[var_String, it_Integer, m:(_Integer|None), rl_Integer, c:(_Integer|None)] :=Module[{map="", component=""},
  If[m =!= None, map=" m="<>ToString[m]];
  If[c =!= None, component=" c="<>ToString[c]];
  "/" <> var <> " it=" <> ToString[it] <> " tl=0"<>map<>" rl=" <> ToString[rl] <> component];

Options[ReadCarpetHDF5] = {StripGhostZones -> True, VerboseRead -> False};

Datasets[file_]:= Datasets[file] = ImportHDF5[file, "Datasets"];
Annotations[file_]:= Annotations[file] = ImportHDF5[file, "Annotations"];
Dims[file_]:= Dims[file] = ImportHDF5[file, "Dimensions"];
HDF5Data[file_, dataset:(_String|_Integer)]:= HDF5Data[file, dataset] = ImportHDF5[file, {"Datasets", dataset}];

PreloadCarpetHDF5Data[file_]:= Module[{allData, data},
  allData = ImportHDF5[file, "Rules"];
  data = "Data"/. allData;

  Datasets[file] = "Datasets" /. allData;
  Annotations[file] = "Annotations" /. allData;
  Dims[file] = "Dimensions" /. allData;
  (HDF5Data[file, #] = data[[#]])& /@ Range[Length[data]];
];

ReadCarpetHDF5[file_String, ds_, OptionsPattern[]] :=
 Module[{data, annots, dims, origin, spacing, name, idx, strip, verbose, reg, ghosts, posns, allds, time},
  strip = OptionValue[StripGhostZones];
  verbose = OptionValue[VerboseRead];

  If[!StringQ[ds] && !IntegerQ[ds],
    Throw["ReadCarpetHDF5: expected a string or integer dataset specification, but instead got " <>ToString[ds]]];

  If[verbose, Print["Reading File: "<>file]];

  If[IntegerQ[ds], idx = ds,
    If[verbose, Print["Reading Dataset Names"]];
    allds = Datasets[file];
    posns = Position[allds, ds];
    If[posns === {}, Throw["Cannot find dataset " <> ds <> " in HDF5 file " <> file]];
    idx = posns[[1]][[1]]
  ];

  If[verbose, Print["Reading Data"]];
  data = HDF5Data[file, idx];

  If[verbose, Print["Reading Annotations"]];   
  annots = Annotations[file][[idx]];

  If[verbose, Print["Reading Dimensions"]];
  dims = Reverse[Dims[file][[idx]]];

  origin = "origin" /. annots;
  spacing = "delta" /. annots;
  name = "name" /. annots;

  ghosts = "cctk_nghostzones" /. annots;
  If[ghosts=="cctk_nghostzones", ghosts=0];

  time = "time" /. annots;
  reg = MakeDataRegion[data, name, dims, origin, spacing, time];
  If[strip, Strip[reg, ghosts], reg]
];

ClearCarpetHDF5Cache[] :=
  Module[{},
    ClearAll[Datasets, Annotations, Dims, HDF5Data];
    Datasets[file_]:= Datasets[file] = ImportHDF5[file, "Datasets"];
    Annotations[file_]:= Annotations[file] = ImportHDF5[file, "Annotations"];
    Dims[file_]:= Dims[file] = ImportHDF5[file, "Dimensions"];
    HDF5Data[file_, dataset:(_String|_Integer)]:= HDF5Data[file, dataset] = ImportHDF5[file, {"Datasets", dataset}]];

Options[ReadCarpetHDF5Components] = {StripGhostZones -> True};

ReadCarpetHDF5Components[file_, var_, it_, rl_, map_, opts___] :=
  Module[{filePrefix, fileNames, datasets, pattern, MultiFile, Filetype1D, Filetype2D, components},
    If[FileType[file] === None,
      Throw["File " <> file <> " not found in ReadCarpetHDF5Components"]];
 
    (* TODO: Add support for 2D. *)	
    Filetype1D = RegularExpression["\\.[dxyz]\\.h5"];
    Filetype2D = RegularExpression["\\.[xyz]{2}\\.h5"];
    MultiFile = RegularExpression["file_\\d+\\.h5"];
    If[StringCount[file, Filetype1D]>0 || StringCount[file, Filetype2D]>0,
		components = CarpetHDF5Components[file, it, rl];
		If[Length[components]==0,
			datasets = {ReadCarpetHDF5[file, CarpetHDF5DatasetName[var, it, map, rl, None], opts]};
		,
			datasets = Table[ReadCarpetHDF5[file, CarpetHDF5DatasetName[var, it, map, rl, c], opts], {c,components}];
		];
    , If[StringCount[file, MultiFile]>0,
		filePrefix = StringReplace[file, ".file_0.h5" -> ""];
		pattern = StringReplace[file, ".file_0.h5" -> ".file_*.h5"];
		fileNames = FileNames[FileNameTake[pattern], DirectoryName[pattern]];
		components = Flatten[DeleteDuplicates[StringCases[fileNames,"file_"~~x:DigitCharacter..:>ToExpression[x]]]];
		datasets = Table[ReadCarpetHDF5[filePrefix<>".file_"<>ToString[c]<>".h5",
			CarpetHDF5DatasetName[var, it, map, rl, c], opts], {c,components}];
      , datasets={ReadCarpetHDF5[file,
			CarpetHDF5DatasetName[var, it, map, rl, None], opts]};
      ]
    ];

	Return[datasets]
];

ReadCarpetHDF5Variable[file_, var_, it_, rl_, map_:None, opts___]:=
  MergeDataRegions[ReadCarpetHDF5Components[file, var, it, rl, map, opts]];

Options[ReadCarpetHDF5Variable] = {Iteration -> None, Variable -> None, RefinementLevel -> None, Map -> None};

ReadCarpetHDF5Variable[file_, opts:OptionsPattern[]]:=
  Module[{it, rl, var, map},
    var = If[OptionValue[Variable] =!= None, OptionValue[Variable], First[CarpetHDF5Variables[file]]];
    it = If[OptionValue[Iteration] =!= None, OptionValue[Iteration], First[CarpetHDF5Iterations[file]]];
    rl = If[OptionValue[RefinementLevel] =!= None, OptionValue[RefinementLevel], First[CarpetHDF5RefinementLevels[file]]];
    map = If[OptionValue[Map] =!= None, OptionValue[Map], First[CarpetHDF5Maps[file]]];
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
