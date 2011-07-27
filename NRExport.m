(* ::Package:: *)

(* Copyright (C) 2011 Ian Hinder and Barry Wardell *)

BeginPackage["NRExport`", {"BHCoordinates`", "DataTable`", "Horizons`", "NR`"}];


ExportExtrapolatedWaveform::usage = "ExportExtrapolatedWaveform[run, file, mass, l, m] extrapolates the (l,m) mode of the waveform in run assuming mass is the ADM mass. The extrapolated waveform is then exported to file. The output format depends on the file extension which can be either '.asc' or '.h5'.";
ExportAllExtrapolatedWaveforms::usage = "ExportAllExtrapolatedWaveforms[run, file, mass] extrapolats all modes of the waveform in run assuming mass is the ADM mass. The extrapolated waveforms are then exported to file. The output format depends on the file extension which can be either '.asc' or '.h5'. For ASCII data, multiple files are created, one for each mode.";
JunkTime::usage = "JunkTime is an option for ExportExtrapolatedWaveform and ExportAllExtrapolatedWaveforms which specifies how much to cut off from the start of a waveform to eliminate junk radiation.";

ExportExtractedWaveform::usage = "ExportExtractedWaveform[run, file, l, m, rad] exports the (l,m) mode of the waveform extracted at radius rad in run to file. The output format depends on the file extension which can be either '.asc' or '.h5'.";
ExportAllExtractedWaveforms::usage = "ExportAllExtractedWaveforms[run, file] extrapolats all modes of the waveform in run to file. The output format depends on the file extension which can be either '.asc' or '.h5'. For ASCII data, multiple files are created, one for each mode.";

ExportAllWaveforms::usage = "ExportAllWaveforms[run, file, mass] exports all extracted waveforms along with the extrapolated waveform for run to file. The output format depends on the file extension which can be either '.asc' or '.h5'. For ASCII data, multiple files are created, one for each mode.";

ExportTrajectories::usage = "ExportTrajectories[run, file] exports trajectory and spin information for run to file.";

ExportLocalQuantity;

ExportStatus::usage = "ExportStatus is a variable which reports the current status of an export."

ExportWaveform;
ExportBHCoords;
ExportBHRelativeCoords;
ExportRun::usage = "ExportRun[run, dir] exports run to dir";
ExportGridStructure;
FunctionOfPhase;
Coordinates;
Spin;

Begin["`Private`"];

(* We cut off the first part of the extrapolated waveform where the junk dominates *)
Options[ExportExtrapolatedWaveform] = {JunkTime -> None};

fileExtension[file_] :=
  Module[{ext},
  ext = FileExtension[file];
  If[ext == "gz",
    ext = FileExtension[FileBaseName[file]]<>".gz"];
  ext];

fileBaseName[file_] :=
  Module[{ext, base},
  ext = FileExtension[file];
  base = FileBaseName[file];
  If[ext == "gz",
    base = FileBaseName[FileBaseName[file]]];
  base];

ExportExtrapolatedWaveform[run_String, file_String, mass_, l_Integer, m_Integer, OptionsPattern[]] :=
 Module[{dir, extrap, junkTime, afterjunk, final, dataset},
  dir = DirectoryName[file];
  If[dir=!="" && FileType[dir]=!=Directory,
    CreateDirectory[dir];
  ];

  ExportStatus = "Exporting extrapolated waveform for"<>run<>"("<>ToString[l]<>", "<>ToString[m]<>") to "<>file;

  extrap    = ExtrapolatePsi4[run, l, m, AlignPhaseAt->200, MassADM->mass, ExtrapolationOrder->3];
  junkTime  = OptionValue[JunkTime];
  If[!SameQ[junkTime, None],
    afterjunk = ShiftDataTable[-junkTime, DataTableInterval[extrap, {junkTime, All}]];,
    afterjunk = extrap;
  ];
  final     = Join[Re[afterjunk], Im[afterjunk]];

  Switch[fileExtension[file],
  "h5",
    dataset="l"<>ToString[l]<>"_m"<>ToString[m]<>"_rinf";
    Export[file, final, {"Datasets", dataset}, "Append"->True];,
  "asc",
    Export[file, final, "TABLE"];,
  _,
    Throw["Unsupported file format: "<>fileExtension[file]];
  ];
];

ExportAllExtrapolatedWaveforms[run_String, file_String, mass_] :=
 Module[{dir, modes, files},
  dir = DirectoryName[file];
  modes = ReadPsi4Modes[run];

  Switch[fileExtension[file],
  "asc"|"asc.gz",
    files=(dir<>fileBaseName[file]<>"_l"<>ToString[#[[1]]]<>"_m"<>ToString[#[[2]]]<>"_rinf."<>fileExtension[file] &) /@ modes;
    MapThread[ExportExtrapolatedWaveform[run, #1, mass, Sequence@@#2]&, {files, modes}];,
  "h5",
    (ExportExtrapolatedWaveform[run, file, mass, Sequence@@#]&) /@ modes;,
  _,
    Throw["Unsupported file format: "<>fileExtension[file]];
  ];
];

ExportExtractedWaveform[run_String, file_String, l_Integer, m_Integer, rad_] :=
 Module[{dir, psi4, final, dataset, ext},
  dir = DirectoryName[file];
  If[dir=!="" && FileType[dir]=!=Directory,
    CreateDirectory[dir];
  ];

  ExportStatus = "Exporting extracted waveform for "<>run<>" ("<>ToString[l]<>", "<>ToString[m]<>", "<>ToString[rad]<>") to "<>file;

  psi4  = ReadPsi4[run, l, m, Round[rad]];
  final = Join[Re[psi4], Im[psi4]];

  Switch[fileExtension[file],
  "h5",
    dataset="l"<>ToString[l]<>"_m"<>ToString[m]<>"_r"<>ToString[rad];
    Export[file, final, {"Datasets", dataset}, "Append"->True];,
  "asc",
    Export[file, final, "TABLE"];,
  "asc.gz",
    Export[file, final, {"GZIP", "TABLE"}];,
  _,
    Throw["Unsupported file format: "<>fileExtension[file]];
  ];
];

ExportAllExtractedWaveforms[run_String, file_String] :=
 Module[{dir, radii, modes, allwaveforms, files},
  dir = DirectoryName[file];
  radii = ReadPsi4Radii[run];
  modes = ReadPsi4Modes[run];
  allwaveforms = Flatten[Outer[Join, modes, List/@radii, 1], 1];



  Switch[fileExtension[file],
  "asc"|"asc.gz",
    files = (dir<>fileBaseName[file]<>"_l"<>ToString[#1[[1]]]<>"_m"<>ToString[#1[[2]]]<>"_r"<>ToString[#1[[3]]]<>"."<>fileExtension[file]&) /@ allwaveforms;
   MapThread[ExportExtractedWaveform[run, #1, Sequence@@#2]&, {files, allwaveforms}];,
  "h5",
    ExportExtractedWaveform[run, file, Sequence@@#1]& /@ allwaveforms;,
  _,
    Throw["Unsupported file format: "<>fileExtension[file]];
  ];
];

ExportAllWaveforms[run_String, file_String, mass_] := Module[{},
  ExportAllExtractedWaveforms[run, file];
  ExportAllExtrapolatedWaveforms[run, file, mass];
];

ExportTrajectories[run_String, file_String] :=
 Module[{dir, punc0, punc1, p, spin0, spin1, combined},
  dir = DirectoryName[file];
  If[dir=!="" && FileType[dir]=!=Directory,
    CreateDirectory[dir];
  ];

  ExportStatus = "Exporting trajectory data for "<>run<>" to "<>file;

  {punc0, punc1} = ReadBHCoordinates[run, #]& /@ {0,1};
  {spin0, spin1} = ReadIsolatedHorizonSpin[run, #]& /@ {0,1};

  (* We don't know how to get the momenta - set them to 0 *)
  p = MakeDataTable[({#, {0,0,0}}&) /@ IndVar[punc0]];

  combined = Join[punc0, punc1, p, p, spin0, spin1];
  Switch[fileExtension[file],
  "asc",
    Export[file, combined, "TABLE"];,
  "asc.gz",
    Export[file, combined, {"GZIP", "TABLE"}];,
  "h5",
    Export[file, punc0, {"Datasets", "Trajectory0"}, "Append"->True];
    Export[file, punc1, {"Datasets", "Trajectory1"}, "Append"->True];
    Export[file, p, {"Datasets", "Momentum0"}, "Append"->True];
    Export[file, p, {"Datasets", "Momentum1"}, "Append"->True];
    Export[file, spin0, {"Datasets", "Spin0"}, "Append"->True];
    Export[file, spin1, {"Datasets", "Spin1"}, "Append"->True];,
  _,
    Throw["Unsupported file format: "<>fileExtension[file]];
  ];
];

ExportLocalQuantity[run_String, what_, i_, file_String] :=
 Module[{dir, punc0, punc1, p, spin0, spin1, combined, f},
  dir = DirectoryName[file];
  If[dir=!="" && FileType[dir]=!=Directory,
    CreateDirectory[dir];
  ];

  ExportStatus = "Exporting " <> ToString[what] <> " data for "<>run<>" to "<>file;

  f = Switch[what, Coordinates, ReadBHCoordinates[run, i-1], Spin, ReadIsolatedHorizonSpin[run, i-1]];

  Switch[fileExtension[file],
  "asc",
    Export[file, f, "TABLE"];,
  "asc.gz",
    Export[file, f, {"GZIP", "TABLE"}];,
  "h5",
  Throw["Unsupported"],
(*     Export[file, f0, {"Datasets", "Trajectory0"}, "Append"->True]; *)
(*     Export[file, f1, {"Datasets", "Trajectory1"}, "Append"->True]; *)
(*     Export[file, p, {"Datasets", "Momentum0"}, "Append"->True]; *)
(*     Export[file, p, {"Datasets", "Momentum1"}, "Append"->True]; *)
(*     Export[file, spin0, {"Datasets", "Spin0"}, "Append"->True]; *)
(*     Export[file, spin1, {"Datasets", "Spin1"}, "Append"->True];, *)
  _,
    Throw["Unsupported file format: "<>fileExtension[file]];
  ];
];




(******** DEPRECATED ********)
ExportWaveform[run_String, dir_String, l_Integer, m_Integer, 
  r_?NumberQ] :=
 Module[{},
  If[FileType[dir] =!= Directory, CreateDirectory[dir]];
  Monitor`status = {"Exporting waveform", run, l, m, r};
  Export[dir <> "/psi4_l" <> ToString[l] <> "_m" <> ToString[m] <> 
    "_r" <> ToString[r] <> ".asc", 
   Map[{#[[1]], Re[#[[2]]], Im[#[[2]]]} &, 
    ToList[ReadPsi4[run, l, m, r]]], "TSV"]]

ExportBHCoords[run_String, dir_String, tracker_Integer] :=
 Module[{},
  If[FileType[dir] =!= Directory, CreateDirectory[dir]];
  Export[dir <> "/bh_coords_cart_" <> ToString[tracker] <> ".asc", 
   Map[{#[[1]], #[[2, 1]], #[[2, 2]], #[[2, 3]]} &, 
    ToList[ReadBHCoordinates[run, tracker]]], "TSV"]];

ExportBHRelativeCoords[run_String, dir_String] :=
 Module[{},
  If[FileType[dir] =!= Directory, CreateDirectory[dir]];
  Export[dir <> "/bh_coords_polar.asc", 
   MapThread[{#[[1]], #1[[2]], #2[[2]]} &, {ToList[
      ReadBHSeparation[run]], ToList[ReadBHPhase[run]]}],
    "TSV"]];

ExportBHCoords[run_String, dir_String] :=
 Module[{},
  Table[ExportBHCoords[run, dir, t], {t, 0, 1}]];

selectInRange[xs_, range_] :=
 If[range === All,
  xs,
  Select[xs, (# >= range[[1]] && # <= range[[2]]) &]];

ExportWaveforms[run_String, dir_String, lRange_: All, rRange_: All] :=
  Module[{rs, ls},
  rs = selectInRange[ReadPsi4Radii[run], rRange];
  ls = selectInRange[ReadPsi4Modes[run], lRange];
  Table[ExportWaveform[run, dir, l, m, r], {l, ls}, {m, -l, l}, {r, 
    rs}]];

Options[ExportRun] = {LRange -> All, RadiusRange -> All};

ExportRun[run_String, dir_String, opts:OptionsPattern[]] :=
 Module[{lRange = OptionValue[LRange], rRange = OptionValue[RadiusRange]},
  ExportWaveforms[run, dir, lRange, rRange];
  ExportBHCoords[run, dir];
  ExportBHRelativeCoords[run, dir];
  ExportGridStructure[run, dir]];

ExportGridStructure[run_String, dir_String] :=
 Module[{},
  If[FileType[dir] =!= Directory, CreateDirectory[dir]];
  Export[dir <> "/grid_structure.asc", GridStructure[run], "TSV"]];


End[];

EndPackage[];
