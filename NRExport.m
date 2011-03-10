(* ::Package:: *)

(* Copyright (C) 2011 Ian Hinder and Barry Wardell *)

BeginPackage["NRExport`", {"BHCoordinates`", "DataTable`", "Horizons`", "NR`"}];


ExportExtrapolatedWaveform::usage = "ExportExtrapolatedWaveform[run, dir, mass, l, m] extrapolates the (l,m) mode of the waveform in run assuming mass is the ADM mass. The extrapolated waveform is then exported to a file in dir.";
ExportAllExtrapolatedWaveform::usage = "ExportAllExtrapolatedWaveforms[run, dir, mass] extrapolats all modes of the waveform in run assuming mass is the ADM mass. The extrapolated waveforms are then exported to a file in dir.";
JunkTime::usage "JunkTime is an option for ExportExtrapolatedWaveform and ExportAllExtrapolatedWaveforms which specifies how much to cut off from the start of a waveform to eliminate junk radiation.";

ExportExtractedWaveform::usage = "ExportExtractedWaveform[run, dir, l, m] exports the (l,m) mode of the waveform in run to a file in dir.";
ExportAllExtractedWaveform::usage = "ExportAllExtractedWaveforms[run, dir] extrapolats all modes of the waveform in run to a file in dir.";

ExportAllWaveforms::usage = "ExportAllWaveforms[run, dir] exports all extracted waveforms along with the extrapolated waveform for run to dir."

ExportTrajectories::usage = "ExportTrajectories[run, dir] exports trajectory and spin information for run to a file in dir.";

ExportStatus::usage = "ExportStatus is a variable which reports the current status of an export."

ExportWaveform;
ExportBHCoords;
ExportBHRelativeCoords;
ExportRun::usage = "ExportRun[run, dir] exports run to dir";
ExportGridStructure;
FunctionOfPhase;


Begin["`Private`"];


(* We cut off the first part of the extrapolated waveform where the junk dominates *)
Options[ExportExtrapolatedWaveform] = {JunkTime -> 50};

ExportExtrapolatedWaveform[run_String, dir_String, mass_, l_Integer, m_Integer, OptionsPattern[]] :=
 Module[{extrap, junkTime, afterjunk, final, file, dataset},
  If[FileType[dir] =!= Directory, CreateDirectory[dir]];
  ExportStatus = "Exporting extrapolated waveform for "<>run<>" ("<>ToString[l]<>", "<>ToString[m]<>") to "<>dir;

  (* Extrapolate in r *)
  extrap = ExtrapolatePsi4[run, l, m, AlignPhaseAt->200, MassADM->mass, ExtrapolationOrder->3];

  (* Drop the first part of the waveform where the junk is *)
  junkTime = OptionValue[JunkTime];
  afterjunk = ShiftDataTable[-junkTime, DataTableInterval[extrap, {junkTime, All}]];

  final = Join[Re[afterjunk], Im[afterjunk]];

  file = dir<>"/mp_psi4.h5";
  dataset =  "l"<>ToString[l]<>"_m"<>ToString[m]<>"_rinf";
  Export[file, final, {"Datasets", dataset}, "Append" -> True];
];

ExportAllExtrapolatedWaveforms[run_String, dir_String, mass_] :=
 Module[{modes},
  modes = ReadPsi4Modes[run];
  Map[ExportExtrapolatedWaveform[run, dir, mass, Sequence@@#]&, modes];
];

ExportExtractedWaveform[run_String, dir_String, l_Integer, m_Integer, rad_] :=
 Module[{psi4, final, file, dataset},
  If[FileType[dir] =!= Directory, CreateDirectory[dir]];
  ExportStatus = "Exporting extracted waveform for "<>run<>" ("<>ToString[l]<>", "<>ToString[m]<>", "<>ToString[rad]<>") to "<>dir;

  psi4    = ReadPsi4[run, l, m, Round[rad]];
  final   = Join[Re[psi4], Im[psi4]];
  file    = dir<>"/mp_psi4.h5";
  dataset =  "l"<>ToString[l]<>"_m"<>ToString[m]<>"_r"<>ToString[rad];
  Export[file, final, {"Datasets", dataset}, "Append" -> True];
];

ExportAllExtractedWaveforms[run_String, dir_String] :=
 Module[{radii, modes, allwaveforms},
  radii = ReadPsi4Radii[run];
  modes = ReadPsi4Modes[run];

  (* Produce a list of waveforms available of the form (l, m, rad) *)
  allwaveforms = Flatten[Outer[Join, modes, List /@ radii, 1], 1];

  Map[ExportExtractedWaveform[run, dir, Sequence@@#]&, allwaveforms];
];

ExportAllWaveforms[run_String, dir_String, mass_] := Module[{},
  ExportAllExtractedWaveforms[run, dir];
  ExportAllExtrapolatedWaveforms[run, dir, mass];
]

ExportTrajectories[run_String, dir_String] :=
 Module[{punc0, punc1, p, spin0, spin1, combined, file},
  If[FileType[dir] =!= Directory, CreateDirectory[dir]];
  ExportStatus = "Exporting trajectory data for "<>run<>" to "<>dir;

  {punc0, punc1} = ReadBHCoordinates[run, #]& /@ {0, 1};
  spin0 = ReadIsolatedHorizonSpin[run, 0];
  spin1 = ReadIsolatedHorizonSpin[run, 1];

  (* We don't know how to get the momenta - set them to 0 *)
  p = MakeDataTable[{#, {0,0,0,0,0,0}}& /@ IndVar[punc0]];

  combined = Join[punc0, punc1, p, spin0, spin1];
  file = dir<>"/traj.h5";
  Export[file, combined, {"Datasets", "traj"}];
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
