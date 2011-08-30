(* ::Package:: *)

(* Copyright (C) 2011 Ian Hinder and Barry Wardell *)

BeginPackage["NRExport`", {"BHCoordinates`", "DataTable`", "Horizons`", "NR`", "Parameters`"}];

JunkTime::usage = "JunkTime is an option for ExportExtrapolatedWaveform, ExportAllExtrapolatedWaveforms and ExportMetadata which specifies how long the junk radiation lasts.";

ExportExtrapolatedStrain::usage = "ExportExtrapolatedStrain[run, file, mass, l, m, \!\(\*SubscriptBox[\(\[Omega]\), \(0\)]\)] extrapolates the (l,m) mode of the waveform in run assuming mass is the ADM mass and then computes the strain using the fixed-frequency integration method with cutoff frequency \!\(\*SubscriptBox[\(\[Omega]\), \(0\)]\). The extrapolated waveform is then exported to file. The output format depends on the file extension which can be either '.asc', '.asc.gz' or '.h5'.";
ExportAllExtrapolatedStrain::usage = "ExportAllExtrapolatedStrain[run, file, mass, \!\(\*SubscriptBox[\(\[Omega]\), \(0\)]\)] extrapolates all modes of the waveform in run assuming mass is the ADM mass and then computes the strain using the fixed-frequency integration method with cutoff frequency \!\(\*SubscriptBox[\(\[Omega]\), \(0\)]\). The extrapolated waveforms are then exported to file. The output format depends on the file extension which can be either '.asc', '.asc.gz' or '.h5'. For ASCII data, multiple files are created, one for each mode.";
ExportExtrapolatedWaveform::usage = "ExportExtrapolatedWaveform[run, file, mass, l, m] extrapolates the (l,m) mode of the waveform in run assuming mass is the ADM mass. The extrapolated waveform is then exported to file. The output format depends on the file extension which can be either '.asc', '.asc.gz' or '.h5'.";
ExportAllExtrapolatedWaveforms::usage = "ExportAllExtrapolatedWaveforms[run, file, mass] extrapolates all modes of the waveform in run assuming mass is the ADM mass. The extrapolated waveforms are then exported to file. The output format depends on the file extension which can be either '.asc', '.asc.gz' or '.h5'. For ASCII data, multiple files are created, one for each mode.";
ExportExtractedWaveform::usage = "ExportExtractedWaveform[run, file, l, m, rad] exports the (l,m) mode of the waveform extracted at radius rad in run to file. The output format depends on the file extension which can be either '.asc', '.asc.gz' or '.h5'.";
ExportAllExtractedWaveforms::usage = "ExportAllExtractedWaveforms[run, file] exports all modes of the waveform in run to file. The output format depends on the file extension which can be either '.asc', '.asc.gz' or '.h5'. For ASCII data, multiple files are created, one for each mode.";
ExportAllWaveforms::usage = "ExportAllWaveforms[run, file, mass] exports all extracted waveforms along with the extrapolated waveform for run to file. The output format depends on the file extension which can be either '.asc', '.asc.gz' or '.h5'. For ASCII data, multiple files are created, one for each mode.";

ExportLocalQuantity::usage = "ExportLocalQuantity[run, quantity, bh, file] exports a local quantity for black hole bh to file. Possible choices of quantity are Coordinates or Spin.";
Coordinates;
Spin;

ExportMetadata::usage = "ExportMetadata[file, run, mass, ecc] exports the metadata for run to file.";
ExportSim::usage = "ExportSim[run, niceName, mass, ecc, outputDirectory] exports a full simulation, including waveforms, local quantities and metadata.";
ExportConfig::usage = "ExportConfig[name -> {mass, sims, ecc}, outputDirectory] exports a collection of simulations (at different resolutions, for example) all corresponding to the same physical configuration.";
ExportSimFormat::usage = "ExportSimFormat is an option for ExportSim which specifies the format to use. Possible choices are \"ASCII\" and \"HDF5\".";
ExportStatus::usage = "ExportStatus is a variable which reports the current status of an export.";

Begin["`Private`"];

ExportStatus = "";

(* Waveforms *)

(* We cut off the first part of the extrapolated waveform where the junk dominates *)
Options[ExportExtrapolatedWaveform] = {JunkTime -> None};
Options[ExportExtrapolatedStrain] = {JunkTime -> None};

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

  ExportStatus = "Exporting extrapolated waveform for "<>run<>"("<>ToString[l]<>", "<>ToString[m]<>") to "<>file;

  extrap    = ExtrapolatePsi4[run, l, m, AlignPhaseAt->200, MassADM->mass, ExtrapolationOrder->3];
  junkTime  = OptionValue[JunkTime];
  If[!SameQ[junkTime, None],
    afterjunk = DataTableInterval[extrap, {junkTime + First[DataTableRange[extrap]], All}];,
    afterjunk = extrap;
  ];
  final     = Join[Re[afterjunk], Im[afterjunk]];

  Switch[fileExtension[file],
  "h5",
    dataset="l"<>ToString[l]<>"_m"<>ToString[m]<>"_rinf";
    Export[file, final, {"Datasets", dataset}, "Append"->True];,
  "asc",
    Export[file, final, "TABLE"];,
  "asc.gz",
    Export[file, final, {"GZIP", "TABLE"}];,
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

ExportExtrapolatedStrain[run_String, file_String, mass_, l_Integer, m_Integer, om_, OptionsPattern[]] :=
 Module[{dir, extrap, strain, junkTime, afterjunk, final, dataset},
  dir = DirectoryName[file];
  If[dir=!="" && FileType[dir]=!=Directory,
    CreateDirectory[dir];
  ];

  ExportStatus = "Exporting extrapolated strain waveform for "<>run<>"("<>ToString[l]<>", "<>ToString[m]<>") to "<>file;

  extrap    = ExtrapolatePsi4[run, l, m, AlignPhaseAt->200, MassADM->mass, ExtrapolationOrder->3];
  strain    = StrainFromPsi4[extrap, om];
  junkTime  = OptionValue[JunkTime];
  If[!SameQ[junkTime, None],
    afterjunk = DataTableInterval[strain, {junkTime + First[DataTableRange[strain]], All}];,
    afterjunk = strain;
  ];
  final     = Join[Re[afterjunk], Im[afterjunk]];

  Switch[fileExtension[file],
  "h5",
    dataset="l"<>ToString[l]<>"_m"<>ToString[m]<>"_rinf";
    Export[file, final, {"Datasets", dataset}, "Append"->True];,
  "asc",
    Export[file, final, "TABLE"];,
  "asc.gz",
    Export[file, final, {"GZIP", "TABLE"}];,
  _,
    Throw["Unsupported file format: "<>fileExtension[file]];
  ];
];

ExportAllExtrapolatedStrain[run_String, file_String, mass_, om_] :=
 Module[{dir, modes, files},
  dir = DirectoryName[file];
  modes = ReadPsi4Modes[run];

  Switch[fileExtension[file],
  "asc"|"asc.gz",
    files=(dir<>fileBaseName[file]<>"_l"<>ToString[#[[1]]]<>"_m"<>ToString[#[[2]]]<>"_rinf."<>fileExtension[file] &) /@ modes;
    MapThread[ExportExtrapolatedStrain[run, #1, mass, Sequence@@#2, om #2[[2]]/2]&, {files, modes}];,
  "h5",
    (ExportExtrapolatedStrain[run, file, mass, Sequence@@#, om #[[2]]/2]&) /@ modes;,
  _,
    Throw["Unsupported file format: "<>fileExtension[file]];
  ];
];

ExportExtractedWaveform[run_String, file_String, l_Integer, m_Integer, r_] :=
 Module[{dir, psi4, final, dataset, ext, rad = ToString[r]},
  dir = DirectoryName[file];
  If[dir=!="" && FileType[dir]=!=Directory,
    CreateDirectory[dir];
  ];

  ExportStatus = "Exporting extracted waveform for "<>run<>" ("<>ToString[l]<>", "<>ToString[m]<>", "<>rad<>") to "<>file;

  psi4  = ReadPsi4[run, l, m, Round[ToExpression[rad]]];
  final = Join[Re[psi4], Im[psi4]];

  Switch[fileExtension[file],
  "h5",
    dataset="l"<>ToString[l]<>"_m"<>ToString[m]<>"_r"<>rad;
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
  radii = ReadPsi4RadiiStrings[run];
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


(* Local quantities *)
ExportLocalQuantity[run_String, what_, i_, file_String] :=
 Module[{dir, punc0, punc1, p, spin0, spin1, combined, f, dsName},
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
    dsName = (what /. {Coordinates -> "trajectory", Spin -> "spin"}) <> ToString[i-1];
    Export[file, f, {"Datasets", dsName}, "Append"->True];,
  _,
    Throw["Unsupported file format: "<>fileExtension[file]];
  ];
];


(* Run metadata *)

coord[d_] :=
 {"x", "y", "z"}[[d]];

runMetadata[run_, mass_, ecc_, tJunk_] :=
 Module[{M = TotalMass[run], code, evolution, eta, bibtex},
  evolution = LookupParameter[run, "ADMBase::evolution_method"];
  Which[
    StringMatchQ[evolution, "ctgamma", IgnoreCase -> True],
    code = "CTGamma";
    eta  = LookupParameter[run, "CTGGauge::eta"];
    bibtex = "Pollney:2009yz";,
    StringMatchQ[evolution, RegularExpression["ML_BSSN.*"], IgnoreCase -> True],
    code = "McLachlan";
    eta  = LookupParameter[run, evolution<>"::BetaDriver"];
    bibtex = "Brown:2008sb";,
    True,
    Throw["Unknow evolution code used"];
  ];

  {(* "comments" -> "", *)
   (* "documentation" -> "", *)
   (* "publication" -> "", *)
   "submitter-email" -> "ian.hinder@aei.mpg.de",
   "code" -> "Llama/" <> code,
   (* "code-version"->"", *)
   "code-bibtex-keys" -> bibtex,
   "evolution-system" -> "BSSN",
   "evolution-gauge" ->
    "1+log/Gamma-driver(eta=" <> eta <> ")",
   "resolution" -> Round[0.6/(ReadCoarseGridSpacing[run]/2^5)],
   "resolution-expected-order" -> 8,
   "extraction-radius" -> "finite-radii" (* <>" extrapolated" *),
   (* "extrapolation-techniques" -> "3rd,tortoise", *)
   "ht-generation-technique" -> "ReisswigPollney",
   "initial-ADM-energy" -> mass/M,
   "initial-ADM-angular-momentum" ->
    Norm@InitialAngularMomentum[run]/M^2,
   "initial-separation" -> InitialSeparation[run]/M,
   "initial-data-type" -> "Bowen-York quasicircular",
   "initial-data-bibtex-keys" -> "Bowen:1980yu Brandt:1997tf Ansorg:2004ds",
   "quasicircular-bibtex-keys" -> "Husa:2007rh",
   "eccentricity" -> ecc,
   "eccentricity-measurement-method" -> "newtonian-fit-in-om-gw",
   "initial-freq-22" -> -Interpolation[
       Frequency@ReadPsi4[run, 2, 2, 100]][100 + tJunk],
   "number-of-cycles-22" -> NumCycles[run, 100 + tJunk],
   (* "phase-error" -> "", *)
   (* "amplitude-error-relative" -> "", *)
   "after-junkradiation-time" -> tJunk,
   Sequence @@ Table["mass" <> ToString[i] ->
      Interpolation[ChristodoulouMass[run, i, i-1]][tJunk]/M, {i, 1, 2}],

   Sequence @@ Flatten@Table[
      "initial-bh-position" <> ToString[i] <> coord[d] ->
       InitialPosition[run, i][[d]]/M, {i, 0, 1}, {d, 1, 3}],

   Sequence @@ Flatten@Table[
      "initial-bh-momentum" <> ToString[i] <> coord[d] ->
       InitialLinearMomentum[run, i][[d]]/M, {i, 0, 1}, {d, 1, 3}],
   Sequence @@ Flatten@Table[
      "after-junkradiation-spin" <> ToString[i] <> coord[d] ->
       Interpolation[ReadIsolatedHorizonSpin[run, i, d]][tJunk]/
        M^2, {i, 0, 1}, {d, 1, 3}]
   }
  ];

runAllData[run_String, mass_, ecc_, tJunk_] := Module[{modes, radii},
 modes = ReadPsi4Modes[run];
 radii = ReadPsi4RadiiStrings[run];
 waveform[{l_, m_}, rad_] := ToString[l]<>","<>ToString[m] -> "mp_psi4_l"<>ToString[l]<>"_m"<>ToString[m]<>"_r"<>ToString[rad]<>".asc.gz";
 {"metadata" -> runMetadata[run, mass, ecc, tJunk],
  "body-data" ->
   {Sequence @@ Table["spin" <> ToString[i] ->
      "spin"<>ToString[i]<>".asc.gz", {i, 1, 2}],
   Sequence @@ Table["trajectory" <> ToString[i] ->
      "trajectory"<>ToString[i]<>".asc.gz", {i, 1, 2}],
   Sequence @@ Table["horizon-mass" <> ToString[i] ->
      "horizon_mass"<>ToString[i]<>".asc.gz", {i, 1, 2}]},
   Sequence @@ Table["Psi4t-data" -> Prepend[Map[waveform[#, rad] &, modes], "extraction-radius" -> rad], {rad, radii}]}
];

makeMetadataFile[md_List] :=
 Flatten@Riffle[Map[makeSection, md], ""];

stringPad[s_, n_] :=
 s <> StringJoin@ConstantArray[" ", n - StringLength[s]];

makeSection[sec_ -> entries_] :=
  Module[{pad},
    pad = Max[Map[StringLength[First[#]] &, entries]];
    Prepend[Map[makeEntry[#,pad] &, entries], "[" <> sec <> "]"]];

makeEntry[key_ -> val_, pad_:0] :=
 stringPad[key,pad] <> " = " <> makeEntry[val];

makeEntry[val_] :=
 If[StringQ[val], val, ToString[val, CForm]];

Options[ExportMetadata] = {JunkTime -> None};
ExportMetadata[file_, run_, mass_, ecc_, OptionsPattern[]] :=
 Module[{tJunk},
  tJunk  = OptionValue[JunkTime];
  If[SameQ[tJunk, None], tJunk = 0;];

  Export[file, makeMetadataFile[runAllData[run, mass, ecc, tJunk]], "Text"];
];


(* Full run *)

ExportConfig[name_ -> {Madm_, sims_, ecc_},  outputDirectory_] :=
  Scan[ExportSim[#, name, Madm, ecc, outputDirectory] &, sims];

Options[ExportSim] = {ExportSimFormat -> "ASCII"};
ExportSim[run_String, niceName_, outputDirectory_, mass_, ecc_, OptionsPattern[]] :=
  Module[{dir, h, n, format, ext},

    h = ReadCoarseGridSpacing[run];
    n = Round[0.6/(h/2^5)];
    dir = FileNameJoin[{outputDirectory, niceName, niceName<>"_"<>ToString[n]}];

    Print[run <> " -> " <> dir];
    If[FileType[dir] === None, CreateDirectory[dir]];

    If[OptionValue[ExportSimFormat]==="ASCII", ext = ".asc.gz", ext = ".h5"];

    ExportAllExtractedWaveforms[run, dir <> "/psi4"<>ext];
    ExportAllExtrapolatedWaveforms[run, dir <> "/psi4"<>ext, mass];
    ExportLocalQuantity[run, Coordinates, 1, dir <> "/traj1"<>ext];
    ExportLocalQuantity[run, Coordinates, 2, dir <> "/traj2"<>ext];
    ExportLocalQuantity[run, Spin, 1, dir <> "/spin1"<>ext];
    ExportLocalQuantity[run, Spin, 2, dir <> "/spin2"<>ext];
    ExportMetadata[dir<>"/"<>niceName<>"_"<>ToString[n]<>".bbh", run, mass, ecc];
  ];

End[];

EndPackage[];
