(* ::Package:: *)

(* Copyright (C) 2011 Ian Hinder and Barry Wardell *)

BeginPackage["NRExport`", {"BHCoordinates`", "DataTable`", "Grids`", "Horizons`", "NR`", "Parameters`"}];

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
HorizonMass;

ExportMetadata::usage = "ExportMetadata[file, run, mass, ecc] exports the metadata for run to file.";
ExportSim::usage = "ExportSim[run, niceName, outputDirectory, mass, ecc] exports a full simulation, including waveforms, local quantities and metadata.";
ExportConfig::usage = "ExportConfig[name -> {mass, sims, ecc}, outputDirectory] exports a collection of simulations (at different resolutions, for example) all corresponding to the same physical configuration.";
ExportSimFormat::usage = "ExportSimFormat is an option for ExportSim which specifies the format to use. Possible choices are \"ASCII\" and \"HDF5\".";
ExportStatus::usage = "ExportStatus is a variable which reports the current status of an export.";
ExportOnly;
ExcludeModes::usage = "ExcludeModes is an option for ExportSim.";

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

ExportAllExtrapolatedWaveforms[run_String, file_String, mass_, excludeModes_:None] :=
 Module[{dir, modes, files, excludeRun, excludePattern},
  dir = DirectoryName[file];
  modes = ReadPsi4Modes[run];
  {excludeRun, excludePattern} = excludeModes;
  If[run === excludeRun,
     modes = Select[modes, (!MatchQ[#,excludePattern]) &]];

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
 Module[{dir, psi4, final, dataset, rad = ToString[r]},
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
 Module[{dir, f, dsName},
  dir = DirectoryName[file];
  If[dir=!="" && FileType[dir]=!=Directory,
    CreateDirectory[dir];
  ];

  ExportStatus = "Exporting " <> ToString[what] <> " data for "<>run<>" to "<>file;

  f = Switch[what,
    Coordinates, ReadBHCoordinates[run, i-1],
    Spin, ReadIsolatedHorizonSpin[run, i-1],
    HorizonMass, ReadAHMass[run,i]];

  Switch[fileExtension[file],
  "asc",
    Export[file, f, "TABLE"];,
  "asc.gz",
    Export[file, f, {"GZIP", "TABLE"}];,
  "h5",
    dsName = (what /. {Coordinates -> "traj", Spin -> "spin", HorizonMass -> "horizon_mass"}) <> ToString[i];
    Export[file, f, {"Datasets", dsName}, "Append"->True];,
  _,
    Throw["Unsupported file format: "<>fileExtension[file]];
  ];
];


(* Run metadata *)

coord[d_] :=
 {"x", "y", "z"}[[d]];

runMetadata[run_, mass_, ecc_, tJunk_] :=
 Module[{code, evolution, eta, bibtex},
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
   (* "simulation-bibtex-keys" -> "", *)
   "submitter-email" -> "ian.hinder@aei.mpg.de",
   "authors-emails" -> "Daniela Alic <daniela.alic@aei.mpg.de>, Ian Hinder <ian.hinder@aei.mpg.de>, Philipp Moesta <philipp.moesta@aei.mpg.de>, Luciano Rezzolla <luciano.rezzolla@aei.mpg.de>, Barry Wardell <barry.wardell@aei.mpg.de>",
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
   "initial-ADM-energy" -> mass,
   "initial-ADM-angular-momentum" ->
    Norm@InitialAngularMomentum[run],
   "initial-separation" -> InitialSeparation[run],
   "initial-data-type" -> "Bowen-York quasicircular",
   "initial-data-bibtex-keys" -> "Bowen:1980yu Brandt:1997tf Ansorg:2004ds",
   "quasicircular-bibtex-keys" -> "Husa:2007rh",
   "eccentricity" -> ecc,
   "eccentricity-measurement-method" -> "newtonian-fit-in-om-gw",
   "freq-start-22" -> -Interpolation[
       Frequency@ReadPsi4[run, 2, 2, 100]][100 + tJunk],
   "number-of-cycles-22" -> NumCycles[run, 100 + tJunk],
   (* "phase-error" -> "", *)
   (* "amplitude-error-relative" -> "", *)
   "after-junkradiation-time" -> tJunk,
   Sequence @@ Table["mass" <> ToString[i] ->
      Interpolation[ChristodoulouMass[run, i, i-1]][tJunk], {i, 1, 2}],

   Sequence @@ Flatten@Table[
      "initial-bh-position" <> ToString[i+1] <> coord[d] ->
       InitialPosition[run, i][[d]], {i, 0, 1}, {d, 1, 3}],

   Sequence @@ Flatten@Table[
      "initial-bh-momentum" <> ToString[i+1] <> coord[d] ->
       InitialLinearMomentum[run, i][[d]], {i, 0, 1}, {d, 1, 3}],
   Sequence @@ Flatten@Table[
      "after-junkradiation-spin" <> ToString[i+1] <> coord[d] ->
       Interpolation[ReadIsolatedHorizonSpin[run, i, d]][tJunk],
         {i, 0, 1}, {d, 1, 3}],
   Sequence @@ Flatten@Table[
      "initial-bh-spin" <> ToString[i+1] <> coord[d] ->
       DepVar[ReadIsolatedHorizonSpin[run, i, d]][[2]], (* Poison in the first entry of q1D11.8S0.3s0b *)
        {i, 0, 1}, {d, 1, 3}],
   "data-type" -> "NR"
   }
  ];

runAllData[run_String, mass_, ecc_, tJunk_, format_String] :=  Module[{modes, radii, ext},
 ext = Switch[format, "HDF5", "h5", "ASCII", "asc.gz", _, Throw["Unrecognised format: "<>ToString@format]];
 modes = ReadPsi4Modes[run];
 radii = ReadPsi4RadiiStrings[run];
 waveform[{l_, m_}, rad_, "ASCII"] := ToString[l]<>","<>ToString[m] -> "psi4_l"<>ToString[l]<>"_m"<>ToString[m]<>"_r"<>ToString[rad]<>".asc.gz";
 waveform[{l_, m_}, rad_, "HDF5"] := ToString[l]<>","<>ToString[m] -> "psi4.h5:l"<>ToString[l]<>"_m"<>ToString[m]<>"_r"<>ToString[rad];

 spinFileName[i_, "ASCII"] := "spin"<>ToString[i]<>"."<>ext;
 spinFileName[i_, "HDF5"] := "spin"<>ToString[i]<>"."<>ext<>":spin"<>ToString[i];

 trajFileName[i_, "ASCII"] := "traj"<>ToString[i]<>"."<>ext;
 trajFileName[i_, "HDF5"] := "traj"<>ToString[i]<>"."<>ext<>":traj"<>ToString[i];

 horMassFileName[i_, "ASCII"] := "horizon_mass"<>ToString[i]<>"."<>ext;
 horMassFileName[i_, "HDF5"] := "horizon_mass"<>ToString[i]<>"."<>ext<>":horizon_mass"<>ToString[i];

 {"metadata" -> runMetadata[run, mass, ecc, tJunk],
  "body-data" ->
   {Sequence @@ Table["spin" <> ToString[i] ->
      spinFileName[i,format], {i, 1, 2}],
   Sequence @@ Table["trajectory" <> ToString[i] ->
      trajFileName[i,format], {i, 1, 2}],
   Sequence @@ Table["horizon-mass" <> ToString[i] ->
      horMassFileName[i,format], {i, 1, 2}]},
   Sequence @@ Table["Psi4t-data" -> Prepend[Map[waveform[#, rad, format] &, modes], "extraction-radius" -> rad], {rad, radii}]}
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

Options[ExportMetadata] = {JunkTime -> None, ExportSimFormat -> "ASCII"};
ExportMetadata[file_, run_, mass_, ecc_, OptionsPattern[]] :=
 Module[{tJunk},
  tJunk  = OptionValue[JunkTime];
  If[SameQ[tJunk, None], tJunk = 0;];

  Export[file, makeMetadataFile[runAllData[run, mass, ecc, tJunk, OptionValue[ExportSimFormat]]], "Text"];
];


(* Full run *)

Options[ExportConfig] = Options[ExportSim];
ExportConfig[name_ -> {Madm_, sims_, ecc_},  outputDirectory_, opts:OptionsPattern[]] :=
  Scan[ExportSim[#, name, outputDirectory, Madm, ecc, opts] &, sims];

Options[ExportSim] = {ExportSimFormat -> "ASCII", ExportOnly -> All, ExcludeModes -> None};
ExportSim[run_String, niceName_, outputDirectory_, mass_, ecc_, OptionsPattern[]] :=
  Module[{dir, h, n, ext, all, export},

    h = ReadCoarseGridSpacing[run];
    n = Round[0.6/(h/2^5)];
    dir = FileNameJoin[{outputDirectory, niceName, niceName<>"_"<>ToString[n]}];

    Print[run <> " -> " <> dir];
    If[FileType[dir] === None, CreateDirectory[dir]];

    If[OptionValue[ExportSimFormat]==="ASCII", ext = ".asc.gz", ext = ".h5"];

    all = {"ExtrapolatedWaves", "Metadata", "Coordinates", "Spin", "HorizonMass", "FiniteRadiiWaves"};
    export = If[OptionValue[ExportOnly] === All, all, OptionValue[ExportOnly]];

    Do[
    Switch[item,
      "FiniteRadiiWaves",  ExportAllExtractedWaveforms[run, dir <> "/psi4"<>ext],
      "ExtrapolatedWaves", ExportAllExtrapolatedWaveforms[run, dir <> "/psi4"<>ext, mass,
                                                          OptionValue[ExcludeModes]],
      "Coordinates",       ExportLocalQuantity[run, Coordinates, 1, dir <> "/traj1"<>ext];
                           ExportLocalQuantity[run, Coordinates, 2, dir <> "/traj2"<>ext],
      "Spin",              ExportLocalQuantity[run, Spin, 1, dir <> "/spin1"<>ext];
                           ExportLocalQuantity[run, Spin, 2, dir <> "/spin2"<>ext],
      "HorizonMass",       ExportLocalQuantity[run, HorizonMass, 1, dir <> "/horizon_mass1"<>ext];
                           ExportLocalQuantity[run, HorizonMass, 2, dir <> "/horizon_mass2"<>ext],
      "Metadata",          ExportMetadata[dir<>"/"<>niceName<>"_"<>ToString[n]<>".bbh", run, mass,
                                          ecc],
      _, Throw["Error"]], {item, export}];
  ];

End[];

EndPackage[];
