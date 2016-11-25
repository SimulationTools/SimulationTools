(* Copyright 2010-2012 Ian Hinder and Barry Wardell

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

BeginPackage["SimulationTools`NRExport`",
 {
  "SimulationTools`BHCoordinates`",
  "SimulationTools`BlackHole`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`Grids`",
  "SimulationTools`Horizons`",
  "SimulationTools`Parameters`",
  "SimulationTools`InitialData`",
  "SimulationTools`ReadHDF5`",
  "SimulationTools`TwoPunctures`",
  "SimulationTools`Utils`",
  "SimulationTools`WaveformExtrapolation`",
  "SimulationTools`Waveforms`"
 }];

ExportExtrapolatedStrain(*::usage = "ExportExtrapolatedStrain[sim, file, mass, l, m, \!\(\*SubscriptBox[\(\[Omega]\), \(0\)]\)] extrapolates the (l,m) mode of the waveform in sim assuming mass is the ADM mass and then computes the strain using the fixed-frequency integration method with cutoff frequency \!\(\*SubscriptBox[\(\[Omega]\), \(0\)]\). The extrapolated waveform is then exported to file. The output format depends on the file extension which can be either '.asc', '.asc.gz' or '.h5'."*);
ExportAllExtrapolatedStrain(*::usage = "ExportAllExtrapolatedStrain[sim, file, mass, \!\(\*SubscriptBox[\(\[Omega]\), \(0\)]\)] extrapolates all modes of the waveform in sim assuming mass is the ADM mass and then computes the strain using the fixed-frequency integration method with cutoff frequency \!\(\*SubscriptBox[\(\[Omega]\), \(0\)]\). The extrapolated waveforms are then exported to file. The output format depends on the file extension which can be either '.asc', '.asc.gz' or '.h5'. For ASCII data, multiple files are created, one for each mode."*);
ExportExtrapolatedWaveform(*::usage = "ExportExtrapolatedWaveform[sim, file, mass, l, m] extrapolates the (l,m) mode of the waveform in sim assuming mass is the ADM mass. The extrapolated waveform is then exported to file. The output format depends on the file extension which can be either '.asc', '.asc.gz' or '.h5'."*);
ExportAllExtrapolatedWaveforms(*::usage = "ExportAllExtrapolatedWaveforms[sim, file, mass] extrapolates all modes of the waveform in sim assuming mass is the ADM mass. The extrapolated waveforms are then exported to file. The output format depends on the file extension which can be either '.asc', '.asc.gz' or '.h5'. For ASCII data, multiple files are created, one for each mode."*);
ExportExtractedWaveform(*::usage = "ExportExtractedWaveform[sim, file, l, m, rad] exports the (l,m) mode of the waveform extracted at radius rad in sim to file. The output format depends on the file extension which can be either '.asc', '.asc.gz' or '.h5'."*);
ExportAllExtractedWaveforms(*::usage = "ExportAllExtractedWaveforms[sim, file] exports all modes of the waveform in sim to file. The output format depends on the file extension which can be either '.asc', '.asc.gz' or '.h5'. For ASCII data, multiple files are created, one for each mode."*);
ExportAllWaveforms(*::usage = "ExportAllWaveforms[sim, file, mass] exports all extracted waveforms along with the extrapolated waveform for sim to file. The output format depends on the file extension which can be either '.asc', '.asc.gz' or '.h5'. For ASCII data, multiple files are created, one for each mode."*);

ExportLocalQuantity(*::usage = "ExportLocalQuantity[sim, quantity, bh, file] exports a local quantity for black hole bh to file. Possible choices of quantity are Coordinates or Spin."*);
Coordinates;
Spin;
HorizonMass;

ExportMetadata(*::usage = "ExportMetadata[file, sim, mass, ecc] exports the metadata for sim to file."*);
ExportNumericalRelativitySimulation::usage = "ExportNumericalRelativitySimulation[sim, alias, directory, mass, eccentricity] exports a numerical relativity simulation in the Numerical Relativity Data Format (NRDF) including waveforms, local quantities (black hole masses, spins and coordinates) and metadata.  mass is the ADM mass of the system.";
ExportConfig(*::usage = "ExportConfig[name -> {mass, sims, ecc}, outputDirectory] exports a collection of simulations (at different resolutions, for example) all corresponding to the same physical configuration."*);
ExportStatus(*::usage = "ExportStatus is a variable which reports the current status of an export."*);

ExportSim = ExportNumericalRelativitySimulation;
ExportSXSSimulation;
ExportSXSSimulationResolutions;
BinaryBlackHoleRelaxedTime;
HDF5FilesDiffer;

Begin["`Private`"];

ExportStatus = "";

(* Waveforms *)

(* We cut off the first part of the extrapolated waveform where the junk dominates *)
Options[ExportExtrapolatedWaveform] = {"JunkTime" -> None};
Options[ExportExtrapolatedStrain] = {"JunkTime" -> None};

(* JunkTime::usage = "JunkTime is an option for ExportExtrapolatedWaveform, ExportAllExtrapolatedWaveforms and ExportMetadata which specifies how long the junk radiation lasts."; *)

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

SetStatus[str_] :=
  Module[
    {},
    If[$FrontEnd === Null,
       Print[str]];
    ExportStatus = str];

ExportExtrapolatedWaveform[run_String, file_String, mass_, l_Integer, m_Integer, OptionsPattern[]] :=
 Module[{dir, extrap, junkTime, afterjunk, final, dataset},
  dir = DirectoryName[file];
  If[dir=!="" && FileType[dir]=!=Directory,
    CreateDirectory[dir];
  ];

  SetStatus["Exporting extrapolated waveform for "<>run<>"("<>ToString[l]<>", "<>ToString[m]<>") to "<>file];

  (* TODO: don't hard code 200 here *)
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
    Error["Unsupported file format: "<>fileExtension[file]];
  ];
];

ExportAllExtrapolatedWaveforms[run_String, file_String, mass_, excludeModes_:None] :=
 Module[{dir, modes, files, excludeRun, excludePattern},
  dir = DirectoryName[file];
  modes = ReadPsi4Modes[run];
  If[excludeModes =!= None,
    {excludeRun, excludePattern} = excludeModes;
    If[run === excludeRun,
      modes = Select[modes, (!MatchQ[#,excludePattern]) &]];
  ];

  Switch[fileExtension[file],
  "asc"|"asc.gz",
    files=(dir<>fileBaseName[file]<>"_l"<>ToString[#[[1]]]<>"_m"<>ToString[#[[2]]]<>"_rinf."<>fileExtension[file] &) /@ modes;
    MapThread[ExportExtrapolatedWaveform[run, #1, mass, Sequence@@#2]&, {files, modes}];,
  "h5",
    (ExportExtrapolatedWaveform[run, file, mass, Sequence@@#]&) /@ modes;,
  _,
    Error["Unsupported file format: "<>fileExtension[file]];
  ];
];

ExportExtrapolatedStrain[run_String, file_String, mass_, l_Integer, m_Integer, om_, OptionsPattern[]] :=
 Module[{dir, extrap, strain, junkTime, afterjunk, final, dataset},
  dir = DirectoryName[file];
  If[dir=!="" && FileType[dir]=!=Directory,
    CreateDirectory[dir];
  ];

  SetStatus["Exporting extrapolated strain waveform for "<>run<>"("<>ToString[l]<>", "<>ToString[m]<>") to "<>file];

  (* TODO: don't hard code 200 here *)
  extrap    = ExtrapolatePsi4[run, l, m, AlignPhaseAt->200, MassADM->mass, ExtrapolationOrder->3];
  (* Use time-domain integration for the m=0 modes *)
  strain    = If[m =!= 0,
    StrainFromPsi4[extrap, om],
    StrainFromPsi4[extrap, CoordinateRange[extrap]]];

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
    Error["Unsupported file format: "<>fileExtension[file]];
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
    
    (ExportExtrapolatedStrain[run, file, mass, Sequence@@#, om Abs[#[[2]]]/2]&) /@ modes;,
  _,
    Error["Unsupported file format: "<>fileExtension[file]];
  ];
];

ExportExtractedWaveform[run_String, file_String, l_Integer, m_Integer, r_] :=
 Module[{dir, psi4, final, dataset, rad = ToString[r]},
  dir = DirectoryName[file];
  If[dir=!="" && FileType[dir]=!=Directory,
    CreateDirectory[dir];
  ];


  If[fileExtension[file] === "h5" && FileExistsQ[file],
     dataset="l"<>ToString[l]<>"_m"<>ToString[m]<>"_r"<>rad;
     If[MemberQ[ReadHDF5[file], "/"<>dataset],
        SetStatus["Skipping existing dataset "<>file<>":"<>dataset];
        Return[]]];

  SetStatus["Exporting extracted waveform for "<>run<>" ("<>ToString[l]<>", "<>ToString[m]<>", "<>rad<>") to "<>file];

  psi4  = ReadPsi4[run, l, m, Round[ToExpression[rad]]];
  final = Join[Re[psi4], Im[psi4]];

  Switch[fileExtension[file],
  "h5",
    dataset="l"<>ToString[l]<>"_m"<>ToString[m]<>"_r"<>rad;
    Module[
      {tmp = file<>".tmp.h5"},
      If[FileExistsQ[tmp], SetStatus["Deleting existing temporary file "<>tmp]; DeleteFile[tmp]];
      If[FileExistsQ[file],RenameFile[file, tmp]];
      Export[tmp, final, {"Datasets", dataset}, "Append"->True];
      RenameFile[tmp,file]];,
  "asc",
    Export[file, final, "TABLE"];,
  "asc.gz",
    Export[file, final, {"GZIP", "TABLE"}];,
  _,
    Error["Unsupported file format: "<>fileExtension[file]];
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
    Error["Unsupported file format: "<>fileExtension[file]];
  ];
];

ExportAllWaveforms[run_String, file_String, mass_] := Module[{},
  ExportAllExtractedWaveforms[run, file];
  ExportAllExtrapolatedWaveforms[run, file, mass];
];


(* Return the isolated horizon spins only for the range where the apparent horizon was found *)
ihSpin[run_, i_] := Module[{ti, tf, dir, spins},
  {ti, tf} = First[CoordinateRanges[ReadAHMass[run, i+1]]];
  spins = Table[Slab[ReadIsolatedHorizonSpin[run, i, dir], ti;;tf], {dir, 3}];
  Transpose[{ToListOfCoordinates[spins[[1]]], Sequence@@(ToListOfData/@spins)}]
]

(* Local quantities *)
ExportLocalQuantity[run_String, what_, i_, file_String] :=
 Module[{dir, f, dsName},
  dir = DirectoryName[file];
  If[dir=!="" && FileType[dir]=!=Directory,
    CreateDirectory[dir];
  ];

  If[fileExtension[file] === "h5" && FileExistsQ[file],
     dsName = (what /. {Coordinates -> "traj", Spin -> "spin", HorizonMass -> "horizon_mass"}) <> ToString[i];
     If[MemberQ[ReadHDF5[file], "/"<>dsName],
        SetStatus["Skipping existing dataset "<>file<>":"<>dsName];
        Return[],
       (*else*)
       SetStatus["Not skipping for "<>file<>":"<>dsName]]];

  SetStatus["Exporting " <> ToString[what] <> " data for "<>run<>" to "<>file];

  If[what === HorizonMass && !HaveChristodoulouMassData[run,i,i-1],
     SetStatus["Warning: Horizon mass data not found for BH "<>ToString[i]<>" in "<>run];
     Return[]];

  If[what === Spin && (!HaveIsolatedHorizonSpinData[run,i-1] || !HaveHorizonData[run,i]),
     SetStatus["Warning: Spin data not found for BH "<>ToString[i]<>" in "<>run];
     Return[]];

  f = Switch[what,
    Coordinates, ReadBHCoordinates[run, i-1],
    Spin, ihSpin[run, i-1],
    HorizonMass, ChristodoulouMass[run, i, i-1]];

  Switch[fileExtension[file],
  "asc",
    Export[file, f, "TABLE"];,
  "asc.gz",
    Export[file, f, {"GZIP", "TABLE"}];,
  "h5",
    dsName = (what /. {Coordinates -> "traj", Spin -> "spin", HorizonMass -> "horizon_mass"}) <> ToString[i];
   
    Module[
      {tmp = file<>".tmp.h5"},
      If[FileExistsQ[tmp], SetStatus["Deleting existing temporary file "<>tmp]; DeleteFile[tmp]];
      If[FileExistsQ[file],RenameFile[file, tmp]];
      Export[tmp, f, {"Datasets", dsName}, "Append"->True];
      RenameFile[tmp,file]];,
  _,
    Error["Unsupported file format: "<>fileExtension[file]];
  ];
];


(* Run metadata *)

coord[d_] :=
 {"x", "y", "z"}[[d]];

overrideMetadata[md_, addMD_] :=
  (* TODO: Support versions of Mathematica without associations *)
  Normal[Join[Association[md],Association[addMD]]];

runMetadata[run_, mass_, ecc_, tJunk_, addMD_] :=
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
    Error["Unknown evolution code used"];
  ];

  (* TODO: Don't hardcode any of these *)
  overrideMetadata[{(* "comments" -> "", *)
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
   Sequence @@ Table[
      "initial-bh-mass-parameter" <> ToString[i] ->
       ReadPunctureBareMassParameters[run][[i]], {i, 1, 2}],
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
      If[HaveChristodoulouMassData[run, i, i-1],
        Interpolation[ChristodoulouMass[run, i, i-1]][tJunk],
        "unknown"], {i, 1, 2}],

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
       ReadPunctureSpinParameters[run, i][[d]],
        {i, 0, 1}, {d, 1, 3}],
   "final-bh-mass" -> If[HaveChristodoulouMassData[run, 3, 2],
                         Last[ChristodoulouMass[run, 3, 2]], "unknown"],
   Sequence @@ Flatten@Table[
      "final-bh-spin" <> coord[d] ->
       Last[ReadIsolatedHorizonSpin[run, 2, d]], {d, 1, 3}],
   "data-type" -> "NR"
   }, addMD]
  ];

runAllData[run_String, mass_, ecc_, tJunk_, format_String, addMD_] :=  Module[{modes, radii, ext},
 ext = Switch[format, "HDF5", "h5", "ASCII", "asc.gz", _, Error["Unrecognised format: "<>ToString@format]];
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

 {"metadata" -> runMetadata[run, mass, ecc, tJunk, addMD],
  "body-data" ->
   {Sequence @@ Table["spin" <> ToString[i] ->
      spinFileName[i,format], {i, 1, 3}],
   Sequence @@ Table["trajectory" <> ToString[i] ->
      trajFileName[i,format], {i, 1, 2}],
   Sequence @@ Table["horizon-mass" <> ToString[i] ->
      horMassFileName[i,format], {i, 1, 3}]},
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

makeEntry[val_String] :=
 val;

makeEntry[val_] :=
  ToString[val, CForm];

makeEntry[vals_List] :=
  StringJoin@@Riffle[Map[makeEntry,vals],", "];

(* ExportSimFormat::usage = "ExportSimFormat is an option for ExportSim which specifies the format to use. Possible choices are \"ASCII\" and \"HDF5\"."; *)

Options[ExportMetadata] = {"JunkTime" -> None, "ExportSimFormat" -> "ASCII"};
ExportMetadata[file_String, run_String, mass_?NumericQ, ecc_?NumericQ, addMD_:{}, OptionsPattern[]] :=
 Module[{tJunk},
  tJunk  = OptionValue[JunkTime];
  If[SameQ[tJunk, None], tJunk = 0;];

  Export[file, makeMetadataFile[runAllData[run, mass, ecc, tJunk, OptionValue[ExportSimFormat], addMD]], "Text"];
];

ExportMetadata[file_String, md_List] :=
 Module[{},
  Export[file, makeMetadataFile[md], "Text"];
];


(* Full run *)

Options[ExportNumericalRelativitySimulation] = {"ExportSimFormat" -> "ASCII", "ExportOnly" -> All, "ExcludeModes" -> None, "GuessResolutionSuffix" -> True, "AdditionalMetadata" -> {}};
DocumentationBuilder`OptionDescriptions["ExportNumericalRelativitySimulation"] = {
  "ExportSimFormat" -> "Data format for exported simulation. \"ASCII\" and \"HDF5\" are "<>
    "the currently supported formats.",
  "ExportOnly" -> "Which data to export from a simulation. Possible values are All, or "<>
    "a list of items including any of \"ExtrapolatedWaves\", \"Metadata\", "<>
    "\"Coordinates\", \"Spin\", \"HorizonMass\" or \"FiniteRadiiWaves\".",
  "ExcludeModes" -> "A list of {l,m} spherical harmonic modes to exclude from the export."
  };

ExportNumericalRelativitySimulation[run_String, niceName_, outputDirectory_, mass_, ecc_, OptionsPattern[]] :=
  Module[{dir, h, n, ext, all, export},

    h = ReadCoarseGridSpacing[run];
    n = Round[0.6/(h/2^5)];
    dir = If[OptionValue[GuessResolutionSuffix],
      FileNameJoin[{outputDirectory, niceName, niceName <> "_"<>ToString[n]}],
      FileNameJoin[{outputDirectory, niceName}]];

    Print[run <> " -> " <> dir];
    If[FileType[dir] === None, CreateDirectory[dir]];

    Switch[ToString[OptionValue[ExportSimFormat]],
     "ASCII",
      ext = ".asc.gz";,
     "HDF5",
      ext = ".h5",
     _,
      Error["Unsupported value " <> ToString[OptionValue[ExportSimFormat]] <> " for ExportSimFormat option."]
    ];

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
                           ExportLocalQuantity[run, Spin, 2, dir <> "/spin2"<>ext];
                           Catch[ExportLocalQuantity[run, Spin, 3, dir <> "/spin3"<>ext], _ErrorString],
      "HorizonMass",       ExportLocalQuantity[run, HorizonMass, 1, dir <> "/horizon_mass1"<>ext];
                           ExportLocalQuantity[run, HorizonMass, 2, dir <> "/horizon_mass2"<>ext];
                           Catch[ExportLocalQuantity[run, HorizonMass, 3, dir <> "/horizon_mass3"<>ext], _ErrorString],
      "Metadata",          ExportMetadata[dir<>"/"<>niceName<>If[OptionValue[GuessResolutionSuffix],"_"<>ToString[n],""]<>".bbh", run, mass,
                                          ecc, OptionValue[AdditionalMetadata]],
      _, Error["Error"]], {item, export}];
  ];

Options[ExportConfig] = Options[ExportSim];
ExportConfig[name_ -> {Madm_, sims_, ecc_},  outputDirectory_, opts:OptionsPattern[]] :=
  Scan[ExportNumericalRelativitySimulation[#, name, outputDirectory, Madm, ecc, opts] &, sims];

HDF5FilesDiffer[f1_String, f2_String] :=
  Module[{code,out,err},
    {code,out,err} = RunSubprocess[{"/opt/local/bin/h5diff", "-q", f1, f2}, 
      Exceptions -> False, "StringLists" -> False];
    If[code === 2*256, Error["Error when running h5diff: "<>ToString[err]]];
    code === 1*256];

(* TODO: check on Linux whether exit codes from Run are multiplied by
   256. Also find out why they are on Mac OS.*)

BinaryBlackHoleRelaxedTime[sim_String] :=
  200;


extrapolatedWaveform[sim_String] :=
  Module[{rads, freq22, freqFit, ominit, omCutoff, tJunkCactus, extrapStrain, tFreqFit},
    rads = ReadPsi4Radii[sim];

    tFreqFit = {100, 200};

    freq22 = -Frequency[100 Shifted[ReadPsi4[sim, 2, 2, 100], -100]];
    freqFit = Module[{model, t, fit, a0, a1, a2, tFit = tFreqFit},
      model = a0 + a1 t;
      fit = FindFit[ToList[Slab[freq22, Span @@ tFit]], model, {a0, a1}, 
        t];
      <|(*"Plot" -> 
      Show[PresentationListLinePlot[Slab[freq22, 0 ;; tFit[[2]] + 100], 
        GridLines -> {tFit, None}], Plot[model /. fit, {t, 0, 200}]],*)
      "Freq0" -> a0 /. fit|>];

    omInit = freqFit["Freq0"];
    omCutoff = 0.75*freqFit["Freq0"];
    tJunkCactus = If[tMerger < 500, 50, 200];

    extrapStrain = 
      WaveformExtrapolationAnalysis[rads, 
        StrainFromPsi4[ReadPsi4[sim, 2, 2, #], omCutoff] & /@ rads, rads,
        ReadADMMass[sim]];

    extrapStrain["ExtrapolatedWaveform"]];

configName[sim_String] := 
 StringReplace[sim, x__ ~~ "_" ~~ NumberString ~~ EndOfString :> x];

resolutionCode[sim_String] := 
 StringReplace[sim, 
  x__ ~~ "_" ~~ n : NumberString ~~ EndOfString :> n];

ExportSXSSimulationResolutions[sims_List, outDir_, opts___] :=
  Do[
    ExportSXSSimulation[sim, 
      FileNameJoin[{outDir, configName[sim], resolutionCode[sim]}], opts],
    {sim, sims}];

Options[ExportSXSSimulation] = {"RelaxedTime" -> Automatic, "Eccentricity" -> None, "h22" -> Automatic};

ExportSXSSimulation[sim_String, dir_, h22_DataTable, opts:OptionsPattern[]] :=
  ExportSXSSimulation[sim, dir, "h22" -> h22, opts];

ExportSXSSimulation[sim_String, dir_String, opts:OptionsPattern[]] :=
  Module[{waveformFile, mdFile, tRelaxed, masses, spins, md, mdText, i, coord, tPeak,
         ecc, tmpFile, h22, initialPunctureADMMasses},

    h22 = Replace[OptionValue["h22"],
      {d_DataTable :> d,
        Automatic :> extrapolatedWaveform[sim],
        _ :> Error["Unrecognised option value"]}];

  waveformFile = dir <> "/rhOverM_Asymptotic_GeometricUnits.h5";
  mdFile = dir <> "/metadata.txt";
  If[! FileExistsQ[dir], 
    CreateDirectory[dir, CreateIntermediateDirectories -> True]];

  tmpFile = waveformFile<>".tmp";  
    Export[tmpFile, {Map[{#[[1]], Re[#[[2]]], Im[#[[2]]]} &,
      ToList[h22]]}, {"HDF5", "Datasets", {"Extrapolated_N2.dir/Y_l2_m2.dat"}}];
    
    (* Print[{FileExistsQ[waveformFile],HDF5FilesDiffer[tmpFile, waveformFile]}]; *)
    If[!FileExistsQ[waveformFile] || HDF5FilesDiffer[tmpFile, waveformFile],
      Print["Writing ", waveformFile];
      If[FileExistsQ[waveformFile], DeleteFile[waveformFile]];
      RenameFile[tmpFile, waveformFile],
      Print["Skipping empty update of ", waveformFile];
      DeleteFile[tmpFile]];

  (* TODO: handle relaxed time for short waveforms *)
   tRelaxed = Replace[OptionValue[RelaxedTime],{
     Automatic :> BinaryBlackHoleRelaxedTime[sim],
     t_ :> t}];

  masses = Table[ReadBlackHoleMass[sim, i], {i, 1, 3}];
  spins = Table[ReadBlackHoleSpin[sim, i], {i, 1, 3}];
  coord[d_] := {"x", "y", "z"}[[d]];
  tPeak = LocateMaximum[Abs[h22]];

  initialPunctureADMMasses = ReadPunctureADMMassParameters[sim];

  md = {"metadata" ->
    DeleteCases[{
      "point-of-contact-email" -> UserEmailDisplayName[],

      "initial-mass1" -> initialPunctureADMMasses[[1]],
      "initial-mass2" -> initialPunctureADMMasses[[2]],

      "relaxed-measurement-time" -> tRelaxed,
      "relaxed-mass1" -> Interpolation[masses[[1]], tRelaxed], 
      "relaxed-mass2" -> Interpolation[masses[[2]], tRelaxed], 
      "relaxed-spin1" -> (Interpolation[#, tRelaxed] & /@ spins[[1]]),
      "relaxed-spin2" -> (Interpolation[#, tRelaxed] & /@ 
         spins[[2]]), 
      "remnant-mass" -> 
       Interpolation[masses[[3]], MaxCoordinate[masses[[3]]]],
      "remnant-spin" -> (Interpolation[#, 
           MaxCoordinate[spins[[3, 1]]]] & /@ spins[[3]]),
       "initial-separation" -> First[ReadBHSeparation[sim]],
       "number-of-orbits-to-h-peak" -> WaveformCycles[h22, tRelaxed]/2,
       "relaxed-orbital-frequency" -> RelaxedOrbitalFrequency[sim, tRelaxed],
       
       (* Initial data parameters (Bowen York) *)
       Sequence @@ 
       Table["initial-bh-mass-parameter" <> ToString[i] -> 
         ReadPunctureBareMassParameters[sim][[i]], {i, 1, 2}], Sequence @@ 
       Flatten@Table[
         "initial-bh-position" <> ToString[i + 1] -> 
         InitialPosition[sim, i], {i, 0, 1}], Sequence @@ 
       Flatten@Table[
         "initial-bh-momentum" <> ToString[i + 1] -> 
         InitialLinearMomentum[sim, i], {i, 0, 1}],
       Sequence @@ 
       Flatten@Table[
         "initial-bh-spin" <> ToString[i + 1] -> 
         ReadPunctureSpinParameters[sim, i], {i, 0, 1}],
       If[OptionValue[Eccentricity] =!= None, Sequence["relaxed-eccentricity" -> OptionValue[Eccentricity]],Sequence[]]
       
      },Null]};
  mdText = makeMetadataFile[md];
  Export[mdFile, mdText, "Text"]];

End[];

EndPackage[];
