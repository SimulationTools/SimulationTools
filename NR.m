(* ::Package:: *)

(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

(* A package for dealing with numerical relativity data *)

BeginPackage["NR`", {"BHCoordinates`", "Convergence`", "DataRegion`", "DataTable`",
  "Horizons`", "Memo`", "Parameters`", "Plotting`", "ReadHDF5`", "RunFiles`",
  "SystemStatistics`", "Timers`"}];

ReadPsi4::usage = "ReadPsi4[run, l, m, r] returns a DataTable of the l,m mode of Psi4 at radius r from run.";
ReadPsi4From::usage = "ReadPsi4From is an option for ReadPsi4 which specifies which mode decomposition to read from. Possible options are \"MultipoleHDF5\", \"MultipoleASCII\" and \"YlmDecomp\".";
ReadPsi4Phase::usage = "ReadPsi4Phase[run, l, m, r, threshold] returns a DataTable of the phase of the complex l,m mode of Psi4 at radius r from run.  The phase is cut off after the time that the amplitude goes below threshold."
ReadMultipoleHDF5::usage = "ReadMultipoleHDF5[run, var, l, m, r] returns a DataTable of the l,m mode of var at radius r from run using the HDF5 output of the Multipole thorn.";
ReadMultipoleASCII::usage = "ReadMultipoleASCII[run, var, l, m, r] returns a DataTable of the l,m mode of var at radius r from run using the ASCII output of the Multipole thorn.";
ReadYlmDecomp::usage = "ReadYlmDecomp[run, var, l, m, r] returns a DataTable of the l,m mode of var at radius r from run using the YlmDecomp output from the WeylScal4 thorn.";

ReadPsi4RadiiStrings::usage = "ReadPsi4RadiiStrings[run] returns a list of the radii (in original string form) at which the modes of \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) are available in run.";
ReadPsi4Radii::usage = "ReadPsi4Radii[run] returns a list of the radii at which the modes of \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) are available in run.";
ReadMultipoleHDF5Radii::usage = "ReadMultipoleHDF5Radii[run, var] returns a list of the radii at which the modes of var are available in run.";
ReadMultipoleASCIIRadii::usage = "ReadMultipoleASCIIRadii[run, var] returns a list of the radii at which the modes of var are available in run.";
ReadYlmDecompRadii::usage = "ReadYlmDecompRadii[run, var] returns a list of the radii at which the modes of var are available in run.";

ReadPsi4Modes::usage = "ReadPsi4Modes[run] returns a list of the modes of \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) that are available in run.";
ReadMultipoleHDF5Modes::usage = "ReadMultipoleHDF5Modes[run, var] returns a list of the modes of var that are available in run.";
ReadMultipoleASCIIModes::usage = "ReadMultipoleASCIIModes[run, var] returns a list of the modes of var that are available in run.";
ReadYlmDecompModes::usage = "ReadYlmDecompModes[run, var] returns a list of the modes of var that are available in run.";

Options[ReadPsi4] = {ReadPsi4From -> Automatic};
MultipolePsi4Variable = "psi4";
YlmDecompPsi4Variable = "Psi4";

StrainFromPsi4::usage = "StrainFromPsi4[\!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\), \!\(\*SubscriptBox[\(\[Omega]\), \(0\)]\)] converts a DataTable containing \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\)[t] into strain and its time derivative, {h[t], h'[t]}, using the method of Reisswig and Pollney with a cut-off frequency \!\(\*SubscriptBox[\(\[Omega]\), \(0\)]\).
StrainFromPsi4[\!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\), {\!\(\*SubscriptBox[\(t\), \(start\)]\), \!\(\*SubscriptBox[\(t\), \(end\)]\)}] converts using time domain integration.";


ExtrapolateScalarFull;
ExtrapolateScalar;
ExtrapolatedValue;
ExtrapolateScalarWithRadii;
Data;
ExtrapolatedCurve;
ExtrapolateDataTables;
AlignPhases;
RadiusTimeDataToTimeRadiusData;
ExtrapolateRadiatedQuantity;
ExtrapolatePsi4Phase;
ReadADMMass;
TortoiseCoordinate;
UseTortoiseCoordinate;
FilterDCT;
ApplyToPhases;
ExtrapolationError;
ExtrapolatePsi4Amplitude;
ExtrapolatePsi4;
FitFunction;
LocateMaximum;
LocateMaximumPoint;
GridSpacingOnLevel;
BoxRadiiOnLevel;
BoxRadiiForCentre;
RefinementLevels;
GridStructure;
FinestGridSpacing;
TimeRefinementFactors;
CourantFactorOnLevel;
LevelExistsEvery;
RadialPoints;
RunName;
ReadCoarseGridSpacing;
ReadAngularPoints;
ReadInnerBoundary;
ReadOuterBoundary;
CountRefinementLevels;

AbsOfPhase;
ReadPunctureADMMasses;
ReadPunctureADMMasses2;
PercentageDifference;
NumCycles::usage = "NumCycles[run,start] gives the number of gravitational wave cycles for run. The number of cycles is calculated starting at start and terminating at the merger, which is determined from the maimum of the gravitational wave signal.
NumCycles[psi4,start] operates on the DataTable psi4 instead of run."

ReadHamiltonianConstraintNorm;

ReadWaveformFile;
AlignMaxima;
AlignMaximaOfAbs;
ReadFineTimeStep;
ReadTimeRange;
ShiftPhase;
PhaseOfFrequency::usage = "PhaseOfFrequency[d] gives the phase of a complex data table d as a function of the frequency, where the frequency is defined as the derivative of the phase.";

ReturnValue;
FittedFunction;
Eccentricity;
FitEcc;
FitEccOm;
FitParameters;
Continuous;
ToFixedWidth;

LRange;
RadiusRange;

TotalMass;
InitialSpinAngularMomentum;
InitialLinearMomentum;
InitialOrbitalAngularMomentum;
InitialAngularMomentum;
InitialSpin;
SpinAngle;
InitialSpinAngle;
InitialDimensionlessSpin;
MassRatio;
SymmetricMassRatio;
InitialSeparation;

Options[ExtrapolateRadiatedQuantity] = 
  {ExtrapolationOrder -> 1,
   UseTortoiseCoordinate -> True,
   MassADM -> None,
   ApplyFunction -> None, 
   AlignPhaseAt -> None,
   RadiusRange -> All,
   ExtrapolationErrorRelative -> False};

Options[ExtrapolationError] = Options[ExtrapolateRadiatedQuantity];
Options[ExtrapolatePsi4Phase] = Options[ExtrapolateRadiatedQuantity];

ReadTwoPuncturesData::usage = "ReadTwoPuncturesData[file, col] reads a data file output by the standalone TwoPunctures code by Marcus Ansorg and returns a DataRegion containing the data in column col. col can be 1, 2 or 3 for the coordinates or >= 4 for the data.";

Begin["`Private`"];

RunDirectory := Global`RunDirectory;

(*--------------------------------------------------------------------
  Reading Subscript[\[Psi], 4]
  --------------------------------------------------------------------*)

ReadPsi4[runName_String, l_?NumberQ, m_?NumberQ, rad_:Automatic, OptionsPattern[]] :=
  Module[{psi4, radii, radString},

  (* Get a list of radii available in the form expr -> "exprstring" *)
  radii = Thread[ReadPsi4Radii[runName] -> ReadPsi4RadiiStrings[runName]];

  If[radii == Null,
    Throw["No \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) data found."];
  ];

  (* Which radius to read: Automatic=first available, Infinity, or a number *)
  Which[
    rad === Automatic,
      radString = radii[[1,2]],
    (rad === Infinity) && ((Infinity /. radii) === "inf"),
      radString = "inf",
    NumberQ[rad] && (Select[radii[[All,1]], NumberQ] =!= {}),
      radString = First[Nearest[radii, rad]],
    True,
      Throw["Radius "<>ToString[rad]<>" not found."];
  ];

  (* Read the actual data *)
  If[OptionValue[ReadPsi4From] === Automatic,
    Which[
      HaveMultipoleHDF5[runName, MultipolePsi4Variable],
        psi4 = ReadMultipoleHDF5[runName, MultipolePsi4Variable, l, m, radString];,
      HaveYlmDecomp[runName, YlmDecompPsi4Variable],
        psi4 = ReadYlmDecomp[runName, YlmDecompPsi4Variable, l, m, radString];,
      HaveMultipoleASCII[runName, MultipolePsi4Variable],
        psi4 = ReadMultipoleASCII[runName, MultipolePsi4Variable, l, m, radString];,
      True,
        Throw["No \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) data found."];
    ];
  ,
    Switch[OptionValue[ReadPsi4From],
      "MultipoleHDF5",
      If[HaveMultipoleHDF5[runName, MultipolePsi4Variable],
        psi4 = ReadMultipoleHDF5[runName, MultipolePsi4Variable, l, m, radString];,
        Throw["Multipole HDF5 data not available for run "<>runName];
      ];,
      "MultipoleASCII",
      If[HaveMultipoleASCII[runName, MultipolePsi4Variable],
        psi4 = ReadMultipoleASCII[runName, MultipolePsi4Variable, l, m, radString];,
        Throw["Multipole ASCII data not available for run "<>runName];
      ];,
      "YlmDecomp",
      If[HaveYlmDecomp[runName, YlmDecompPsi4Variable],
        psi4 = ReadYlmDecomp[runName, YlmDecompPsi4Variable, l, m, radString];,
        Throw["YlmDecomp data not available for run "<>runName];
      ];,
      _,
      Throw["Invalid value for ReadPsi4From option."];
    ];
  ];

  psi4
];

DefineMemoFunction[ReadYlmDecomp[runName_String, var_String, l_?NumberQ, m_?NumberQ, rad_],
  Module[{fileName, threeCols, psi4},
    fileName = "Ylm_WEYLSCAL4::"<>var<>"r_l" <>
             ToString[l] <> "_m" <> ToString[m] <> "_r" <> ToString[rad] <> ".asc";
    threeCols = ReadColumnFile[runName, fileName, {1,2,3}];
    psi4 = Map[{#[[1]], #[[2]] + I #[[3]]}&, threeCols];
    Return[AddAttribute[MakeDataTable[psi4], RunName -> runName]]]
];

DefineMemoFunction[ReadMultipoleASCII[runName_String, var_String, l_?NumberQ, m_?NumberQ, rad_],
  Module[{fileName, threeCols, psi4},
    fileName = "mp_"<>var<>"_l" <>
             ToString[l] <> "_m" <> ToString[m] <> "_r" <> ToString[rad] <> ".asc";
    threeCols = ReadColumnFile[runName, fileName, {1,2,3}];
    psi4 = Map[{#[[1]], #[[2]] + I #[[3]]}&, threeCols];
    Return[AddAttribute[MakeDataTable[psi4], RunName -> runName]]]
];

DefineMemoFunction[ReadMultipoleHDF5[runName_String, var_String, l_?NumberQ, m_?NumberQ, rad_],
  Module[{fileName, datasetName, runFiles, files, data, psi4},
    runFiles = MultipoleHDF5Files[runName, var];
    datasetName = "l" <> ToString[l] <> "_m" <> ToString[m] <> "_r" <> ToString[rad];
    files = Map[ReadHDF5[#,{"Datasets", datasetName}] &, runFiles];
    data = MergeFiles[files];
    psi4 = Map[{#[[1]], #[[2]] + I #[[3]]}&, data];
    Return[AddAttribute[MakeDataTable[psi4], RunName -> runName]]]
];

ReadPsi4Phase[run_, l_: 2, m_: 2, r_: 100, threshold_: 10.0^-3] :=
 Module[{psi4, rAmp, rAmpTb, t2, phase},

  (* Two problems with this function: (1) The default radius is not
     determined from the available radii.  This is because in my runs,
     I commonly have a useless one at r = 30 which I don't care about.
     (2) The threshold is not adjusted for the mode number.  Probably
     this is not possible to fix in a general way.  *)

  psi4 = ReadPsi4[run, l, l, r];
  rAmp = r Abs[psi4];
  rAmpTb = ToList[rAmp];
  t2 = Last[Select[rAmpTb, #[[2]] > threshold &]][[1]];
  phase =
   DataTableInterval[Phase[psi4], {rAmpTb[[1, 1]] - 100.0, t2}]];

(* Return a list of radii available *)
ReadPsi4Radii[runName_] := ToExpression /@ (ReadPsi4RadiiStrings[runName] /. "inf" -> "Infinity");

ReadPsi4RadiiStrings[runName_] :=
  Module[{mpl},
  Which[
    Length[mpl = ReadMultipoleHDF5Radii[runName, MultipolePsi4Variable]] != 0,
    Null,
    Length[mpl = ReadYlmDecompRadii[runName, YlmDecompPsi4Variable]] != 0,
    Null,
    Length[mpl = ReadMultipoleASCIIRadii[runName, MultipolePsi4Variable]] != 0,
    Null
  ];

  mpl
];

DefineMemoFunction[ReadYlmDecompRadii[runName_, var_],
  Module[{names, radiusFromFileName, radii},
    names = YlmDecompFiles[runName, var];
    radiusFromFileName[name_] :=
      StringReplace[name,
        "Ylm_WEYLSCAL4::"<>var<>"r_l" ~~ __ ~~ "m" ~~ __ ~~ "r"
        ~~ x : (NumberString|"inf") ~~ ".asc" -> x];
    radii = Sort[Union[Map[radiusFromFileName, names]]]]
];

DefineMemoFunction[ReadMultipoleASCIIRadii[runName_, var_],
  Module[{names, radiusFromFileName, radii},
    names = MultipoleASCIIFiles[runName, var];
    radiusFromFileName[name_] :=
      StringReplace[name,
        "mp_"<>var<>"_l" ~~ __ ~~ "m" ~~ __ ~~ "r"
        ~~ x : (NumberString|"inf") ~~ ".asc" -> x];
    radii = Sort[Union[Map[radiusFromFileName, names]]]]
];

DefineMemoFunction[ReadMultipoleHDF5Radii[runName_, var_],
  Module[{datasets, radii},
    datasets = Union@@Map[ReadHDF5[#] &, MultipoleHDF5Files[runName, var]];
    radii = Sort[(Union@@StringCases[datasets, "r" ~~ x : (NumberString|"inf") :> x])];
    radii
  ]
];

(* Check if modes are available as Multipole HDF5, Multipole ASCII or YlmDecomp *)
HaveYlmDecomp[runName_, var_] :=
  ReadYlmDecompRadii[runName, var] =!= {};

HaveMultipoleASCII[runName_, var_] :=
  ReadMultipoleASCIIRadii[runName, var] =!= {};

HaveMultipoleHDF5[runName_, var_] :=
  ReadMultipoleHDF5Radii[runName, var] =!= {};

(* Return a list of (l, m) modes available *)
ReadPsi4Modes[runName_] :=
  Module[{mpl},
  Which[
    Length[mpl = ReadMultipoleHDF5Modes[runName, MultipolePsi4Variable]] != 0,
    Null,
    Length[mpl = ReadYlmDecompModes[runName, YlmDecompPsi4Variable]] != 0,
    Null,
    Length[mpl = ReadMultipoleASCIIModes[runName, MultipolePsi4Variable]] != 0,
    Null
  ];

  mpl
];

DefineMemoFunction[ReadYlmDecompModes[runName_, var_],
  Module[{names, modes},
    names = YlmDecompFiles[runName, var];
    modes = Sort[Round /@ ToExpression /@ (Union@@StringCases[names,
      "l" ~~ l:NumberString ~~ "_m" ~~ m:NumberString :> {l,m}])];
    modes
  ]
];

DefineMemoFunction[ReadMultipoleASCIIModes[runName_, var_],
  Module[{names, modes},
   names = MultipoleASCIIFiles[runName, var];
   modes = Sort[Round /@ ToExpression /@ (Union@@StringCases[names, "l" ~~ l:NumberString ~~ "_m" ~~ m:NumberString :> {l,m}])];
   modes
  ]
];

DefineMemoFunction[ReadMultipoleHDF5Modes[runName_, var_],
  Module[{datasets, modes},
    datasets = Union@@Map[ReadHDF5, MultipoleHDF5Files[runName, var]];
    modes = Sort[Round /@ ToExpression /@ (Union@@StringCases[datasets, "l" ~~ l:NumberString ~~ "_m" ~~ m:NumberString :> {l,m}])];
    modes
  ]
];

(* Return a list of files for the variable 'var' in 'runName'. 'runName' may
 * be a SimFactory run, a directory or a single file *)
YlmDecompFiles[runName_, var_, l_:"*", m_:"*", r_:"*"] := Module[{runFiles},
  If[FileType[runName]===File,
      runFiles = {runName},
      runFiles = FindRunFilesFromPattern[runName,
        "Ylm_WEYLSCAL4::"<>var<>"r_l"<>ToString[l]<>"_m"<>ToString[m]<>"_r"<>ToString[r]<>".asc"];
  ];

  runFiles
];

MultipoleASCIIFiles[runName_, var_, l_:"*", m_:"*", r_:"*"] := Module[{runFiles},
  If[FileType[runName]===File,
      runFiles = {runName},
      runFiles = FindRunFilesFromPattern[runName,
        "mp_"<>var<>"_l"<>ToString[l]<>"_m"<>ToString[m]<>"_r"<>ToString[r]<>".asc"];
  ];

  runFiles
];

MultipoleHDF5Files[runName_, var_] := Module[{runFiles},
  If[FileType[runName]===File,
      runFiles = {runName},
      runFiles = FindRunFile[runName, "mp_"<>var<>".h5"];
  ];

  runFiles
];


ReadADMMass[runName_String] :=
  Module[{massMDFiles, output, lines},
    massMDFiles = FindRunFile[runName, "ADM_mass_tot.asc"];
    If[massMDFiles =!= {},
      Return[ReadList[massMDFiles[[1]], Real][[1]]],
      (* Else *)
      output = StandardOutputOfRun[runName];
      If[Length[output] < 1,
        Throw["Cannot find standard output for run "<>runName]];

      lines = Select[ReadList[output[[1]], String], StringMatchQ[#, __ ~~ "ADM mass is" ~~ __] &, 1];
      If[Length[lines] < 1,
        Throw["Cannot find ADM mass in standard output of run "<>runName]];
      ToExpression@Last@StringSplit[lines[[1]]]]];

(*--------------------------------------------------------------------
  Extrapolation
  --------------------------------------------------------------------*)

ExtrapolateScalarFull[order_, rfTb_] :=
  Module[{radModel, rfCompact, a, x, fit, curve, rMin},
   radModel = Sum[a[i] x^i, {i, 0, order}];
   rfCompact = Map[{1/#[[1]] // N, #[[2]]} &, rfTb];
   fit = FindFit[rfCompact, radModel, Table[a[i], {i, 0, order}], x];
   rMin = rfTb[[1]][[1]];
   curve = Table[{x, radModel /. fit},
     {x, 0, 1./rMin, 1./rMin/10}];
   Return[
    {ExtrapolatedValue -> a[0] /. fit,
     Data -> rfCompact,
     ExtrapolatedCurve -> curve}]];

(*
 ExtrapolateScalarFull[order_, rfTb_] :=
  Module[{radModel, rfCompact, a, x, fit, curve, rMin},
   radModel = Sum[a[i] x^i, {i, 0, order}];
   rfCompact = Map[{1/#[[1]] // N, #[[2]]} &, rfTb];
   d = Map[Last, rfCompact];
   xs = Map[First, rfCompact];
   m = Map[Table[#^r, {r, 0, order}] &, xs];
   ls = LeastSquares[m, d];
   rs = Table[r, {r, 0, order}];
   fit = MapThread[a[#1] -> #2 &, {rs, ls}];
(*   fit = FindFit[rfCompact, radModel, Table[a[i], {i, 0, order}], x];*)
   rMin = rfTb[[1]][[1]];
   curve = Table[{x, radModel /. fit},
     {x, 0, 1./rMin, 1./rMin/10}];
   Return[
    {ExtrapolatedValue -> a[0] /. fit,
     Data -> rfCompact,
     ExtrapolatedCurve -> curve}]]; 
*)

ExtrapolateScalar[args__] :=
 ExtrapolatedValue /. ExtrapolateScalarFull[args];

ExtrapolateScalarWithRadii[f_, rads_List, order_:1] :=
  ExtrapolatedValue /. ExtrapolateScalarFull[order, MapThread[List, {rads, Map[f, rads]}]];

RadiusTimeDataToTimeRadiusData[rdTb : {{_, DataTable[__]} ...}] :=
 Module[{rads, dts, ts, lists, tbToVec, vecs, rfTbs, combineWithRads, lengths, rfWithRads},
  rads = Map[First, rdTb];
(*  Print["rads = ", rads];*)
  dts = Map[Last, rdTb];
  lists = Map[ToList, dts];
  lengths = Map[Length, lists];
  If[! (Length[Union[lengths]] === 1), 
  Throw["ExtrapolateDataTables: Input DataTable objects do not have \
  the same number of points: ", lengths]];
  ts = Map[First, First[lists]];
  tbToVec[tb_] := Map[Last, tb];
  vecs = Map[tbToVec, lists];
  rfTbs = Transpose[vecs];
  combineWithRads[v_] := MapThread[List, {rads, v}];
  rfWithRads = Map[combineWithRads, rfTbs];
  MapThread[List, {ts, rfWithRads}]];

Options[ExtrapolateDataTables] = {Resample -> False};

ExtrapolateDataTables[p_Integer, rdTb : {{_, DataTable[__]} ...}, opts___] :=
 Module[{trd, rds, ts, extraps},
  trd = RadiusTimeDataToTimeRadiusData[rdTb];
  rds = Map[Last,trd];
  ts = Map[First,trd];
  extraps = Map[ExtrapolateScalar[p, #] &, rds];
  MakeDataTable[MapThread[List, {ts, extraps}]]];

ExtrapolateDataTables[p_Integer, rdTb : {{_, DataTable[__]} ...}, {rMin_, rMax_}, opts___] :=
  ExtrapolateDataTables[p,Select[rdTb, #[[1]] >= rMin && #[[1]] <= rMax &], opts];


Options[AlignPhases] = {Continuous -> False};

AlignPhases[phaseTbs:{DataTable[__] ...}, t_, opts:OptionsPattern[]] :=
  Module[{phaseFns, refPhases, adjustments, adjusted,
          ranges, min, max, constTb},
    If[Length[phaseTbs] < 2, Return[phaseTbs]];

    ranges = Map[DataTableRange, phaseTbs];
    min = Apply[Max, Map[First, ranges]];
    max = Apply[Min, Map[Last, ranges]];
    If[t < min || t > max, Throw["Cannot align phases at " <> ToString[t] <> " because not all the inputs exist there"]];

    phaseFns = Map[Interpolation, phaseTbs];
    refPhases = Map[#[t]&, phaseFns];
    adjustments =
      If[!OptionValue[Continuous],
        Map[Round[#/(2. Pi)] 2.0 Pi &, refPhases],
        refPhases];

    constTb[a_, d_DataTable] := 
      MapData[a&,d];

    adjusted = MapThread[(#1-constTb[#2,#1]) &, {phaseTbs, adjustments}];
    Return[adjusted];
];

TortoiseCoordinate[r_, Madm_] :=
  r + 2 Madm Log[r/(2. Madm) - 1.];

ExtrapolateRadiatedQuantity[rdTb : {{_, DataTable[__]} ...}, OptionsPattern[]] :=
  Module[{radRange, rdTb2, rMin, rMax, rads, fTbs, alignPhaseAt, fTbsRes, applyFunction, 
          fTbsRes2, resWithr},

    radRange = OptionValue[RadiusRange];
    rdTb2 = If[radRange === All, 
      rdTb, 
      {rMin, rMax} = radRange; Select[rdTb, #[[1]] >= rMin && #[[1]] <= rMax &]];

    rads = Map[First, rdTb2];
    fTbs = Map[Last, rdTb2];

    If[OptionValue[UseTortoiseCoordinate],
      If[OptionValue[MassADM] === None,
        Throw["No mass given for tortoise coordinate.  Specify MassADM -> m in ExtrapolateRadiatedQuantity"]];
      fTbs = MapThread[ShiftDataTable[-TortoiseCoordinate[#1, OptionValue[MassADM]],#2]&, 
                                {rads, fTbs}],
      fTbs = MapThread[ShiftDataTable[-#1, #2] &, 
                                {rads, fTbs}]];

    alignPhaseAt = OptionValue[AlignPhaseAt];
    If[!(alignPhaseAt === None),
      fTbs = AlignPhases[fTbs, alignPhaseAt]];
                                
    fTbsRes = ResampleDataTables[fTbs];

    applyFunction = OptionValue[ApplyFunction];

    fTbsRes2 = If[!(applyFunction === None),
      Map[applyFunction, fTbsRes],
      fTbsRes];

    resWithr = MapThread[List, {rads, fTbsRes2}];
    ExtrapolateDataTables[OptionValue[ExtrapolationOrder], resWithr]];

ExtrapolateRadiatedQuantity[runName_String, reader_, opts:OptionsPattern[]] :=
  Module[{allRads, rads, fs, rdTb, rMin, rMax, mADM},
    allRads = ReadPsi4Radii[runName];
    rads = If[OptionValue[RadiusRange] === All,
      allRads,
      {rMin, rMax} = OptionValue[RadiusRange]; 
      Select[allRads, # >= rMin && # <= rMax &]];

    fs = Map[reader[runName, #] &, rads];
    rdTb = MapThread[List, {rads, fs}];

    mADM = If[OptionValue[MassADM] === None,
              ReadADMMass[runName],
              OptionValue[MassADM]];
    ExtrapolateRadiatedQuantity[rdTb, opts, MassADM -> mADM]];

ExtrapolatePsi4Phase[runName_String, l_, m_, opts:OptionsPattern[]] :=
  Module[{reader, tAlign},
    reader[run_, rad_] := Phase[ReadPsi4[run, l, m, rad]];
    tAlign = If[OptionValue[AlignPhaseAt] === None,
      100,
      OptionValue[AlignPhaseAt]];
    ExtrapolateRadiatedQuantity[runName, reader, opts, AlignPhaseAt -> tAlign]];

ExtrapolatePsi4Amplitude[runName_String, l_, m_, opts:OptionsPattern[]] :=
  Module[{reader},
    reader[run_, rad_] := rad Abs[ReadPsi4[run, l, m, rad]];
    ExtrapolateRadiatedQuantity[runName, reader, opts]];

DefineMemoFunction[ExtrapolatePsi4[runName_String, l_, m_, opts:OptionsPattern[]],
  Module[{phase,amp,psi4},
    phase = ExtrapolatePsi4Phase[runName, l, m, opts];
    amp = ExtrapolatePsi4Amplitude[runName, l, m, opts];
    psi4 = MapThreadData[#1 Exp[I #2] &, {amp, phase}];
    psi4]];

ExtrapolationError[f_, args__, opts:OptionsPattern[]] :=
  Module[{p, newOpts, fpp1, fp},
    p = OptionValue[ExtrapolationOrder];
    newOpts = DeleteCases[{opts}, ExtrapolationOrder -> _];

    fpp1 = f[args, Apply[Sequence,newOpts], ExtrapolationOrder -> p+1];
    fp = f[args, opts];

    If[OptionValue[ExtrapolationErrorRelative],
      (fpp1 - fp)/fp,
      (fpp1 - fp)]];

(*
ExtrapolateComplexRadiatedQuantity[runName_String, reader_, opts:OptionsPattern[]] :=
  Module[{phase,amp,psi4},
    ampReader[run_, rad_] :=
      Amp[reader[run_, rad]];
    phaseReader[run_, rad_] :=
      Phase[reader[run_, rad]];

    ampExt = ExtrapolateRadiatedQuantity[runName, ampReader, opts];
    phaseExt = ExtrapolateRadiatedQuantity[runName, phaseReader, opts];

    phase = reader[runName];
    amp = ExtrapolatePsi4Amplitude[runName, l, m, opts];
    psi4 = MapThreadData[#1 Exp[I #2] &, {amp, phase}];
    psi4];
*)

ffi[{f_, d_}, f0_] :=
 Module[{div},
  If[f > f0,
    div = 2 Pi I f;,
    div = 2 Pi I f0;
  ];
  {f, d/div}
];

StrainFromPsi4[psi4_DataTable, f0_?NumericQ] :=
 Module[{psi4f, dhf, hf, dh, h},
  psi4f = Fourier[psi4];
  dhf = Map[ffi[#, f0 / (2 Pi)] &, psi4f];
  hf  = Map[ffi[#, f0 / (2 Pi)] &, dhf];
  {h, dh} = InverseFourier /@ {hf, dhf};
  {h, dh}
]

StrainFromPsi4[psi4_DataTable, range:(_List | All)] :=
  Module[{range2},
    range2 = If[range === All,
     DataTableRange[psi4],
     range];
    StrainFromPsi4[psi4, range2[[1]], range2[[2]]]
  ];

StrainFromPsi4[psi4_DataTable, fitStart_, fitEnd_] :=
  Module[{tStart, tStep, tEnd, psi4Fn, ints, dataRealTb, modelReal, 
          fitReal, dataImagTb, modelImag, fitImag, hTb, ar, br, ai, bi},
    {tStart, tEnd} = DataTableRange[psi4];
    tStep = Spacing[psi4];
    psi4Fn = Interpolation[psi4, InterpolationOrder->4];
    ints = NDSolve[{psi4I'[t] == psi4Fn[t], psi4II'[t] == psi4I[t], 
      psi4I[tStart] == 0, psi4II[tStart] == 0},
     {psi4I, psi4II}, {t, tStart, tEnd}][[1]];

    dataRealTb = Table[{t, Re[psi4II[t]] /. ints}, {t, fitStart, fitEnd}];
    modelReal = ar + br t;
    fitReal = FindFit[dataRealTb, modelReal, {ar,br}, t];

    dataImagTb = Table[{t, Im[psi4II[t]] /. ints}, {t, fitStart, fitEnd}];
    modelImag = ai + bi t;
    fitImag = FindFit[dataImagTb, modelImag, {ai,bi}, t];

    hTb = Table[{t, (psi4II/.ints)[t] - 
      (modelReal /. fitReal)  - I (modelImag /. fitImag) }, 
      {t, tStart, tEnd, tStep}];
    hDotTb = Table[{t, (psi4I/.ints)[t]   - 
      (br /. fitReal) - I (bi /. fitImag)  }, 
      {t, tStart, tEnd, tStep}];
    Return[{MakeDataTable[hTb], MakeDataTable[hDotTb]}];
];

zeroAfter[l_, n_] :=
 Module[{len},
  len = Length[l];
  Join[Take[l, n], Table[0, {i, n + 1, len}]]];

(* FilterDCT[f_List, nModes_Integer] := *)
(*  Module[{times, data, dataDCT, dataFilDCT, dataFil, fFil}, *)
(*   times = Map[First, f]; *)
(*   data = Map[Last, f]; *)
(*   dataDCT = FourierDCT[data, 2]; *)
(*   dataFilDCT = zeroAfter[dataDCT, nModes]; *)
(*   dataFil = FourierDCT[dataFilDCT, 3]; *)
(*   fFil = MapThread[List, {times, dataFil}]; *)
(*   Return[fFil]; *)
(*   ]; *)

(* PartitionTable[d_DataTable, {tMin_?NumberQ, tMax_?NumberQ}] := *)
(*  Module[{before, middle, after, t1, t2}, *)
(*   {t1,t2} = DataTableRange[d]; *)
(*   before = DataTableInterval[d, t1, tMin]; *)
(*   middle = DataTableInterval[d, tMin, tMax]; *)
(*   after = DataTableInterval[d, tMax, t2]; *)
(*   Return[{before, middle, after}] *)
(*   ]; *)

(* FilterDCT[f_List, nModes_Integer, *)
(*    range1 : {tMin1_?NumberQ, tMax1_?NumberQ}, *)
(*    range2 : {tMin2_?NumberQ, tMax2_?NumberQ}] := *)
(*   Module[{filtered, t1, t2, t3}, *)
(*    filtered =  *)
(*     DataTableInterval[FilterDCT[DataTableInterval[f, range1], nModes], range2]; *)
(*    {t1, t2, t3} = PartitionTable[f, range2]; *)
(*    Return[Join[t1, filtered, t3]]]; *)

TableRange[t_List, tStart_?NumberQ, tEnd_?NumberQ] :=
  Select[t, 
   (#[[1]] >= tStart && #[[1]] < tEnd) &];

TableRange[t_List, range_List] :=
  TableRange[t,range[[1]],range[[2]]];

FilterDCT[f_List, nModes_Integer] :=
 Module[{times, data, dataDCT, dataFilDCT, dataFil, fFil},
  times = Map[First, f];
  data = Map[Last, f];
  dataDCT = FourierDCT[data, 2];
  dataFilDCT = zeroAfter[dataDCT, nModes];
  dataFil = FourierDCT[dataFilDCT, 3];
  fFil = MapThread[List, {times, dataFil}];
  Return[fFil];
  ];


PartitionTable[t_List, {tMin_?NumberQ, tMax_?NumberQ}] :=
 Module[{before, middle, after},
  before = TableRange[t, First[t][[1]], tMin];
  middle = TableRange[t, tMin, tMax];
  after = TableRange[t, tMax, Last[t][[1]] + 1];
  Return[{before, middle, after}]
  ];

FilterDCT[f_DataTable,  nModes_Integer,
   range1 : {tMin1_?NumberQ, tMax1_?NumberQ},
   range2 : {tMin2_?NumberQ, tMax2_?NumberQ}] :=
  Module[{},
    MakeDataTable[FilterDCT[ToList[f], nModes, range1, range2]]
  ];

FilterDCT[f_List, nModes_Integer,
   range1 : {tMin1_?NumberQ, tMax1_?NumberQ},
   range2 : {tMin2_?NumberQ, tMax2_?NumberQ}] :=
  Module[{filtered, t1, t2, t3},
   filtered = 
    TableRange[FilterDCT[TableRange[f, range1], nModes], range2];
   {t1, t2, t3} = PartitionTable[f, range2];
   Return[Join[t1, filtered, t3]]];

(*--------------------------------------------------------------------
  Model fitting
  --------------------------------------------------------------------*)

FitFunction[d_, f_, paramSpecs_] :=
  FitFunction[d, f, paramSpecs, FindMinimum, Automatic];

(* Take a table d of data from a simulation and work out values of a set
   of parameters ps which cause the data to match best to a given
   function f over a given interval tMin to tMax. *)

FitFunction[d_List, f_, paramSpecs_, method_, subMethod_] :=
  Module[{squareDiff,lastFitMessageTime, it, pList, pList2, p2, p3, paramSpecs2, fit, fit2},
    squareDiff[params__?NumberQ] :=
      Module[{fSoln, diffs, sqDiff, interval},
(*        FitFunctionParamPath = Append[FitFunctionParamPath, {it, params}];*)
        fSoln = f[params];
        If[fSoln === Indeterminate, Return[100]];
        interval = Last[d][[1]] - First[d][[1]];
        diffs = Map[#[[2]] - fSoln[#[[1]]] &, d];
        Map[If[!NumberQ[#], Return[100]] &, diffs];
        sqDiff = Sqrt[diffs . diffs / Length[diffs]];
(*        sqDiff = (diffs . diffs / Length[diffs]);*)
        If[SessionTime[] - lastFitMessageTime > 5,
          lastFitMessageTime = SessionTime[]; 
          Print[ToString[it]<> " "<>ToString[sqDiff,CForm]<>" "<>ToString[Map[ToString[#,CForm] &, {params}]]]];
        it++;
        Return[sqDiff]];

    pList = Map[First, paramSpecs];
(*    Print["pList = ",  pList];*)

    pList2 = Map[Unique, pList];
    p2 = Map[#[[2]]&, paramSpecs];
    p3 = Map[#[[3]]&, paramSpecs];

    paramSpecs2 = MapThread[List, {pList2, p2, p3}];

    lastFitMessageTime = SessionTime[];
    it = 0;
(*    FitFunctionParamPath = {};*)

    If[method === FindMinimum,
      fit = FindMinimum[Apply[squareDiff, pList2], paramSpecs2, 
               (* AccuracyGoal -> 2,  PrecisionGoal->2, *)  Method-> PrincipalAxis   ],
      fit = Quiet[NMinimize[Apply[squareDiff, pList2], paramSpecs2, 
                  (* AccuracyGoal -> Infinity, PrecisionGoal->3,*)   Method->subMethod], {NMinimize::cvmit}]
    ];

    If[Head[fit] === FindMinimum,
      Throw["FindMinimum failed"]];

    If[Head[fit] === NMinimize,
      Throw["NMinimize failed"]];

    fit2 = fit /. MapThread[(#1 -> #2) &, {pList2, pList}];

    Print["fit = ", fit2];
    Return[fit2];
  ];

LocateMaximumPoint[d_DataTable] :=
 Module[{tMax, fMax, l, maxFind, t1, t2},
  l = ToList[d];
  {t1, t2} = DataTableRange[d];
  fMax = -Infinity;
  maxFind[{t_, f_}] :=
   If[f > fMax, fMax = f; tMax = t];
  Scan[maxFind, l];
  Return[tMax]];

LocateMaximum[d_DataTable] :=
 Module[{tMax, fMax, l, maxFind, fn, t1, t2, t, tMax2},
  l = ToList[d];
  {t1, t2} = DataTableRange[d];
  fMax = -Infinity;
  maxFind[{time_, f_}] :=
   If[f > fMax, fMax = f; tMax = time];
  Scan[maxFind, l];
  fn = Interpolation[d];
  tMax2 = 
   t /. FindMaximum[{fn[t], {t > tMax - 50, t < tMax + 50}}, {t, 
       tMax}][[2]];
  Return[tMax2]];

GridSpacingOnLevel[runName_, l_] :=
  Module[{h0},
    h0 = ToExpression[LookupParameter[runName, "CoordBase::dx",
      LookupParameter[runName, "Coordinates::h_cartesian"]]];
    h0 / 2^l
  ];

ReadCoarseGridSpacing[runName_] :=
  Module[{h0},
    h0 = ToExpression[LookupParameter[runName, "CoordBase::dx",
      LookupParameter[runName, "Coordinates::h_cartesian", $Failed]]]
  ];

ReadAngularPoints[runName_] :=
  Module[{},
    ToExpression[LookupParameter[runName, "Coordinates::n_angular"]]
  ];

ReadInnerBoundary[runName_] :=
  Module[{},
    ToExpression[LookupParameter[runName, "Coordinates::sphere_inner_radius"]]
  ];

ReadOuterBoundary[runName_] :=
  Module[{},
    ToExpression[LookupParameter[runName, "Coordinates::sphere_outer_radius"]]
  ];

FinestGridSpacing[runName_] :=
  GridSpacingOnLevel[runName, Max[RefinementLevels[runName]]];

BoxRadiiOnLevel[runName_, l_] :=
  Module[{params},
    If[l == 0, Return[{ToExpression[LookupParameter[runName, "CoordBase::xmax"]]}]];
    params = FindParameters[runName, "regridboxes::centre_*_radius" ~~ (Whitespace | "") ~~ "["<>ToString[l]<>"]"];

    If[params === {},
      params = FindParameters[runName, "carpetregrid2::radius_*" ~~ (Whitespace | "") ~~ "["<>ToString[l]<>"]"]];

    Map[ToExpression[LookupParameter[runName, #]] &, params]
  ];

BoxRadiiForCentre[runName_, c_] :=
  Module[{params},
    (* If[l == 0, Return[{ToExpression[LookupParameter[runName, "CoordBase::xmax"]]}]]; *)
    params = FindParameters[runName, "carpetregrid2::radius_"<>ToString[c] ~~ (Whitespace | "") ~~ "[*]"];

    Prepend[Map[{ToExpression@StringReplace[#, __ ~~ "[" ~~ n__ ~~ "]" -> n],
         ToExpression[LookupParameter[runName, #]]} &, params], {0, ReadOuterBoundary[runName]}]
  ];

CountRefinementLevels[runName_String] :=
  ToExpression[LookupParameter[runName, "Carpet::max_refinement_levels"]];

CountRefinementLevels[runName_String, i_] :=
  ToExpression[LookupParameter[runName,
                               "CarpetRegrid2::num_levels_"<>ToString[i]]];

RefinementLevels[runName_String] :=
  Table[l, {l, 0, CountRefinementLevels[runName]-1}];

RadialPoints[runName_String, level_Integer] :=
   Round[BoxRadiiOnLevel[runName, level] / GridSpacingOnLevel[runName, level]];

GridStructure[runName_String] :=
  Map[{#, BoxRadiiOnLevel[runName, #],
       GridSpacingOnLevel[runName, #], 
       RadialPoints[runName, #],
       CourantFactorOnLevel[runName, #],
       LevelExistsEvery[runName, #]} &, RefinementLevels[runName]];

TimeRefinementFactors[runName_String] :=
  Module[{s1, s2, s3s, facs},
    s1 = LookupParameter[runName, "Carpet::time_refinement_factors"];
    s2 = StringCases[s1, ("[" ~~ facs__ ~~ "]") -> facs][[1]];
    s3s = StringSplit[s2, ","];
    facs = Map[ToExpression, s3s];
    Return[facs];
  ];

TimeRefinementFactor[runName_String, level_] :=
  TimeRefinementFactors[runName][[level+1]];

CourantFactorOnLevel[runName_String, level_] :=
  Module[{dtfac, trf, srf},
    dtfac = ToExpression[LookupParameter[runName, "Time::dtfac"]];
    trf = TimeRefinementFactor[runName, level];
    srf = 2^level;
    Return[dtfac * srf/trf];
  ];

LevelExistsEvery[runName_String, level_Integer] :=
  TimeRefinementFactor[runName, CountRefinementLevels[runName]-1] / 
    TimeRefinementFactor[runName, level];

RunCost[length_, speed_, nProcs_] :=
 {CPUHours -> nProcs length/speed // N,
  WallTimeDays -> length/speed/24 // N};


AbsOfPhase[d_DataTable, {t1_, t2_}] :=
  FunctionOfPhase[Abs[d], Phase[d], {t1,t2}];

FunctionOfPhase[d_DataTable, p_DataTable, {t1_, t2_}, dp_: 0.01] :=
 Module[{phiOft, tOfphi, tOfphiFn, phiMin, phiMax, dOftFn, dOfphiTb},
  phiOft = ToList[DataTableInterval[p,{t1,t2}]];
  tOfphi = Map[Reverse, phiOft];
  tOfphiFn = Interpolation[tOfphi];
  {phiMin,phiMax} = Sort[{First[tOfphi][[1]], Last[tOfphi][[1]]}];
  dOftFn = Interpolation[d];
  dOfphiTb = 
   Table[{phi, dOftFn[tOfphiFn[phi]]}, {phi, phiMin, phiMax, dp}];
  AddAttributes[MakeDataTable[dOfphiTb], ListAttributes[d]]];

PhaseOfFrequency[psi4_] :=
 Module[{phase1, freq1, phaseOfFreq1},
  phase1 = -Phase[psi4];
  freq1 = NDerivative[phase1];
  phaseOfFreq1 = 
   MakeDataTable@
    MapThread[List, 
     Map[DepVar, IntersectDataTables[{freq1, phase1}]]]
  ];

ShiftPhase[d_DataTable, dph_] :=
  MapData[Exp[I dph] # &, d];

PercentageDifference[ap_,bp_] :=
  Module[{a,b},
    {a,b} = Sort[{ap,bp}];
    100 Abs[a-b]/a];

NumCycles[sim_String, start_] := Module[{psi4, rads, r},
  rads = ReadPsi4Radii[sim];
  r = If[rads === {}, 0, First[rads]];
  psi4 = ReadPsi4[sim, 2, 2, r];
  NumCycles[psi4, start]
  ]

NumCycles[psi4_DataTable, start_] :=
 Module[{mergertime, phasei, cycles},
  mergertime = LocateMaximum[Abs[psi4]];
  phasei = Interpolation[Phase[psi4]];
  cycles = (phasei[start] - phasei[mergertime])/(2 \[Pi] );
  cycles
  ]

ReadHamiltonianConstraintNorm[run_] :=
  ReadColumnFile[run, "ctgconstraints::hamiltonian_constraint.norm2.asc", {2,3}];

ReadWaveformFile[file_] :=
  MakeDataTable[Select[Map[{#[[1]],#[[2]]+I #[[3]]}&, Import[file,"Table"]], NumberQ[#[[2]]]&]];

AlignMaxima[ds_List] :=
  Module[{maxima},
   maxima = Map[LocateMaximum, ds];
   MapThread[ShiftDataTable[-#1, #2] &, {maxima, ds}]];

AlignMaximaOfAbs[ds_List] :=
  Module[{maxima},
   maxima = Map[LocateMaximum, Abs /@ ds];
   MapThread[ShiftDataTable[-#1, #2] &, {maxima, ds}]];

ReadPunctureADMMassesFromFiles[files_List] :=
  Module[{lines, massLines, file, plusLine, minusLine, mPlus, mMinus},
    If[files === {}, Throw["Cannot find puncture ADM masses"]];
    file = files[[1]];
    lines = ReadList[file, String];
    massLines = Select[lines, StringMatchQ[#, "INFO (TwoPunctures):   M_adm_" ~~ _ ~~ " = " ~~ __] &];
    If[massLines === {}, Return[ReadPunctureADMMassesFromFiles[Drop[files, 1]]]];

    plusLine = Select[massLines, StringMatchQ[#, "INFO (TwoPunctures):   M_adm_+ = " ~~ __] &][[1]];
    minusLine = Select[massLines, StringMatchQ[#, "INFO (TwoPunctures):   M_adm_- = " ~~ __] &][[1]];

    mPlus  = ToExpression[First@StringCases[plusLine, "INFO (TwoPunctures):   M_adm_+ = " ~~ x__ -> x]];
    mMinus = ToExpression[First@StringCases[minusLine, "INFO (TwoPunctures):   M_adm_- = " ~~ x__ -> x]];

    Return[{mPlus, mMinus}];
    ];

DefineMemoFunction[ReadPunctureADMMasses[run_String],
  Module[{},
    stdoutFiles = StandardOutputOfRun[run];
    ReadPunctureADMMassesFromFiles[stdoutFiles]]];

DefineMemoFunction[ReadPunctureADMMasses2[run_String],
  ToExpression/@{LookupParameter[run, "TwoPunctures::target_M_plus"], 
   LookupParameter[run, "TwoPunctures::target_M_minus"]}];

ReadFineTimeStep[run_] :=
 Module[{pairs, it1, it2, t1, t2, dtFine},
  pairs = 
   Take[ReadColumnFile[run, "puncturetracker::pt_loc..asc", {1, 9}], 
    2];
  {{it1, t1}, {it2, t2}} = pairs;
  dtFine = (t2 - t1)/(it2 - it1)];

ReadTimeRange[run_] :=
  Module[{pairs, first, last},
   pairs = ReadColumnFile[run, "puncturetracker::pt_loc..asc", {1, 9}];
   first = pairs[[1, 2]];
   last = pairs[[-1, 2]];
   {first, last}];


Options[FitEcc] = {ReturnValue -> FittedFunction};

FitEcc[sep_, int : {t1_, t2_}, opts : OptionsPattern[]] :=
 Module[{eccModel, eccPars, eccData, eccFit, eccSepFitted, t, a, e, phi, b, n},
  eccModel = a (1 - e Cos[n t + phi]) + 1/2 e^2 (1 - Cos[2 n t]) + b t;
  eccPars = {{a, 10,11}, {b, -0.001, 0}, {e, -0.01, 0.01}, {n, 0.01, 0.1}, {phi, 0, Pi}};
  eccData = DataTableInterval[sep, int];
  eccFit = 
   FindFit[ToList@eccData, eccModel, eccPars, t, 
    MaxIterations -> 1000];
  eccSepFitted = eccModel /. eccFit;
  Switch[OptionValue[ReturnValue],
   Eccentricity, e /. eccFit,
   FittedFunction, Function[tp,Evaluate[eccSepFitted/.t->tp]],
   FitParameters, eccFit,
   _, Throw["Unknown option given to FitEcc"]]
  ];

Options[FitEccOm] = {ReturnValue -> FittedFunction};

FitEccOm[om_, int : {t1_, t2_}, opts : OptionsPattern[]] :=
 Module[{eccModel, eccPars, eccData, eccFit, eccSepFitted, t, a, e, n, phi, b},
  eccModel = a (1 + 2 e Cos[n t + phi] + 5/2 e^2 Cos[2 n t + phi]) + b t;
  eccPars = {{a, 0.015, 0.022}, {b, 6*10^-6,6.2*10^-6}, {e, -0.01, 0.01}, {n, 0.02, 0.021}, {phi, 0, Pi}};
  eccData = DataTableInterval[om, int];
  eccFit =
   FindFit[ToList@eccData, eccModel, eccPars, t,
    MaxIterations -> 1000];
  eccSepFitted = eccModel /. eccFit;
  Switch[OptionValue[ReturnValue],
   Eccentricity, e /. eccFit,
   FittedFunction, Function[tp,Evaluate[eccSepFitted/.t->tp]],
   FitParameters, eccFit,
   _, Throw["Unknown option given to FitEcc"]]
  ];

ToFixedWidth[n_Integer, width_Integer] :=
  StringJoin[PadLeft[Characters[ToString[n]], width, "0"]];

(* Read data output by the standalone TwoPunctures code by Marcus Ansorg *)
ReadTwoPuncturesData[file_String, col_] :=
 Module[{lines, lines2, table},
  lines = Import[file, "Table"];
  lines2 = Drop[Select[lines, Length[#] != 0 && First[#] != "#" &], 0];
  table = Map[Append[Take[#, 3], #[[col]]] &, lines2];
  TableToDataRegion[table]];

(* Initial data *)

TotalMass[run_] :=
 Plus @@ ReadPunctureADMMasses2[run];

DefineMemoFunction[InitialSpinAngularMomentum[run_],
 Module[{sp, sm, m},
  m = Plus @@ ReadPunctureADMMasses2[run];
  sp = Table[
     ToExpression@
      LookupParameter[run, 
       "TwoPunctures::par_s_plus[" <> ToString[i] <> "]", "0"], {i, 0,
       2}]/m^2;
  sm = Table[
     ToExpression@
      LookupParameter[run, 
       "TwoPunctures::par_s_minus[" <> ToString[i] <> "]", "0"], {i, 
      0, 2}]/m^2;
  sp + sm]];

InitialLinearMomentum[run_, idx_] :=
  Module[{suffix},
    suffix = If[idx == 0, "plus", "minus"];
    Table[
      ToExpression@
        LookupParameter[run, "TwoPunctures::par_P_"<>suffix<>"["<>ToString@d<>"]", 0],
      {d,0,2}]];

InitialOrbitalAngularMomentum[run_] :=
 Module[{xp, xm, pyp, pym, initialL, m},
  m = Plus @@ ReadPunctureADMMasses2[run];
  xp = (ToExpression@LookupParameter[run, "TwoPunctures::par_b"] + 
      ToExpression@
       LookupParameter[run, "TwoPunctures::center_offset[0]"])/m;
  xm = (-ToExpression@LookupParameter[run, "TwoPunctures::par_b"] + 
      ToExpression@
       LookupParameter[run, "TwoPunctures::center_offset[0]"])/m;
  pyp = (ToExpression@
      LookupParameter[run, "TwoPunctures::par_P_plus[1]"])/m;
  pym = (ToExpression@
      LookupParameter[run, "TwoPunctures::par_P_minus[1]"])/m;
  initialL = {0, 0, xp*pyp + xm*pym}
  ];

InitialAngularMomentum[run_] :=
  InitialOrbitalAngularMomentum[run] + InitialSpinAngularMomentum[run];

InitialSpin[run_, i_] :=
 First@DepVar@SpinAngle[run, i];

SpinAngle[run_, idx_] :=
  MapData[AnglesOfVector[#][[2]] &, ReadIHSpin[run, idx]];

InitialSpinAngle[run_, i_] :=
 First@DepVar@SpinAngle[run, i];

InitialSpinAngle[run_] :=
 Module[{S0, S1},
  S0 = First@DepVar@ReadIHSpin[run, 0];
  S1 = First@DepVar@ReadIHSpin[run, 1];
  If[Norm@S0 > Norm@S1,
   AnglesOfVector[S0][[2]],
   AnglesOfVector[S1][[2]]]];

InitialSpin[run_] :=
 Module[{S0, S1},
  S0 = First@DepVar@ReadIHSpin[run, 0];
  S1 = First@DepVar@ReadIHSpin[run, 1];
  If[Norm@S0 > Norm@S1,
   S0,
   S1]];

DefineMemoFunction[InitialDimensionlessSpin[run_],
 Module[{S0, S1, mp, mm},
  S0 = First@DepVar@ReadIHSpin[run, 0];
  S1 = First@DepVar@ReadIHSpin[run, 1];
  {mp, mm} = ReadPunctureADMMasses2[run];
  If[Norm@S0 > Norm@S1,
   S0/mp^2,
   S1/mm^2]]];

MassRatio[run_] :=
 Module[{mp, mm},
  {mp, mm} = ReadPunctureADMMasses2[run];
  Return[If[mp < mm, mp/mm, mm/mp]]];

SymmetricMassRatio[run_] :=
 Module[{q = MassRatio[run]},
  q/(1 + q)^2];

DefineMemoFunction[InitialSeparation[run_],
  First@DepVar@ReadBHSeparation[run]];


End[];

EndPackage[];
