
(* A package for dealing with numerical relativity data *)

BeginPackage["NR`", {"DataTable`", "Memo`", "RunFiles`", "Timers`"}];

ReadPsi4;
ReadMinTrackerCoordinates;
ReadMinTrackerCoordinate;
ReadMinTrackerRadius;
ReadMinTrackerPhase;
ReadMinTrackerTrajectory;
ReadMinTrackerTrajectories;
ReadMinTrackerSpeed;
ConvergenceMultiplier;
LoadConvergenceSeries;
RescaledErrors;
ConvergenceRate;
ExtrapolateScalarFull;
ExtrapolateScalar;
ExtrapolatedValue;
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
StrainFromPsi4;
ResolutionCode;
RichardsonExtrapolate;
RichardsonExtrapolate3;
FitFunction;
RichardsonExtrapolationError;
ResName;
NPoints;
ReadRunSpeed;
ReadPsi4Radii;
LocateMaximum;
LookupParameter;
FindParameters;
GridSpacingOnLevel;
BoxRadiiOnLevel;
RefinementLevels;
GridStructure;
FinestGridSpacing;
TimeRefinementFactors;
CourantFactorOnLevel;
LevelExistsEvery;
RadialPoints;
RunCost::usage = "RunCost[length, speed, nprocs] returns information about the cost of a run.";
CPUHours;
WallTimeDays;
ReadMemory;
RunName;
ReadCPUHours;
ReadWalltimeHours;
ReadCores;
ReadIHSpin;
ReadIHSpinX;
ReadIHSpinY;
ReadIHSpinPhase;
ReadAHMass;
ReadAHRadius;
ReadPsi4Modes;
ExportWaveform;
ExportBHCoords;
ExportBHRelativeCoords;

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

Begin["`Private`"];

RunDirectory = Global`RunDirectory;

(* Simulation data *)

DefineMemoFunction[ReadPsi4[runName_String, l_?NumberQ, m_?NumberQ, rad_?NumberQ],
  Module[{fileName, threeCols, psi4},
    fileName = "Ylm_WEYLSCAL4::Psi4r_l" <>
             ToString[l] <> "_m" <> ToString[m] <> "_r" <> 
             ToString[rad] <> ".00.asc";
    threeCols = ReadColumnFile[runName, fileName, {1,2,3}];
    psi4 = Map[{#[[1]], #[[2]] + I #[[3]]}&, threeCols];
    Return[AddAttribute[MakeDataTable[psi4], RunName -> runName]]]];

ReadMinTrackerCoordinates[runName_String, tracker_Integer] :=
  Module[{list, list2},
    list = ReadColumnFile[runName, "MinTracker"<>ToString[tracker]<>".asc", {2,3,4,5}];
    list2 = Map[{#[[1]], {#[[2]], #[[3]], #[[4]]}} &, list];
    Return[MakeDataTable[list2, {RunName -> runName}]]];

ReadMinTrackerCoordinate[runName_String, tracker_Integer, coord_Integer] :=
  Module[{coords},
    coords = ReadMinTrackerCoordinates[runName, tracker];
    MapData[#[[coord]]&, coords]];

ReadMinTrackerTrajectory[runName_String, tracker_Integer] :=
  Map[Take[Last[#], 2] &, 
    ToList[ReadMinTrackerCoordinates[runName, tracker]]];

ReadMinTrackerTrajectory[runName_String] :=
  Module[{coords1,coords2,rel},
    coords1 = ReadMinTrackerCoordinates[runName, 0];
    coords2 = ReadMinTrackerCoordinates[runName, 1];
    rel = coords1 - coords2;
    Map[Take[Last[#], 2] &, ToList[rel]]];

ReadMinTrackerTrajectories[runName_String] :=
  {ReadMinTrackerTrajectory[runName, 0], ReadMinTrackerTrajectory[runName, 1]};

ReadMinTrackerVelocity[runName_String, tracker_Integer] :=
  Module[{x,v},
    x = Table[ReadMinTrackerCoordinate[runName, tracker, dir], {dir, 1 3}];
    v = Map[NDerivative, x];
    MapThreadData[{#1,#2,#3}&, v]];

ReadMinTrackerVelocity[runName_String] :=
  ReadMinTrackerVelocity[runName, 0] - ReadMinTrackerVelocity[runName, 1];

ReadMinTrackerSpeed[runName_String] :=
  MapData[Norm, ReadMinTrackerVelocity[runName]];

ReadADMMass[runName_String] :=
  ReadList[FileInRun[runName, "ADM_mass_tot.asc"], Real][[1]];

ReadPsi4Radii[runName_] :=
  Module[{names, radiusFromFileName, radii},
    names = FindRunFilesFromPattern[runName, 
      "Ylm_WEYLSCAL4::Psi4r_l*_m*_r*.asc"];
    radiusFromFileName[name_] :=
      Round[ToExpression[
        StringReplace[name,
          "Ylm_WEYLSCAL4::Psi4r_l" ~~ __ ~~ "m" ~~ __ ~~ "r"
          ~~ x__ ~~ ".asc" -> x]]];
    radii = Union[Map[radiusFromFileName, names]]];

ReadRunSpeed[runName_] := 
 MakeDataTable[ReadColumnFile[runName, "runstats.asc", {2, 4}]];

ReadCPUHours[runName_] := 
 Last[ReadColumnFile[runName, "runstats.asc", {6}]][[1]];

ReadWalltimeHours[runName_] := 
  ReadCPUHours[runName] / ReadCores[runName] //N;

ReadMemory[runName_] :=
 MakeDataTable[ReadColumnFile[runName, "MemStats0000.asc", {1, 2}]];

ReadCores[runName_] :=
  ReadList[FileNameJoin[{RunDirectory, runName, "output-0000", "PROCS"}], Number][[1]];

(*--------------------------------------------------------------------
  Data conversion 
  --------------------------------------------------------------------*)

ReadMinTrackerRadius[runName_String] :=
  Module[{x0, x1, rad, l},
    x0 = ReadMinTrackerCoordinates[runName, 0];
    x1 = ReadMinTrackerCoordinates[runName, 1];
    l = Min[Length[x0],Length[x1]];
    rad = MapThreadData[Norm[#1-#2] &, {Take[x0,l], Take[x1,l]}];
    Return[rad];
  ];

ReadMinTrackerPhase[runName_String] :=
  Module[{x0, x1, rad,l},
    x0 = ReadMinTrackerCoordinates[runName, 0];
    x1 = ReadMinTrackerCoordinates[runName, 1];
    l = Min[Length[x0],Length[x1]];
    xyTrans = MapThreadData[Take[#1-#2,2] &, {Take[x0,l], Take[x1,l]}]; (* Project into xy plane *)
    Return[Phase[xyTrans]];
  ];


(*--------------------------------------------------------------------
  Convergence
  --------------------------------------------------------------------*)
ResolutionCode[n_Integer] := 
 Module[{}, 
  If[! (Mod[n, 4] === 0), 
   Throw["Number of points must be a multiple of 4"]];
  If[n < 16, Throw["Number of points must be at least 16"]];
  If[n > 116, Throw["Number of points must be less than 116"]];
  Return[FromCharacterCode[n/4 - 4 + 97]]];

ResName[s_String, n_] := s <> ResolutionCode[n];

ConvergenceMultiplier[{h1_, h2_, h3_}, p_] :=
  Module[{eq, eqs, f, f0, f1},
    eq = f[h] == f0 + f1 h^p;
    eqs = {eq /. h -> h1, eq /. h -> h2, eq /. h -> h3};
    Return[C /. Solve[
     f[h1] - f[h2] == C (f[h2] - f[h3]) 
       /. (eqs /. Equal -> Rule), C][[1]] // N]];

Options[LoadConvergenceSeries] = {Downsample -> False, Interpolate -> False};

LoadConvergenceSeries[runBase_,ns:{n1_,n2_,n3_},reader_,namer_, opts___] :=
  Module[{downsample = Downsample /. {opts} /. Options[LoadConvergenceSeries],
          interpolate = Interpolate /. {opts} /. Options[LoadConvergenceSeries],
          fs,len,tables},
    
    fs = Map[reader, Map[runBase <> namer[#] &, ns]];
    dts = Map[Spacing, fs];
(*    Print[dts];*)
    If[!(Length[Union[dts]] === 1) && !downsample && !interpolate,
       downsample = True;
(*       Print["Automatically downsampling"] *) ];

    If[downsample,
      gcd = Apply[GCD, ns];
(*      Print[gcd];*)
      dsFacs = Map[#/gcd&, ns];
(*      Print[dsFacs];*)
      fs = MapThread[Downsample[#1, #2] &, {fs, dsFacs}]];

    If[interpolate,
      dt = Apply[Min, dts];
      fs = Map[MakeInterpolatingDataTable[#, dt] &, fs]];

    len = Apply[Min, Map[Length, fs]];
    tables = Map[Take[#, len] &, fs];
    tsWithAttrs = MapThread[AddAttribute[#1, NPoints->#2] &, {tables, ns}];

    Return[tsWithAttrs];
  ];

RescaledErrors[p_, ds:List[DataTable[__]..]] :=
  Module[{d1, d2, d3, ns, hs, d12, d23, cm, ds2, dts},
    dts = Map[Spacing, ds];
    ranges = Map[DataTableRange, ds];
    If[!Apply[Equal, dts] || !Apply[Equal,ranges], 
      ds2 = ResampleDataTables[ds],
      ds2 = ds];
    {d1, d2, d3} = ds2;
    ns = Map[ReadAttribute[#, NPoints] &, ds2];
    hs = Map[1/#&, ns];
    d12 = d1 - d2;
    d23 = d2 - d3;
    cm = ConvergenceMultiplier[hs, p];
    Return[{d12/cm, d23}];
  ];

ConvergenceRateEquations = Table[CRF[i] == CRf0 + CRf1 CRh[i]^CRp, {i, 1, 3}];
ConvergenceRatePEquation = Eliminate[ConvergenceRateEquations, {CRf0, CRf1}];

RichardsonExtrapolationEquation = Eliminate[Take[ConvergenceRateEquations, 2], {CRf1}];
RichardExtrapolationExpression = CRf0 /. Solve[RichardsonExtrapolationEquation, CRf0][[1]];


ConvergenceRateEquations3 = Table[CRF[i] == CRf0 + CRf1 CRh[i]^CRp + CRf2 CRh[i]^(CRp+1), {i, 1, 3}];

RichardsonExtrapolationEquation3 = Eliminate[ConvergenceRateEquations3, {CRf1, CRf2}];
RichardExtrapolationExpression3 = CRf0 /. Solve[RichardsonExtrapolationEquation3, CRf0][[1]];

ConvergenceRate[{F1_?NumberQ, F2_, F3_}, {h1_, h2_, h3_}] := 
 Module[{rateEq, rate}, 
  rateEq = 
   ConvergenceRatePEquation /. {CRF[1] -> F1, CRF[2] -> F2, 
      CRF[3] -> F3, CRh[1] -> h1, CRh[2] -> h2, CRh[3] -> h3} // N;
  rate = Check[CRp /. FindRoot[rateEq, {CRp, 1, 10}], None, FindRoot::lstol];
  If[rate < 0.1 || rate > 9.9, Return[None], Return[rate]]];

ConvergenceRateSlow[fs:{f1_, f2_, f3_}, hs:{h1_, h2_, h3_}] :=
  Module[{eq, eqs, el, a0, a1},
    eq = f == a0 + a1 h^p;
    eqs = MapThread[eq /.{f -> #1, h -> #2} &, {fs, hs}];
    el = Eliminate[eqs, {a0, a1}];
    p /. FindRoot[el, {p, 1, 10}]];

ConvergenceRate[ds:{DataTable[__]..}] :=
  Module[{hs,dts,ds2},
    dts = Map[Spacing, ds];
    ranges = Map[DataTableRange, ds];
    If[!Apply[Equal, dts] || !Apply[Equal,ranges], 
      ds2 = ResampleDataTables[ds],
      ds2 = ds];
    hs = Map[1/ReadAttribute[#, NPoints] &, ds2];
    MapThreadData[ConvergenceRate[{#1, #2, #3}, hs] &, ds2]];

RichardsonExtrapolate[F1_, F2_, h1_, h2_, p_] :=
  Module[{},
    Return[RichardExtrapolationExpression /. {CRp -> p, CRF[1] -> F1, CRF[2] -> F2, 
      CRh[1] -> h1, CRh[2] -> h2}//N];
  ];

RichardsonExtrapolate[{F1_, F2_}, {h1_, h2_}, p_] :=
  Module[{},
    Return[RichardExtrapolationExpression /. {CRp -> p, CRF[1] -> F1, CRF[2] -> F2, 
      CRh[1] -> h1, CRh[2] -> h2}//N];
  ];


RichardsonExtrapolate[ds:{d1_DataTable, d2_DataTable}, p_] :=
  Module[{ns, hs, dts, ranges},
    dts = Map[Spacing, ds];
    ranges = Map[DataTableRange, ds];
    If[!Apply[Equal, dts] || !Apply[Equal,ranges], 
      ds2 = ResampleDataTables[ds],
      ds2 = ds];

    ns = Map[ReadAttribute[#, NPoints] &, ds2];
    hs = Map[1/#&, ns];
    Return[MapThreadData[RichardsonExtrapolate[#1,#2, hs[[1]], hs[[2]], p] &, ds2]]];

RichardsonExtrapolate[ds:{d1_DataTable, d2_DataTable, d3_DataTable}, p_] :=
  RichardsonExtrapolate[{d2,d3},p];

RichardsonExtrapolate[{F1_, F2_, F3_}, {h1_, h2_, h3_}, p_] :=
  RichardsonExtrapolate[F2, F3, h2, h3, p];

RichardsonExtrapolate3[F1_, F2_, F3_, h1_, h2_, h3_, p_] :=
  Module[{},
    Return[RichardExtrapolationExpression3 /. {CRp -> p, CRF[1] -> F1, CRF[2] -> F2, CRF[3] -> F3,
      CRh[1] -> h1, CRh[2] -> h2, CRh[3] -> h3}//N];
  ];

RichardsonExtrapolate3[ds:{d1_DataTable, d2_DataTable, d3_DataTable}, p_] :=
  Module[{ns, hs},
    ns = Map[ReadAttribute[#, NPoints] &, ds];
    hs = Map[1/#&, ns];
    Return[MapThreadData[RichardsonExtrapolate3[#1,#2,#3, hs[[1]], hs[[2]], hs[[3]], p] &, {d1,d2,d3}]]];

RichardsonExtrapolationError[ds:{d1_DataTable, d2_DataTable, d3_DataTable}, p_] :=
    RichardsonExtrapolate[Drop[ds,1], p] - RichardsonExtrapolate3[ds, p];

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

AlignPhases[phaseTbs:{DataTable[__] ...}, t_] :=
  Module[{dts, dt, phaseFns, refPhases, adjustments, adjusted,
          ranges, min, max, constTb},
    If[Length[phaseTbs] < 2, Return[phaseTbs]];

    ranges = Map[DataTableRange, phaseTbs];
    min = Apply[Max, Map[First, ranges]];
    max = Apply[Min, Map[Last, ranges]];
    If[t < min || t > max, Throw["Cannot align phases at " <> ToString[t] <> " because not all the inputs exist there"]];

    phaseFns = Map[Interpolation, phaseTbs];
    refPhases = Map[#[t]&, phaseFns];
    adjustments = Map[Round[#/(2. Pi)] 2.0 Pi &, refPhases];
    
    constTb[a_, d_DataTable] := 
      MapData[a&,d];

    adjusted = MapThread[#1-constTb[#2,#1] &, {phaseTbs, adjustments}];
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
    ExtrapolateDataTables[OptionValue[ExtrapolationOrder], resWithr, range]];

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
  Module[{p, newOpts},
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

StrainFromPsi4[psi4_DataTable, fitStart_, fitEnd_] :=
  Module[{tStart, tStep, tEnd, psi4Fn, ints, dataRealTb, modelReal, 
          fitReal, dataImagTb, modelImag, fitImag, hTb},
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
  Module[{squareDiff,lastFitMessageTime, it, pList, fit, fit2},
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
      fit = NMinimize[Apply[squareDiff, pList2], paramSpecs2, 
                AccuracyGoal -> Infinity, PrecisionGoal->3,   Method->subMethod]
    ];


    fit2 = fit /. MapThread[(#1 -> #2) &, {pList2, pList}];

(*    Print["fit = ", fit2];*)
    Return[fit2];
  ];

LocateMaximum[d_DataTable] :=
 Module[{tMax, fMax, l, maxFind, fn, max, t1, t2, t, tMax2},
  l = ToList[d];
  {t1, t2} = DataTableRange[d];
  fMax = -Infinity;
  maxFind[{t_, f_}] :=
   If[f > fMax, fMax = f; tMax = t];
  Scan[maxFind, l];
  fn = Interpolation[d];
  tMax2 = 
   t /. FindMaximum[{fn[t], {t > tMax - 50, t < tMax + 50}}, {t, 
       tMax}][[2]];
  Return[tMax2]]

(* Parameter file parsing *)

ParseParameterFile[from_String] :=
 Module[{lines, parseLine, thorns, param, val, removeWhiteSpace, fileName, fileNames},
  If[StringMatchQ[from, __ ~~ ".par"],
    fileNames = {from},
    fileNames = FindRunFile[from, from <> ".par"];
    If[Length[fileNames] == 0,
      fileNames = FindRunFile[from, from <> "-1.par"];
      If[Length[fileNames] == 0,
        Throw["Cannot find parameter file " <> ToString[from]]]]];

  fileName = First[fileNames];

  lines = ReadList[fileName, String];
  removeWhiteSpace[t_] := 
   StringReplace[
    t, (StartOfString ~~ Whitespace) | (Whitespace ~~ EndOfString) :> 
     ""];
  
  parseLine[s_] :=
   If[StringMatchQ[s, RegularExpression["[ \t]*#.*"]],
    Comment[s],
    If[StringMatchQ[
      s, (Whitespace | StartOfString) ~~ 
       "ActiveThorns" ~~ (Whitespace | "=") ~~ __, IgnoreCase -> True],
     ActiveThorns[
      StringSplit[
       StringCases[s, "\"" ~~ thorns__ ~~ "\"" -> thorns][[1]]]],
     If[StringMatchQ[s, RegularExpression[".*::.*=.*"]],
      ParameterSetting[
       ToLowerCase[
        removeWhiteSpace[
         StringCases[s, param__ ~~ "=" ~~ val__ -> param][[1]]]], 
       removeWhiteSpace[
        StringCases[s, param__ ~~ "=" ~~ val__ -> val][[1]]]],
      Throw["Unrecognized line in parameter file: " <> s]]]];
  Map[parseLine, lines]
  ];

LookupParameter[parFile_List, name_] :=
 Module[{l},
  l = Cases[parFile, ParameterSetting[ToLowerCase[name], x_] -> x];
  If[Length[l] == 0, Throw["Parameter " <> name <> " not found"]];
  First[l]];

LookupParameter[from_String, name_] :=
  Module[{},
    (* Assume the parameter file is named after the run *)
    LookupParameter[ParseParameterFile[from], name]
  ];

FindParameters[parFile_String, pattern_] :=
  FindParameters[ParseParameterFile[parFile], pattern];

FindParameters[parFile_List, pattern_] :=
  Module[{parameters},
    parameters = Cases[parFile, ParameterSetting[name_,value_] -> name];
    Select[parameters, StringMatchQ[#, pattern] &]];

GridSpacingOnLevel[runName_, l_] :=
  Module[{h0},
    h0 = ToExpression[LookupParameter[runName, "CoordBase::dx"]];
    h0 / 2^l
  ];

FinestGridSpacing[runName_] :=
  GridSpacingOnLevel[runName, Max[RefinementLevels[runName]]];

BoxRadiiOnLevel[runName_, l_] :=
  Module[{h0},
    If[l == 0, Return[{ToExpression[LookupParameter[runName, "CoordBase::xmax"]]}]];
    params = FindParameters[runName, "regridboxes::centre_*_radius" ~~ (Whitespace | "") ~~ "["<>ToString[l]<>"]"];
    Map[ToExpression[LookupParameter[runName, #]] &, params]
  ];

CountRefinementLevels[runName_String] :=
  ToExpression[LookupParameter[runName, "CarpetRegrid::refinement_levels"]];

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
  Module[{},
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

ReadIHSpin[runName_, hn_] :=
 MakeDataTable[Map[{#[[1]], {#[[2]], #[[3]], #[[4]]}} &, 
  ReadColumnFile[runName, "ihspin_hn_" <> ToString[hn] <> ".asc"]]];

ReadIHSpinX[runName_, hn_] :=
 MakeDataTable[Map[{#[[1]], #[[2]]} &, 
  ReadColumnFile[runName, "ihspin_hn_" <> ToString[hn] <> ".asc"]]];

ReadIHSpinY[runName_, hn_] :=
 MakeDataTable[Map[{#[[1]], #[[3]]} &, 
  ReadColumnFile[runName, "ihspin_hn_" <> ToString[hn] <> ".asc"]]];

ReadIHSpinPhase[runName_, hn_] :=
 Module[{spin, sx, sy},
  spin = ReadIHSpin[runName, hn];
  sx = MakeDataTable[Map[{#[[1]], #[[2]][[1]]} &, ToList[spin]]];
  sy = MakeDataTable[Map[{#[[1]], #[[2]][[2]]} &, ToList[spin]]];
  Phase[sx + I sy]];

ReadAHMass[runName_, hn_] :=
 Module[{},
  MakeDataTable[
   ReadColumnFile[runName, "BH_diagnostics.ah"<>ToString[hn]<>".gp", {2, 27}]]];

ReadAHRadius[runName_, hn_] :=
 Module[{},
  MakeDataTable[
   ReadColumnFile[runName, "BH_diagnostics.ah"<>ToString[hn]<>".gp", {2, 8}]]];

functionOfPhase[d_DataTable, p_DataTable, dp_: 0.01] :=
 Module[{phiOft, tOfphi, tOfphiFn, phiMin, phiMax, dOftFn, dOfphiTb},
  phiOft = ToList[p];
  tOfphi = Map[Reverse, phiOft];
  (*Return[tOfphi];*)
  tOfphiFn = Interpolation[tOfphi];
  phiMin = First[tOfphi][[1]];
  phiMax = Last[tOfphi][[1]];
  (*Return[{phiMin,phiMax}];*)
  dOftFn = Interpolation[d];
  dOfphiTb = 
   Table[{phi, dOftFn[tOfphiFn[phi]]}, {phi, phiMin, phiMax, dp}];
  MakeDataTable[dOfphiTb]];

ReadPsi4Modes[runName_] :=
  Module[{names, modeFromFileName, radii}, 
   names = FindRunFilesFromPattern[runName, 
     "Ylm_WEYLSCAL4::Psi4r_l*_m*_r*.asc"];
   modeFromFileName[name_] := 
    Round[ToExpression[
      StringReplace[name, 
       "Ylm_WEYLSCAL4::Psi4r_l" ~~ x__ ~~ "_m" ~~ __ ~~ "r" ~~ __ ~~ 
         ".asc" -> x]]];
   radii = Union[Map[modeFromFileName, names]]];

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
    ToList[ReadMinTrackerCoordinates[run, tracker]]], "TSV"]];

ExportBHRelativeCoords[run_String, dir_String] :=
 Module[{},
  If[FileType[dir] =!= Directory, CreateDirectory[dir]];
  Export[dir <> "/bh_coords_polar.asc", 
   MapThread[{#[[1]], #1[[2]], #2[[2]]} &, {ToList[
      ReadMinTrackerRadius[run]], ToList[ReadMinTrackerRadius[run]]}],
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

End[];

EndPackage[];
