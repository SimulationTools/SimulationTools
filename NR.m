
(* A package for dealing with numerical relativity data *)

BeginPackage["NR`", {"DataTable`", "Memo`"}];

FileInRun;
ReadColumnFile;
ReadColumnFile2;
ReadPsi4;
ReadMinTrackerCoordinates;
ReadMinTrackerCoordinate;
ReadMinTrackerRadius;
ReadMinTrackerPhase;
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

(*--------------------------------------------------------------------
  File reading
  --------------------------------------------------------------------*)

FileInRun[runName_String, fileName_] :=
  Module[{fullPath = ToFileName[runName, fileName],
          runPath = ToFileName[{Global`RunDirectory, runName <> "-all"}, fileName],
          sfPath1 = ToFileName[{Global`RunDirectory, runName, "output-0000-active", runName}, fileName],
          sfPath2 = ToFileName[{Global`RunDirectory, runName, "output-0000", runName}, fileName]},

    Which[
      StringMatchQ[runName, __ ~~ "/" ~~ __], fullPath,
      FileType[runPath] =!= None, runPath,
      FileType[sfPath1] =!= None, sfPath1,
      FileType[sfPath2] =!= None, sfPath2,
      Throw["Can't find file "<>fileName<>" in run "<>runName], Null]];

ReadColumnFile[fileName_String, cols_List] :=
  Module[{list, list2, isComment},
    If[FileType[fileName] === None, Throw["File " <> fileName <> " not found"]];
    list = Import[fileName, "TSV"];

    isComment[x_] :=
      StringQ[x[[1]]] && StringMatchQ[x[[1]], "#" ~~ ___];

    list2 = Select[list, !isComment[#] &];

(*    list = ReadList[fileName, Real, RecordLists->True];*)
    Return[Map[Extract[#, Map[List, cols]] &, list2]]];

ReadColumnFile2[fileName_String, cols_List] :=
  Module[{list, list2, isComment},
    If[FileType[fileName] === None, Throw["File " <> fileName <> " not found"]];

    list = ReadList[fileName, String];

    isComment[x_] :=
      StringQ[x] && StringMatchQ[x, "#" ~~ ___];

    list2 = Select[list, !isComment[#] &];

    list3 = Map[StringSplit, list2];
    list4 = Map[Map[ToExpression, #] &, list3];

    Return[Map[Extract[#, Map[List, cols]] &, list4]]];


(* Simulation data *)

DefineMemoFunction[ReadPsi4[runName_String, l_?NumberQ, m_?NumberQ, rad_?NumberQ],
  Module[{fileName, list, psi4},
    fileName = FileInRun[runName, "Ylm_WEYLSCAL4::Psi4r_l" <>
             ToString[l] <> "_m" <> ToString[m] <> "_r" <> 
             ToString[rad] <> ".00.asc"];
    list = ReadColumnFile[fileName, {1,2,3}];
    psi4 = Map[{#[[1]], #[[2]] + I #[[3]]}&, list];
    Return[AddAttribute[MakeDataTable[psi4], RunName -> runName]]]];

DefineMemoFunction[ReadMinTrackerCoordinates[runName_String, tracker_Integer],
  Module[{name},
(*      Print["Reading..."];*)
      fileName = FileInRun[runName, "MinTracker" <> ToString[tracker] <> ".asc"];
      list = ReadColumnFile[fileName, {2,3,4,5}];
      list2 = Map[{#[[1]], {#[[2]], #[[3]], #[[4]]}}&, list];
      Return[MakeDataTable[list2]]]];

ReadMinTrackerCoordinate[runName_String, tracker_Integer, coord_Integer] :=
  Module[{coords},
    coords = ReadMinTrackerCoordinates[runName, tracker];
    MapData[#[[coord]]&, coords]];

ReadADMMass[runName_String] :=
  ReadList[FileInRun[runName, "ADM_mass_tot.asc"], Real][[1]];

ReadPsi4Radii[runName_] :=
  Module[{dirs, names, radiusFromFileName, radii},
    dirs = {Global`RunDirectory <> "/" <> runName <> "-all",
            Global`RunDirectory <> "/" <> runName <> "/output-0000-active/" <> runName,
            Global`RunDirectory <> "/" <> runName <> "/output-0000" <> runName};
    names = FileNames[{"*Ylm_WEYLSCAL4::Psi4r_l2_m2_r*.asc"}, dirs];
    radiusFromFileName[name_] :=
      Round[ToExpression[
        StringReplace[name,
          __ ~~ "Ylm_WEYLSCAL4::Psi4r_l" ~~ __ ~~ "m" ~~ __ ~~ "r"
          ~~ x__ ~~ ".asc" -> x]]];
    radii = Map[radiusFromFileName, names]];

ReadRunSpeed[runName_] := 
 ReadColumnFile2[FileInRun[runName, "runstats.asc"], {2, 4}];

(*--------------------------------------------------------------------
  Data conversion 
  --------------------------------------------------------------------*)

ReadMinTrackerRadius[runName_String] :=
  Module[{x0, x1, rad},
    x0 = ReadMinTrackerCoordinates[runName, 0];
    x1 = ReadMinTrackerCoordinates[runName, 1];
    rad = MapThreadData[Norm[#1-#2] &, {x0, x1}];
    Return[rad];
  ];

ReadMinTrackerPhase[runName_String] :=
  Module[{x0, x1, rad},
    x0 = ReadMinTrackerCoordinates[runName, 0];
    x1 = ReadMinTrackerCoordinates[runName, 1];
    xyTrans = MapThreadData[Take[#1-#2,2] &, {x0, x1}]; (* Project into xy plane *)
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
  Module[{ns, hs},
    ns = Map[ReadAttribute[#, NPoints] &, ds];
    hs = Map[1/#&, ns];
    Return[MapThreadData[RichardsonExtrapolate[#1,#2, hs[[1]], hs[[2]], p] &, ResampleDataTables[{d1,d2}]]]];

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
          ranges, min, max},
    If[Length[phaseTbs] < 2, Return[phaseTbs]];

    ranges = Map[DataTableRange, phaseTbs];
    min = Apply[Max, Map[First, ranges]];
    max = Apply[Min, Map[Last, ranges]];
    If[t < min || t > max, Throw["Cannot align phases at " <> ToString[t] <> " because not all the inputs exist there"]];

    phaseFns = Map[Interpolation, phaseTbs];
    refPhases = Map[#[t]&, phaseFns];
    adjustments = Map[Round[#/(2. Pi)] 2.0 Pi &, refPhases];
    adjusted = MapThread[#1-#2 &, {phaseTbs, adjustments}]];

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
  Module[{allRads, rads, fs, rdTb, rMin, rMax},
    allRads = ReadPsi4Radii[runName];
    rads = If[OptionValue[RadiusRange] === All,
      allRads,
      {rMin, rMax} = OptionValue[RadiusRange]; 
      Select[allRads, # >= rMin && # <= rMax &]];

    fs = Map[reader[runName, #] &, rads];
    rdTb = MapThread[List, {rads, fs}];
    If[!(OptionValue[MassADM] === None),
      Print["ExtrapolateRadiatedQuantity: warning: you have specified MassADM unnecessarily, and it is being ignored"]];
    ExtrapolateRadiatedQuantity[rdTb, opts, MassADM -> ReadADMMass[runName]]];

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

ExtrapolatePsi4[runName_String, l_, m_, opts:OptionsPattern[]] :=
  Module[{phase,amp,psi4},
    phase = ExtrapolatePsi4Phase[runName, l, m, opts];
    amp = ExtrapolatePsi4Amplitude[runName, l, m, opts];
    psi4 = MapThreadData[#1 Exp[I #2] &, {amp, phase}];
    psi4];

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

FitFunction[d_, f_, paramSpecs_, method_, subMethod_] :=
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

End[];

EndPackage[];
