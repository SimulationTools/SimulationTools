
BeginPackage["Waveforms`", {"RunFiles`", "DataTable`", "Memo`", "ReadHDF5`", "Providers`", "InitialData`", "Error`"}];

SchmidtAngle::usage = "SchmidtAngle[run, t, r] computes the angle between the z-axis and the direction in which the (2,2) and (2,-2) modes are maximized.";
ReconstructPsi4::usage = "ReconstructPsi4[run, t, r] returns a CompiledFunction of two real arguments (\[Theta] and \[Phi]) which is computed by summing all the spherical harmonic modes, \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) at time t and radius r.";
ReadPsi4::usage = "ReadPsi4[run, l, m, r] returns a DataTable of the l,m mode of Psi4 at radius r from run.";
ReadPsi4Phase::usage = "ReadPsi4Phase[run, l, m, r, threshold] returns a DataTable of the phase of the complex l,m mode of Psi4 at radius r from run.  The phase is cut off after the time that the amplitude goes below threshold.";
ReadPsi4RadiiStrings::usage = "ReadPsi4RadiiStrings[run] returns a list of the radii (in original string form) at which the modes of \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) are available in run.";
ReadPsi4Radii::usage = "ReadPsi4Radii[run] returns a list of the radii at which the modes of \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) are available in run.";
ReadPsi4Modes::usage = "ReadPsi4Modes[run] returns a list of the modes of \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) that are available in run.";
RunName::usage = "RunName is a DataTable attribute added by ReadPsi4.";
RadiusRange;
StrainFromPsi4::usage = "StrainFromPsi4[psi4, omega0] converts a DataTable containing psi4[t] into strain and its time derivative, {h[t], h'[t]}, using the method of Reisswig and Pollney with a cut-off frequency omega0.
StrainFromPsi4[psi4, {t0, t1}] converts using time domain integration.";
NumCycles::usage = "NumCycles[run, start] gives the number of gravitational wave cycles for run. The number of cycles is calculated starting at start and terminating at the merger, which is determined from the maimum of the gravitational wave signal.
NumCycles[psi4, start] operates on the DataTable psi4 instead of run.";
AlignPhases::usage = "AlignPhases[{d1, ...}, t] aligns the DataTables {d1, ...} at time t. The independent variable is assumed to be a phase so that the resulting phases all start out within 2\[Pi] of each other.";
Continuous;
ExtrapolatePsi4::usage = "ExtrapolatePsi4[run, l, m] extrapolates the (l, m) mode of \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) to infinite radius.";
ExtrapolatePsi4Amplitude::usage = "ExtrapolatePsi4Amplitude[run, l, m] extrapolates the amplitude of the (l, m) mode of \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) to infinite radius.";
ExtrapolatePsi4Phase::usage = "ExtrapolatePsi4Amplitude[run, l, m] extrapolates the phase of the (l, m) mode of \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) to infinite radius.";
UseTortoiseCoordinate::usage = "UseTortoiseCoordinate is an option for radius extrapolation routines which specifies whether the radial coordinate should be converted to a tortoise coordinate. This may improve the accuracy of the extrapolation.";
TortoiseCoordinate::usage = "TortoiseCoordinate[r, M] gives the tortoise coordinate corresponding to radius r assuming a Schwarzschild black hole of mass M. The constant of integration is chosen so that the tortoise and radial coordinate coincide at r=4M.";

ExtrapolateScalarFull;
ExtrapolateScalar;
ExtrapolatedValue;
ExtrapolateScalarWithRadii;

ExtrapolatedCurve;
ExtrapolateDataTables;
RadiusTimeDataToTimeRadiusData;
ExtrapolateRadiatedQuantity;
ApplyToPhases;
ExtrapolationError;


ReadWaveformFile;
AlignMaxima;
AlignMaximaOfAbs;
ExtrapolateStrain;
FixedFrequencyIntegrate;

RadialExtrapolation;
ToRetardedTime;
ToAbsPhase;
ToComplex;
ReadRadiallyExtrapolatedWave;
RadiallyExtrapolatedWave;
ReadRadiallyExtrapolatedPsi4;
ReadRadiallyExtrapolatedStrain;
Psi4PerturbativeCorrection;

Options[ExtrapolateRadiatedQuantity] = 
  {ExtrapolationOrder -> 1,
   UseTortoiseCoordinate -> True,
   MassADM -> None,
   ApplyFunction -> None, 
   AlignPhaseAt -> None,
   RadiusRange -> All,
   "Radii" -> All,
   ExtrapolationErrorRelative -> False,
   NonUniformGrid -> False};

Options[ExtrapolationError] = Options[ExtrapolateRadiatedQuantity];
Options[ExtrapolatePsi4Phase] = Options[ExtrapolateRadiatedQuantity];

Begin["`Private`"];

(* master branch compatibility *)
CoordinateRange = DataTableRange;
ToListOfData = DepVar;
ToListOfCoordinates = IndVar;
AlignedPhases = AlignPhases;

ToDataTable[l_List] := MakeDataTable[l];

(*--------------------------------------------------------------------
  Reading Psi4 from an external provider
  --------------------------------------------------------------------*)

If[ProviderPreferences["Waveforms"] === {},
   ProviderPreferences["Waveforms"] = {"MultipoleHDF5", "MultipoleASCII"}];

ReadPsi4Data[runName_String, l_?NumberQ, m_?NumberQ, rad_] :=
  CallProvidedFunction["Waveforms","ReadPsi4Data",{runName,l,m,rad}];

ReadPsi4RadiiStrings[runName_] :=
  CallProvidedFunction["Waveforms","ReadPsi4RadiiStrings",{runName}];

ReadPsi4Modes[runName_] :=
  Select[CallProvidedFunction["Waveforms","ReadPsi4Modes",{runName}], #[[1]] >= 2 &];

(*--------------------------------------------------------------------
  Operations on Psi4
  --------------------------------------------------------------------*)

DefineMemoFunction[
  ReadPsi4[runName_String, l_?NumberQ, m_?NumberQ, rad_:Automatic],
  Module[{radii, radString},

  (* Get a list of radii available in the form expr -> "exprstring" *)
  radii = Thread[ReadPsi4Radii[runName] -> ReadPsi4RadiiStrings[runName]];

  If[radii == Null,
    Error["No \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) data found."];
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
      Error["Radius "<>ToString[rad]<>" not found."];
  ];

  AddAttribute[ReadPsi4Data[runName, l, m, radString],RunName -> runName]]];

(* Return a list of radii available *)
ReadPsi4Radii[runName_] := ToExpression /@ (ReadPsi4RadiiStrings[runName] /. "inf" -> "Infinity");

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

ReconstructPsi4[sim_, t_, rad_: Automatic] :=
 Module[{modes, psi4modes, harmonics, cf, psi4},
  modes = ReadPsi4Modes[sim];
  psi4modes = Interpolation[ReadPsi4[sim, #[[1]], #[[2]], rad]][t] & /@ modes;
  harmonics[th_, ph_] := SpinWeightedSphericalHarmonic[-2, #[[1]], #[[2]], th, ph] & /@ modes;
  cf=Compile[{{th, _Real}, {ph, _Real}}, Evaluate[Plus @@ (psi4modes.harmonics[th, ph])]];
  psi4[th_?NumericQ, ph_?NumericQ] := cf[th, ph];
  psi4
];

SchmidtAngle[run_, t_, rad_: Automatic] := 
 Module[{psi4, psi4l2m2, psi4l2mm2, th1},
  psi4 = ReconstructPsi4[run, t, rad];
  psi4l2m2[th1_?NumericQ, ph1_?NumericQ] := 
   Abs[NIntegrate[rad Sin[th] psi4[th - th1, ph - ph1] *
     Conjugate[SpinWeightedSphericalHarmonic[-2, 2, 2, th, ph]],
     {th, 0, Pi}, {ph, 0, 2 Pi}]];
  psi4l2mm2[th1_?NumericQ, ph1_?NumericQ] := 
   Abs[NIntegrate[rad Sin[th] psi4[th - th1, ph - ph1] *
     Conjugate[SpinWeightedSphericalHarmonic[-2, 2, -2, th, ph]],
     {th, 0, Pi}, {ph, 0, 2 Pi}]];
  
  th1 /. {FindMaximum[psi4l2m2[th1, 0], {th1, 0}, Method -> "PrincipalAxis"][[2]], 
          FindMaximum[psi4l2mm2[th1, 0], {th1, 0}, Method -> "PrincipalAxis"][[2]]}
];

(*--------------------------------------------------------------------
  Extrapolation
  --------------------------------------------------------------------*)

Global`StandardDefinition[ExtrapolateScalarFull] = True;

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

(* As a special case, 0th order extrapolation is defined to use the highest radius *)
ExtrapolateScalarFull[0, rfTb_] :=
  {ExtrapolatedValue -> rfTb[[Ordering[First/@rfTb][[-1]]]][[2]],
   Data -> Null,
   ExtrapolatedCurve -> Null};

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

Global`StandardDefinition[ExtrapolateScalar] = True;
ExtrapolateScalar[args__] :=
 ExtrapolatedValue /. ExtrapolateScalarFull[args];

Global`StandardDefinition[ExtrapolateScalarWithRadii] = True;
ExtrapolateScalarWithRadii[f_, rads_List, order_:1] :=
  ExtrapolatedValue /. ExtrapolateScalarFull[order, MapThread[List, {rads, Map[f, rads]}]];

RadiusTimeDataToTimeRadiusData[rdTb : {{_, DataTable[__]} ...}] :=
 Module[{rads, dts, ts, lists, tbToVec, vecs, rfTbs, combineWithRads, lengths, rfWithRads, allts},
  rads = Map[First, rdTb];
(*  Print["rads = ", rads];*)
  dts = Map[Last, rdTb];
  lists = Map[ToList, dts];
  lengths = Map[Length, lists];
  If[! (Length[Union[lengths]] === 1), 
  Error["ExtrapolateDataTables: Input DataTable objects do not have \
  the same number of points: ", lengths]];
  allts = IndVar/@dts;
  If[!Equal@@allts,
     Error["RadiusTimeDataToTimeRadiusData: Input DataTables do not have the same coordinates"]];
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
  If[p > Length[rdTb]-1,
    Error["Insufficient radii (", Length[rdTb], ") for extrapolation at order ", p]];
  extraps = Map[ExtrapolateScalar[p, #] &, rds];
  MakeDataTable[MapThread[List, {ts, extraps}]]];

ExtrapolateDataTables[p_Integer, rdTb : {{_, DataTable[__]} ...}, {rMin_, rMax_}, opts___] :=
  ExtrapolateDataTables[p,Select[rdTb, #[[1]] >= rMin && #[[1]] <= rMax &], opts];


Options[AlignPhases] = {Continuous -> False};

AlignPhases[phaseTbs:{DataTable[__] ...}, t_, opts:OptionsPattern[]] :=
  Module[{phaseFns, refPhases, adjustments, adjusted,
          ranges, min, max, constTb, phase1},
    If[Length[phaseTbs] < 2, Return[phaseTbs]];

    ranges = Map[DataTableRange, phaseTbs];
    min = Apply[Max, Map[First, ranges]];
    max = Apply[Min, Map[Last, ranges]];
    If[t < min || t > max, Error["Cannot align phases at " <> ToString[t] <> " because not all the inputs exist there"]];

    phaseFns = Map[Interpolation, phaseTbs];
    refPhases = Map[#[t]&, phaseFns];
    phase1 = refPhases[[1]];
    adjustments =
      If[!OptionValue[Continuous],
        Map[Round[(#-phase1)/(2. Pi)] 2.0 Pi &, refPhases],
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
        Error["No mass given for tortoise coordinate.  Specify MassADM -> m in ExtrapolateRadiatedQuantity"]];
      fTbs = MapThread[ShiftDataTable[-TortoiseCoordinate[#1, OptionValue[MassADM]],#2]&, 
                                {rads, fTbs}],
      fTbs = MapThread[ShiftDataTable[-#1, #2] &, 
                                {rads, fTbs}]];

    alignPhaseAt = OptionValue[AlignPhaseAt];
    If[!(alignPhaseAt === None),
      fTbs = AlignPhases[fTbs, alignPhaseAt]];

    fTbsRes = 
         If[OptionValue[NonUniformGrid],
            Module[
              {highRes},
              highRes = fTbs[[Ordering[Spacing /@ fTbs][[1]]]];
              IntersectDataTables[ResampleDataTable[#, highRes] & /@ fTbs]],
            (* else *)
            ResampleDataTables[fTbs]];
    applyFunction = OptionValue[ApplyFunction];

    fTbsRes2 = If[!(applyFunction === None),
      Map[applyFunction, fTbsRes],
      fTbsRes];

    resWithr = MapThread[List, {rads, fTbsRes2}];
    ExtrapolateDataTables[OptionValue[ExtrapolationOrder], resWithr]];

ExtrapolateRadiatedQuantity[runName_String, reader_, opts:OptionsPattern[]] :=
  Module[{allRads, rads, missingRadii, fs, rdTb, rMin, rMax, mADM},

    allRads = ReadPsi4Radii[runName];
    rads = If[OptionValue[Radii] === All,
              rads = If[OptionValue[RadiusRange] === All,
                        allRads,
                        {rMin, rMax} = OptionValue[RadiusRange]; 
                        Select[allRads, # >= rMin && # <= rMax &]],
              (* else *)
              OptionValue[Radii]];

    missingRadii = Complement[rads, allRads];
    If[missingRadii =!= {},
       Error["ExtrapolateRadiatedQuantity: Radii "<>ToString[missingRadii]<>" are missing in run "<>runName]];

    (* If[Union[rads] =!= Union[allRads], *)
    (*    Error["ExtrapolateRadiatedQuantity: Radii "<>ToString[Complement[rads, allRads]]<>" not found in run "<>runName]]; *)

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

DefineMemoFunction[ExtrapolateStrain[runName_String, l_, m_, om_, opts:OptionsPattern[]],
  Module[
    {phaseReader,ampReader,phase,amp},

    phaseReader[run_, rad_] :=
    Phase[StrainFromPsi4[ReadPsi4[run, l, m, rad],om]];

    ampReader[run_, rad_] :=
    rad Abs[StrainFromPsi4[ReadPsi4[run, l, m, rad],om]];

    phase = ExtrapolateRadiatedQuantity[runName, phaseReader, AlignPhaseAt->100, opts];
    amp = ExtrapolateRadiatedQuantity[runName, ampReader, opts];

    MapThreadData[#1 Exp[I #2] &, {amp, phase}]]];

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

Global`StandardDefinition[ffi] = True;

(* Note that these expect input in Fourier space *)
ffi[{f_, d_}, f0_] :=
 Module[{div},
  div = 2. Pi I If[f>0., Max[f, f0, $MachineEpsilon], Min[f, -f0, -$MachineEpsilon]];
  {f, d/div}
];

ffiDataTable[d_DataTable, f0_] :=
  Map[ffi[#, f0 / (2. Pi)] &, d];


FixedFrequencyIntegrate[q_DataTable, f0_?NumericQ] :=
 Module[{qUniform,t0,qTilde,intqTilde,intq,
         uniform = UniformGridQ[q]},
  qUniform = If[uniform, q, MakeUniform[q]];
  t0 = DataTableRange[qUniform][[1]];
  qTilde = Fourier[qUniform];
  intqTilde = ffiDataTable[qTilde, f0];
  intq = InverseFourier[intqTilde,t0];
  (* TODO: I'm not sure where the "-" comes from here, but it seems to be necessary *)
  -If[uniform, intq, ResampleDataTable[intq,q]]];

StrainFromPsi4[psi4_DataTable, f0_?NumericQ] :=
 Module[{psi4Uniform, psi4f, dhf, hf, dh, h,
         uniform = UniformGridQ[psi4], t0},
  psi4Uniform = If[uniform, psi4, MakeUniform[psi4]];
  t0 = DataTableRange[psi4Uniform][[1]];
  psi4f = Fourier[psi4Uniform];
  dhf = ffiDataTable[psi4f, f0];
  hf = ffiDataTable[dhf, f0];
  {h, dh} = InverseFourier[#,t0] & /@ {hf, dhf};
  If[uniform, h, ResampleDataTable[h,psi4]]];

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
    Return[MakeDataTable[hTb]];
];

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

AlignMaxima[ds_List] :=
  Module[{maxima},
   maxima = Map[LocateMaximum, ds];
   MapThread[ShiftDataTable[-#1, #2] &, {maxima, ds}]];

AlignMaximaOfAbs[ds_List] :=
  Module[{maxima},
   maxima = Map[LocateMaximum, Abs /@ ds];
   MapThread[ShiftDataTable[-#1, #2] &, {maxima, ds}]];


ReadWaveformFile[file_] :=
  Module[
    {data, lockFile, waitForLock, releaseLock},
    If[FileType[file] ===None,
       Error["ReadWaveformFile: File "<>file<>" does not exist",Global`FileNotFound]];

    (* If[FileExtension[file] === "gz", *)
    (*    data = Import["!gunzip< "<>file,"Table"], *)
    (*    data = Import[file,"Table"]]; *)

    (* The Mathematica GZIP reader creates temporary files in /tmp.
       If multiple kernels try to open gzipped files with the same
       leaf name, this causes these temporary files to be corrupted,
       as they are the same.  Hence we protect access to the Import
       command using a lock file. *)

    lockFile = FileNameJoin[{$TemporaryDirectory, "GZIP.exe.lock"}];
    waitForLock[] :=
    (While[!Quiet[Check[CreateDirectory[lockFile]; True, False, CreateDirectory::filex],CreateDirectory::filex],
           Pause[0.1]]);

    releaseLock[] :=
    DeleteDirectory[lockFile];

    waitForLock[];
    data = Import[file,"Table"];
    releaseLock[];

    If[data === $Failed,
       Error["Failed to open file "<>file]];

    MakeDataTable[Select[Map[{#[[1]],#[[2]]+I #[[3]]}&, data], NumberQ[#[[2]]]&]]];

(****************************************************************)
(* Extrapolation                                                *)
(****************************************************************)

(* RadialExtrapolation *)

RadialExtrapolation[{rs_List, fs_List}, order_Integer] :=
  Module[
    {model, a, x, xf, fit},

    (* These checks do not seem to affect performance *)
    If[!And@@Map[NumberQ,fs],
       Error["RadialExtrapolation: Input data is not numeric"]];
    If[!And@@Map[NumberQ,rs],
       Error["RadialExtrapolation: Input radii are not numeric"]];

    xf = Map[{1./#[[1]], #[[2]]} &, Thread[{rs,fs}]];
    Fit[xf, Table[x^i, {i, 0, order}], {x}][[1]]];

RadialExtrapolation[{rs_List, fs:{_DataTable...}}, order_Integer] :=
  Module[
    {t, ds, rds, de, result},

    (* Print["order = ", order]; *)
    (* Print["rs = ", Short[rs]]; *)

    Assert[Apply[And,DataTable`Private`validQ/@fs]];

    (* These checks do not seem to affect performance *)
    If[!SameGridQ[fs],
       Error["RadialExtrapolation: Input DataTables are not defined on the same grid"]];
    If[!And@@Map[NumberQ,rs],
       Error["RadialExtrapolation: Input radii are not numeric"]];

    t = ToListOfCoordinates[fs[[1]]];
    (* Print["t = ", Short[t]]; *)
    ds = ToListOfData/@fs;
    (* Print["ds = ", Short[ds]]; *)
    rds = Transpose[ds];
    (* Print["rds = ", Short[rds]]; *)
    (* Print["Before RadialExtrapolation"]; *)
    de = Map[RadialExtrapolation[{rs, #}, order] &,rds];
    (* Print["de = ", de]; *)
    ToDataTable[Thread[{t,de}]]];

(* ToAbsPhase *)

ToAbsPhase[f_DataTable] :=
  {Abs[f], Phase[f]};

ToAbsPhase[fs:{_DataTable...}, tAlign_] :=
  {Abs/@fs, AlignedPhases[Phase/@fs, tAlign]};

(* ToComplex *)

ToComplex[{a_DataTable,phi_DataTable}] :=
  a Exp[I phi];

(* ToRetardedTime *)

ToRetardedTime[r_, f_DataTable, rStarOfr_:Identity] :=
  ShiftDataTable[-rStarOfr[r],f];

ToRetardedTime[{rs_List, fs:{_DataTable...}}, rStarOfr_:Identity] :=
  MapThread[ToRetardedTime[#1,#2,rStarOfr] &, {rs,fs}];

(* RadiallyExtrapolatedWave *)

notOptionQ[x_] := ! OptionQ[x];
Options[RadiallyExtrapolatedWave] = {"AbsPhase" -> True,
                                     "DiscretePhaseAlignmentTime" -> Automatic};
RadiallyExtrapolatedWave[{rs_List, fs:{_DataTable...}}, 
                         order_Integer,
                         rStarOfr : (_?notOptionQ) : Identity, 
                         opts:OptionsPattern[]] :=
  Module[
    {ret,ext,tAlign,temp},
    ret = ToRetardedTime[{rs,fs},rStarOfr];
    (* Print["ret = ", ret]; *)
    (* Print["Max[CoordinateRange[First[ret]][[1]],0] = ", Max[CoordinateRange[First[ret]][[1]], 0]]; *)
    ext[ds_] := RadialExtrapolation[{rs, ds}, order];

    If[OptionValue[AbsPhase],
       tAlign = If[OptionValue[DiscretePhaseAlignmentTime] === Automatic,
                   Max[CoordinateRange[First[ret]][[1]], 0],
                   OptionValue[DiscretePhaseAlignmentTime]];
       (* Print["tAlign = ", tAlign]; *)
       
       ToComplex[Map[ext, ResampleDataTables/@ToAbsPhase[ret,tAlign]]],
       ext[ResampleDataTables[ret]]]];

(* ReadRadiallyExtrapolatedWave *)

Options[ReadRadiallyExtrapolatedWave] =
  Join[{"RadialCoordinateTransformation" -> None},
       Options[RadiallyExtrapolatedWave]];

ReadRadiallyExtrapolatedWave[run_String, reader_, rads_List,
                             order_Integer, opts:OptionsPattern[]] :=
  RadiallyExtrapolatedWave[
    Transpose[
      Table[{r,reader[r]}, {r,rads}]],
    order, 
    OptionValue[RadialCoordinateTransformation] /.
    {None -> Identity,
     RadialToTortoise -> (RadialToTortoise[#,ReadADMMass[run]] &),
     IsotropicToTortoise -> (IsotropicToTortoise[#,ReadADMMass[run]] &)},
    FilterRules[{opts}, Options[RadiallyExtrapolatedWave]]];

(* selectRadii *)

selectRadii[rads_List, range:(Automatic|{_,_}), list:(Automatic|_List)] :=
  If[range =!= Automatic,
     Select[rads, (range[[1]] <= # <= range[[2]]) &],
     If[list =!= Automatic,
        list,
        If[range === Automatic && list === Automatic,
           rads,
           Error["Cannot specify both RadiusRange and Radii"]]]];

(* ReadRadiallyExtrapolatedPsi4 *)

Options[ReadRadiallyExtrapolatedPsi4] = 
  Join[Options[ReadRadiallyExtrapolatedWave],
       {"RadiusRange" -> Automatic,
        "Radii" -> Automatic}];

ReadRadiallyExtrapolatedPsi4[run_String, l_Integer, m_Integer,
                             order_Integer, opts:OptionsPattern[]] :=
  ReadRadiallyExtrapolatedWave[
    run,
    (# ReadPsi4[run,l,m,#]) &,
    selectRadii[ReadPsi4Radii[run],
                OptionValue[RadiusRange], OptionValue[Radii]],
    order, FilterRules[{opts},Options[ReadRadiallyExtrapolatedWave]]];

(* ReadRadiallyExtrapolatedStrain *)

(*
  - Not all modes are suitable for FFI (e.g. the 2,0 mode is non-oscillatory
  - The user should be able to select the strain conversion method
  - Some simulations might have CCE or other precomputed strain data; we should allow
    the user to choose
*)

Options[ReadRadiallyExtrapolatedStrain] = Options[ReadRadiallyExtrapolatedPsi4];
ReadRadiallyExtrapolatedStrain[run_String, l_Integer, m_Integer, om0_,
                               order_Integer, opts:OptionsPattern[]] :=
  ReadRadiallyExtrapolatedWave[
    run,
    Psi4ToStrain[# ReadPsi4[run,l,m,#], om0] &,
    selectRadii[ReadPsi4Radii[run],
                OptionValue[RadiusRange], OptionValue[Radii]],
    order, FilterRules[{opts}, Options[ReadRadiallyExtrapolatedWave]]];

(****************************************************************)
(* Psi4PerturbativeCorrection                                   *)
(****************************************************************)

Psi4PerturbativeCorrection[rpsi4_DataTable, l_Integer, r_?NumberQ, om0_?NumberQ] :=
  rpsi4 - 1/2 (l - 1) (l + 2) FixedFrequencyIntegrate[rpsi4/r, om0];

End[];

EndPackage[];
