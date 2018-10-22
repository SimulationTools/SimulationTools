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

BeginPackage["SimulationTools`Waveforms`",
 {
  "SimulationTools`CoordinateTransformations`",
  "SimulationTools`Convergence`",
  "SimulationTools`ColumnFile`",
  "SimulationTools`RunFiles`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`InitialData`",
  "SimulationTools`Memo`",
  "SimulationTools`Plotting`",
  "SimulationTools`Providers`",
  "SimulationTools`ReadHDF5`",
  "SimulationTools`Grids`",
  "SimulationTools`SpEC`",
  If[$VersionNumber >= 10, "GeneralUtilities`", Unevaluated[Sequence[]]]
 }];

(* Public *)

AlignedPhases::usage = "AlignedPhases[{d1, ...}, t] aligns the DataTables {d1, ...} at time t, either exactly or to within multiples of 2 \[Pi].";
Psi4ToStrain::usage = "Psi4ToStrain[psi4, omega0] converts a DataTable containing psi4[t] into strain h[t] using the Fixed Frequency Integration method with a cut-off frequency omega0.\nPsi4ToStrain[psi4, {t1, t2}] converts using time domain integration, choosing integration constants such that h[t1] = h[t2] = 0.";
WaveformCycles::usage = "WaveformCycles[psi4, start] gives the number of gravitational wave cycles in the waveform psi4.";
ReadWaveformCycles::usage = "ReadWaveformCycles[sim, start] gives the number of gravitational wave cycles in Psi4 for simulation sim.";
ReadExtrapolatedPsi4 (*::usage = "ReadExtrapolatedPsi4[sim, l, m] extrapolates the (l, m) mode of Psi4 from the simulation to infinite radius." *);
ReadExtrapolatedStrain (*::usage = "ReadExtrapolatedStrain[sim, l, m, om0] reads the (l, m) mode of \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) at various radii from a simulation, converts them to strain using Psi4ToStrain, and extrapolates the result to infinite radius."*);
ReadPsi4::usage = "ReadPsi4[sim, l, m, r] returns a DataTable of the l,m mode of Psi4 at radius r from sim.";
ReadPsi4Modes::usage = "ReadPsi4Modes[sim] returns a list of the modes of Psi4 that are available in sim.";
ReadPsi4Radii::usage = "ReadPsi4Radii[sim] returns a list of the radii at which the modes of Psi4 are available in sim.";
ImportWaveform::usage = "ImportWaveform[filename] imports an ASCII waveform file with columns time, real part and imaginary part and returns it as a DataTable.";
ExportWaveform::usage = "ExportWaveform[filename, d] exports a complex DataTable as an ASCII file with columns time, real part and imaginary part.";
RadialExtrapolation; (* TODO *)
ReadRadiallyExtrapolatedPsi4::usage = "ReadRadiallyExtrapolatedPsi4[sim, l, m, order] extrapolates the (l, m) mode of Psi4 from the simulation to infinite radius using a particular extrapolation order.";
ReadRadiallyExtrapolatedStrain::usage = "ReadRadiallyExtrapolatedStrain[sim, l, m, om0, order] reads the (l, m) mode of Psi4 at various radii from a simulation, converts them to strain using Psi4ToStrain, and extrapolates the result to infinite radius using a particular extrapolation order.";

(* Old function names *)

AlignPhases = AlignedPhases;
ExtrapolatePsi4 = ReadExtrapolatedPsi4;
ExtrapolateStrain = ReadExtrapolatedStrain;
NumCycles = ReadCycles;
ReadPsi4;
ReadPsi4Modes;
ReadPsi4Radii;
ReadWaveformFile;
SchmidtAngle = ReadSchmidtAngle;
StrainFromPsi4 = Psi4ToStrain;
ReconstructPsi4 = ReadReconstructedPsi4;
ReadWaveformFile = ImportWaveform;
ReadCycles = ReadWaveformCycles;

(* Deprecated / Experimental*)

Continuous;
AlignMaxima;
AlignMaximaOfAbs;
ApplyToPhases;
ExtrapolateDataTables;
ExtrapolatePsi4Amplitude(*::usage = "ExtrapolatePsi4Amplitude[sim, l, m] extrapolates the amplitude of the (l, m) mode of \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) to infinite radius."*);
ExtrapolatePsi4Phase(*::usage = "ExtrapolatePsi4Amplitude[sim, l, m] extrapolates the phase of the (l, m) mode of \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) to infinite radius."*);
ExtrapolateRadiatedQuantity;
ExtrapolateScalar;
ExtrapolateScalarFull;
ExtrapolateScalarWithRadii;
ExtrapolatedCurve;
ExtrapolatedValue;
ExtrapolationError;
RadiusRange;
RadiusTimeDataToTimeRadiusData;
ReadPsi4Phase(*::usage = "ReadPsi4Phase[sim, l, m, r, threshold] returns a DataTable of the phase of the complex l,m mode of Psi4 at radius r from sim.  The phase is cut off after the time that the amplitude goes below threshold."*);
ReadPsi4RadiiStrings(*::usage = "ReadPsi4RadiiStrings[sim] returns a list of the radii (in original string form) at which the modes of \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) are available in sim."*);
RunName(*::usage = "RunName is a DataTable attribute added by ReadPsi4."*);
StrainDotFromPsi4;
UseTortoiseCoordinate(*::usage = "UseTortoiseCoordinate is an option for radius extrapolation routines which specifies whether the radial coordinate should be converted to a tortoise coordinate. This may improve the accuracy of the extrapolation."*);
TortoiseCoordinate(*::usage = "TortoiseCoordinate[r, M] gives the tortoise coordinate corresponding to radius r assuming a Schwarzschild black hole of mass M. The constant of integration is chosen so that the tortoise and radial coordinate coincide at r=4M."*);
SchmidtAngle(*::usage = "SchmidtAngle[sim, t, r] computes the angle between the z-axis and the direction in which the (2,2) and (2,-2) modes are maximized."*);
ReadReconstructedPsi4(*::usage = "ReadReconstructedPsi4[sim, t, r] returns a CompiledFunction of two real arguments (\[Theta] and \[Phi]) which is computed by summing all the spherical harmonic modes, \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\) at time t and radius r."*);
FixedFrequencyIntegrate;

ToRetardedTime;  (* TODO *)
ToAbsPhase;
ToComplex;
ReadRadiallyExtrapolatedWave;  (* TODO *)
RadiallyExtrapolatedWave;  (* TODO *)
Psi4PerturbativeCorrection;
Psi4ToStrain;
ReadStrainCPM;
ReadStrainWaveExtract;
ReadSchwarzschildRadius;
ToSchwarzschildRetardedTime;
ReadWaveExtractRadii;
ReadStrainCPMHDF5;
ReadStrainCPMHDF5Direct;
ReadStrain;
ReadStrainRadii;
ReadSchwarzschildRadiusHDF5;

$UniformGridExtrapolation;
ffiDataTable;

Psi4RePlot;

Psi4PhaseErrors;
Psi4PhaseErrorPlot;
WaveformPhaseErrors;
WaveformPhaseErrorPlot;
StrainPlot;
StrainPhaseErrorPlot;
WaveformPlot;

(* Exceptions *)
Psi4RadiusNotFound;

Options[ExtrapolateRadiatedQuantity] = 
  {"ExtrapolationOrder" -> 1,
   "UseTortoiseCoordinate" -> True,
   "MassADM" -> None,
   "ApplyFunction" -> None, 
   "AlignPhaseAt" -> None,
   "RadiusRange" -> All,
   "Radii" -> All,
   "ExtrapolationErrorRelative" -> False,
   "NonUniformGrid" -> False};

Options[ExtrapolationError] = Options[ExtrapolateRadiatedQuantity];
Options[ExtrapolatePsi4Phase] = Options[ExtrapolateRadiatedQuantity];

Begin["`Private`"];

(**********************************************************)
(* WaveformCycles                                         *)
(**********************************************************)

DocumentationBuilder`MoreInformation["WaveformCycles"] =
  {"The number of cycles is calculated starting at t = start and terminating at the merger, which is determined from the maximum of the Psi4 waveform amplitude."};

SyntaxInformation[WaveformCycles] =
 {"ArgumentsPattern" -> {_, _}};

WaveformCycles[psi4_DataTable, start_] :=
 Module[{mergertime, phasei, cycles},
  mergertime = LocateMaximum[Abs[psi4]];
  phasei = Interpolation[Phase[psi4]];
  cycles = (phasei[start] - phasei[mergertime])/(2 \[Pi] );
  cycles
];


(**********************************************************)
(* ReadWaveformCycles                                     *)
(**********************************************************)

DocumentationBuilder`MoreInformation["ReadWaveformCycles"] =
 {
  "The number of cycles is calculated starting at 'start' and terminating at the merger, which is determined from the maximum of the Psi4 waveform amplitude.",
  "ReadWaveformCycles[sim, start] is equivalent to WaveformCycles[ReadPsi4[sim, 2,2, First[ReadPsi4Radii[sim]]], start].",
  "The lowest extraction radius is used to compute the number of cycles."
 };

SyntaxInformation[ReadWaveformCycles] =
 {"ArgumentsPattern" -> {_, _}};

ReadWaveformCycles[sim_String, start_] :=
 Module[{psi4, rads, r},
  rads = ReadPsi4Radii[sim];
  r = If[rads === {}, 0, First[rads]];
  psi4 = ReadPsi4[sim, 2, 2, r];
  WaveformCycles[psi4, start]
];



(*--------------------------------------------------------------------
  Reading Psi4 from an external provider
  --------------------------------------------------------------------*)

If[ProviderPreferences["Waveforms"] === {},
   ProviderPreferences["Waveforms"] = {"NRDF", "MultipoleHDF5", "MultipoleASCII"}];

ReadPsi4Data[runName_String, l_?NumberQ, m_?NumberQ, rad_] :=
  CallProvidedFunction["Waveforms","ReadPsi4Data",{runName,l,m,rad}];

ReadPsi4RadiiStrings[runName_] :=
  CallProvidedFunction["Waveforms","ReadPsi4RadiiStrings",{runName}];

ReadPsi4Modes[runName_] :=
  Select[CallProvidedFunction["Waveforms","ReadPsi4Modes",{runName}], #[[1]] >= 2 &];

(*--------------------------------------------------------------------
  Operations on Psi4
  --------------------------------------------------------------------*)

DocumentationBuilder`SymbolDescription["ReadPsi4"] =
 "read Psi4 waveform";

DefineMemoFunction[
  ReadPsi4[runName_String, l_?NumberQ, m_?NumberQ, rad_:Automatic],
  Module[{radii, radString},

  (* Get a list of radii available in the form expr -> "exprstring" *)
  radii = ReadPsi4RadiiStrings[runName];
  radii = Thread[radiiStringsToExpressions[radii] -> radii];

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
      radString = First[Nearest[Select[radii,NumericQ[#[[1]]] &], rad]];
      If[Abs[ToExpression[radString] - ToExpression[rad]] > 0.01,
         Error[Psi4RadiusNotFound, "Radius "<>ToString[rad]<>" not found.  Available radii are "<>ToString[radii[[All,1]]]]],
    True,
      Error["Radius "<>ToString[rad]<>" not found."];
  ];

  AddAttribute[ReadPsi4Data[runName, l, m, radString],RunName -> runName]]];

(* Return a list of radii available *)
radiiStringsToExpressions[radii_] := ToExpression /@ (radii /. "inf" -> "Infinity");
ReadPsi4Radii[runName_] := radiiStringsToExpressions[ReadPsi4RadiiStrings[runName]];

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

(* TODO: this does not work because SpinWeightedSphericalHarmonic is not found *)
ReadReconstructedPsi4[sim_, t_, rad_: Automatic] :=
 Module[{modes, psi4modes, harmonics, cf, psi4},
  modes = ReadPsi4Modes[sim];
  psi4modes = Interpolation[ReadPsi4[sim, #[[1]], #[[2]], rad]][t] & /@ modes;
  harmonics[th_, ph_] := SpinWeightedSphericalHarmonic[-2, #[[1]], #[[2]], th, ph] & /@ modes;
  cf=Compile[{{th, _Real}, {ph, _Real}}, Evaluate[Plus @@ (psi4modes.harmonics[th, ph])]];
  psi4[th_?NumericQ, ph_?NumericQ] := cf[th, ph];
  psi4
];

(* TODO: this does not work because SpinWeightedSphericalHarmonic is not found *)
ReadSchmidtAngle[run_, t_, rad_: Automatic] := 
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

SimulationTools`ArgumentChecker`StandardDefinition[ExtrapolateScalarFull] = True;

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

SimulationTools`ArgumentChecker`StandardDefinition[ExtrapolateScalar] = True;
ExtrapolateScalar[args__] :=
 ExtrapolatedValue /. ExtrapolateScalarFull[args];

SimulationTools`ArgumentChecker`StandardDefinition[ExtrapolateScalarWithRadii] = True;
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

Options[ExtrapolateDataTables] = {"Resample" -> False};

ExtrapolateDataTables[p_Integer, rdTb : {{_, DataTable[__]} ...}, opts___] :=
 Module[{trd, rds, ts, extraps},
  trd = RadiusTimeDataToTimeRadiusData[rdTb];
  rds = Map[Last,trd];
  ts = Map[First,trd];
  (* Print["t = ", First[ts]]; *)
  If[p > Length[rdTb]-1,
    Error["Insufficient radii (", Length[rdTb], ") for extrapolation at order ", p]];
  (* Print["order = ", p]; *)
  (* Print["rs = ", Map[First,rdTb]]; *)

  extraps = Map[ExtrapolateScalar[p, #] &, rds];
  (* Print[{rds[[1]], extraps[[1]]}]; *)
  
  MakeDataTable[MapThread[List, {ts, extraps}]]];

ExtrapolateDataTables[p_Integer, rdTb : {{_, DataTable[__]} ...}, {rMin_, rMax_}, opts___] :=
  ExtrapolateDataTables[p,Select[rdTb, #[[1]] >= rMin && #[[1]] <= rMax &], opts];


Options[AlignedPhases] = {"Continuous" -> False};

DocumentationBuilder`OptionDescriptions["AlignedPhases"] = {
  Continuous -> "Whether to allow continuous shifts in alignment. Setting this to "<>
    "False forces alignement to be done in multiples of 2 Pi only."
};

AlignedPhases[phaseTbs:{DataTable[__] ...}, t_, opts:OptionsPattern[]] :=
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

    (* Print["fTbs ranges: ", DataTableRange /@ fTbs//FullForm]; *)

    fTbsRes = 
         If[OptionValue[NonUniformGrid],
            Module[
              {highRes,res},
              highRes = fTbs[[Ordering[Spacing /@ fTbs][[1]]]];
              (* Print["spacings: ", Spacing/@fTbs//FullForm]; *)
              (* Print["ordering = ", Ordering[Spacing /@ fTbs][[1]]]; *)
              (* Print["ref: ", DataTableRange[highRes], " ", Spacing[highRes]]; *)
              res = ResampleDataTable[#, highRes] & /@ fTbs;
              (* Print["resampled ranges: ", DataTableRange/@res//FullForm]; *)
              IntersectDataTables[res]],
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

DefineMemoFunction[ReadExtrapolatedPsi4[runName_String, l_, m_, opts:OptionsPattern[]],
  Module[{phase,amp,psi4},
    phase = ExtrapolatePsi4Phase[runName, l, m, opts];
    amp = ExtrapolatePsi4Amplitude[runName, l, m, opts];
    psi4 = MapThreadData[#1 Exp[I #2] &, {amp, phase}];
    psi4]];

DefineMemoFunction[ReadExtrapolatedStrain[runName_String, l_, m_, om_, opts:OptionsPattern[]],
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

SimulationTools`ArgumentChecker`StandardDefinition[ffi] = True;

(* Note that these expect input in Fourier space *)
ffi[{f_, d_}, f0_] :=
 Module[{div},
  div = 2. Pi I If[f>0., Max[f, f0, $MachineEpsilon], Min[f, -f0, -$MachineEpsilon]];
  d/div
];

(* ffiDataTable[d_DataTable, f0_] := *)
(*   MapList[ffi[#, f0 / (2. Pi)] &, d]; *)

(* This version is about 10 times faster (0.28s -> 0.026s) than the
   original and gives the same answer in test cases. *)
ffiDataTable[d_DataTable, f0_] :=
 Module[{fs, fs2, gs, f1, mask1, mask2, mask},
  f1 = f0/(2. Pi);
  fs = IndVar[d];
  gs = DepVar[d];
  mask1 = (Sign[(fs/f1) - 1.] + 1.)/2.;
  mask2 = (Sign[(-fs/f1) - 1.] + 1.)/2.;
  mask = 1. - (1. - mask1) (1. - mask2);
  fs2 = mask fs + (1. - mask) f1 Sign[fs - $MachineEpsilon];
  MakeDataTable@Thread[{fs, gs/(2. Pi I fs2)}]];

FixedFrequencyIntegrate[q_DataTable, f0_?NumericQ] :=
 Module[{qUniform,t0,qTilde,intqTilde,intq,
         uniform = UniformGridQ[q]},
  qUniform = If[uniform, q, MakeUniform[q]];
  t0 = DataTableRange[qUniform][[1]];
  qTilde = Fourier[qUniform];
  intqTilde = ffiDataTable[qTilde, f0];
  intq = InverseFourier[intqTilde,t0];
  If[uniform, intq, Quiet[ResampleDataTable[intq,q,Intersect->False],
                           InterpolatingFunction::dmval]]];

Psi4ToStrain[psi4_DataTable, f0_?NumericQ] :=
 Module[{psi4Uniform, psi4f, dhf, hf, dh, h,
         uniform = UniformGridQ[psi4], t0},
  psi4Uniform = If[uniform, psi4, MakeUniform[psi4]];
  t0 = DataTableRange[psi4Uniform][[1]];
  psi4f = Fourier[psi4Uniform];
  dhf = ffiDataTable[psi4f, f0];
  hf = ffiDataTable[dhf, f0];
  {h, dh} = InverseFourier[#,t0] & /@ {hf, dhf};
  If[uniform, h, ResampleDataTable[h,psi4]]];

Psi4ToStrain[psi4_DataTable, range:(_List | All)] :=
  Module[{range2},
    range2 = If[range === All,
     DataTableRange[psi4],
     range];
    StrainFromPsi4[psi4, range2[[1]], range2[[2]]]
  ];

Psi4ToStrain[psi4_DataTable, fitStart_, fitEnd_] :=
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

AlignMaxima[ds_List] :=
  Module[{maxima},
   maxima = Map[LocateMaximum, ds];
   MapThread[ShiftDataTable[-#1, #2] &, {maxima, ds}]];

AlignMaximaOfAbs[ds_List] :=
  Module[{maxima},
   maxima = Map[LocateMaximum, Abs /@ ds];
   MapThread[ShiftDataTable[-#1, #2] &, {maxima, ds}]];

(* Import a gzipped file by copying it to a randomly-named file in the
   temp directory.  This avoids the problem that the gzip reader
   creates temporary files named after the original file, which causes
   problems when a number of such processes are running in parallel
   with similarly-named files. *)
SafeImportGzip[file_String, as_] :=
  Module[
    {id,tempfile,data},
    id = IntegerString[RandomInteger[{1, 10^64}], 16]<>".gz";
    tempfile = FileNameJoin[{$TemporaryDirectory,id}];
    CopyFile[file, tempfile];
    data = Import[tempfile,as];
    DeleteFile[tempfile];
    data];

ImportGzip[file_String, as_] :=
  (* ICH: My version of h5mma contains a GZIP file reader because the
     Mathematica reader has many problems. It shouldn't be in h5mma,
     and should eventually move into another package.  Fall back to
     using Import if the h5mma function is not available.  This should
     work as long as usage is light. *)
  If[DownValues[h5mma`ReadGzipFile] =!= {},
    ImportString[h5mma`ReadGzipFile[file], as],
    SafeImportGzip[file,as]];

ImportWaveform[file_] :=
  Module[
    {data, lockFile, waitForLock, releaseLock},
    If[FileType[file] ===None,
       Error["ImportWaveform: File "<>file<>" does not exist"]];

    If[FileExtension[file] === "gz",
       data = ImportGzip[file,"Table"],
       data = Import[file,"Table"]];
    Return[MakeDataTable[Select[Map[{#[[1]],#[[2]]+I #[[3]]}&, data], NumberQ[#[[2]]]&]]];
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

ExportWaveform[file_String, d_DataTable] :=
  Export[file, Map[{#[[1]], Re[#[[2]]], Im[#[[2]]]} &, ToList[d]], "Table"];

(****************************************************************)
(* Simulation Overview                                          *)
(****************************************************************)

SimulationTools`Waveforms`SimulationOverview`Plots[runNames1_] :=
  Module[{runNames, rs, r},
    runNames = Select[runNames1, HaveData["Waveforms",#] &];
    If[runNames === {},
       None,
       rs = Intersection@@(ReadPsi4Radii/@runNames);
       (* TODO: This should probably be a plot rather than an error *)
       If[rs === {}, Error["No common radii in simulations "<>ToString[runNames]]];
       r = First[rs];
       {{PresentationListLinePlot[
         Map[Re[ReadPsi4[#, 2, 2, r]]&, runNames],
         PlotRange -> All, PlotLabel -> "Re[Psi422], R = "<>ToString[r],
         PlotLegend -> runNames],
         PresentationListLinePlot[
           (Log10@Abs@ReadPsi4[#, 2, 2, r] &) /@ runNames,
           PlotRange -> All, PlotLabel -> "|Psi422|, R = "<>ToString[r],
           PlotLegend -> runNames,
           FrameTicks -> {{Table[{x,Superscript[10,x]}, {x,-10,10,2}],None},{Automatic,None}}]}}]];

Psi4RePlot[sims1_List] :=
  Module[{r = 100, sims = Select[sims1, HaveData["Waveforms",#] &]},
  Association["Plot" ->
    PresentationListLinePlot[
      Map[Re[Shifted[r ReadPsi4[#, 2, 2, r], -r]] &, sims],
      PlotRange -> All,
      PlotLegend -> sims],
    "Filename" -> "repsi4",
    "Title" -> "Re[Psi422], r = "<>ToString[r]]];

(****************************************************************)
(* Extrapolation                                                *)
(****************************************************************)

extrapOrderPattern = _Integer | {_Integer, _Integer};

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

    model = Sum[a[i] x^i, {i, 0, order}];
    fit = FindFit[xf, model, Table[a[i], {i, 0, order}], x];
    Return[a[0] /. fit];

    Fit[xf, Table[x^i, {i, 0, order}], {x}][[1]]];

RadialExtrapolation[{rs_List, fs:{_DataTable...}}, order_Integer] :=
  Module[
    {t, ds, rds, de},

    (* Print["order = ", order]; *)
    (* Print["rs = ", Short[rs]]; *)

    Assert[Apply[And,SimulationTools`DataTable`Private`validQ/@fs]];
    If[Length[rs] =!= Length[fs], Error["The number of radii and the number of DataTables are not the same"]];
    If[Length[rs] === 0, Error["Cannot extrapolate an empty list of DataTables"]];

    If[order===0,
       Return[fs[[Ordering[rs][[-1]]]]]];

    (* These checks do not seem to affect performance *)
    If[!SameGridQ@@fs,
       Error["RadialExtrapolation: Input DataTables are not defined on the same grid"]];
    If[!And@@Map[NumberQ,rs],
       Error["RadialExtrapolation: Input radii are not numeric"]];

    t = ToListOfCoordinates[fs[[1]]];
    (* Print["t = ", First[t]]; *)
    ds = ToListOfData/@fs;
    (* Print["ds = ", Short[ds]]; *)
    rds = Transpose[ds];
    (* Print["rds = ", Short[rds]]; *)
    (* Print["Before RadialExtrapolation"]; *)
    de = Map[RadialExtrapolation[{rs, #}, order] &,rds];
    (* Print[{Thread[{rs,rds[[1]]}], de[[1]]}]; *)
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
  Shifted[f, -rStarOfr[r]];

ToRetardedTime[{rs:{_?NumericQ...}, fs:{_DataTable...}}, rStarOfr_:Identity] :=
  MapThread[ToRetardedTime[#1,#2,rStarOfr] &, {rs,fs}];

resampleDataTables[ds:{DataTable[__]...}] :=
  Map[Resampled[#, RestrictedToInterval[First[ds], First[CommonInterval[ds]]]] &, ds];

(* RadiallyExtrapolatedWave *)

notOptionQ[x_] := ! OptionQ[x];
Options[RadiallyExtrapolatedWave] = {"AbsPhase" -> True,
                                     "DiscretePhaseAlignmentTime" -> Automatic};
RadiallyExtrapolatedWave[{rs_List, fs:{_DataTable...}}, 
                         order : extrapOrderPattern,
                         rStarOfr : (_?notOptionQ) : Identity, 
                         opts:OptionsPattern[]] :=
  Module[
    {ret,ext,tAlign,orders,resampled},

    resampled =
    If[$UniformGridExtrapolation === True,
       SimulationTools`DataTable`Private`resampled,
       resampleDataTables];

    ret = ToRetardedTime[{rs,fs},rStarOfr];
    (* Print["ret = ", ret]; *)
    (* Print["Max[CoordinateRange[First[ret]][[1]],0] = ", Max[CoordinateRange[First[ret]][[1]], 0]]; *)
    ext[ds_,p_] := RadialExtrapolation[{rs, ds}, p];

    (* Print["rs = ", rs]; *)

    If[OptionValue[AbsPhase],
       tAlign = If[OptionValue[DiscretePhaseAlignmentTime] === Automatic,
                   Max[CoordinateRange[First[ret]][[1]], 0],
                   OptionValue[DiscretePhaseAlignmentTime]];
       orders = Switch[order, _Integer, {order, order}, {_Integer, _Integer}, order, _, Error["Error"]];
       
       ToComplex[MapThread[ext, {resampled/@ToAbsPhase[ret,tAlign], orders}]],
       (* else *)
       If[ListQ[order], Error["Can only specify a list of extrapolation orders when AbsPhase is True"]];
       ext[resampled[ret],order]]];

(* ReadRadiallyExtrapolatedWave *)

Options[ReadRadiallyExtrapolatedWave] =
  Join[{"RadialCoordinateTransformation" -> None},
       Options[RadiallyExtrapolatedWave]];

ReadRadiallyExtrapolatedWave[run_String, reader_, rads_List,
                             order : extrapOrderPattern, opts:OptionsPattern[]] :=
  If[Length[rads] ===0,
     Error["ReadRadiallyExtrapolatedWave: No waveform radii supplied for run "<>run],
     (* else *)
     RadiallyExtrapolatedWave[
       Transpose[
         Table[{r,reader[r]}, {r,rads}]],
       order, 
       OptionValue[RadialCoordinateTransformation] /.
       {None -> Identity,
        RadialToTortoise -> (RadialToTortoise[#,ReadADMMass[run]] &),
        IsotropicToTortoise -> (IsotropicToTortoise[#,ReadADMMass[run]] &)},
       FilterRules[{opts}, Options[RadiallyExtrapolatedWave]]]];

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
        "Radii" -> Automatic,
        "PerturbativeAdjustment" -> False}];
DocumentationBuilder`OptionDescriptions["ReadRadiallyExtrapolatedPsi4"] = {
  "AbsPhase" -> "Whether to separately extrapolate magnitude and phase.",
  "DiscretePhaseAlignmentTime" -> "Time at which to align phases before doing extrapolation.",
  "PerturbativeAdjustment" -> "Whether to include a perturbative correction during extrapolation.",
  "RadialCoordinateTransformation" -> "Coordinate transformation to apply to radius used in "<>
    "extrapolation. Possible choices are None (equivalent to Identity), RadialToTortoise "<>
    "or IsotropicToTortoise",
  "Radii" -> "List of radii to use in extrapolation.",
  "RadiusRange" -> "Range of radii to use in extrapolation given as a list of the form {rmin, rmax}."
};

DefineMemoFunction[ReadRadiallyExtrapolatedPsi4[run_String, l_Integer, m_Integer,
                                                order : extrapOrderPattern, opts:OptionsPattern[]],
  ReadRadiallyExtrapolatedWave[
    run,
    (If[OptionValue[PerturbativeAdjustment],
        Function[rpsi4,Psi4PerturbativeCorrection[rpsi4,l,#,om0]], (* TODO: pass in om0 *)
        Identity][# ReadPsi4[run,l,m,#]]) &,
    selectRadii[ReadPsi4Radii[run],
                OptionValue[RadiusRange], OptionValue[Radii]],
    order, FilterRules[{opts},Options[ReadRadiallyExtrapolatedWave]]]];

(* ReadRadiallyExtrapolatedStrain *)

(*
  - Not all modes are suitable for FFI (e.g. the 2,0 mode is non-oscillatory
  - The user should be able to select the strain conversion method
  - Some simulations might have CCE or other precomputed strain data; we should allow
    the user to choose
*)

Options[ReadRadiallyExtrapolatedStrain] = Options[ReadRadiallyExtrapolatedPsi4];
DocumentationBuilder`OptionDescriptions["ReadRadiallyExtrapolatedStrain"] =
  DocumentationBuilder`OptionDescriptions["ReadRadiallyExtrapolatedPsi4"];
DefineMemoFunction[ReadRadiallyExtrapolatedStrain[run_String, l_Integer, m_Integer, om0_,
                               order : extrapOrderPattern, opts:OptionsPattern[]],
  ReadRadiallyExtrapolatedWave[
    run,
    Psi4ToStrain[If[OptionValue[PerturbativeAdjustment],
        Function[rpsi4,Psi4PerturbativeCorrection[rpsi4,l,#,om0]],
        Identity][# ReadPsi4[run,l,m,#]], om0] &,
    selectRadii[ReadPsi4Radii[run],
                OptionValue[RadiusRange], OptionValue[Radii]],
    order, FilterRules[{opts}, Options[ReadRadiallyExtrapolatedWave]]]];

(****************************************************************)
(* Psi4PerturbativeCorrection                                   *)
(****************************************************************)

Psi4PerturbativeCorrection[rpsi4_DataTable, l_Integer, r_?NumberQ, om0_?NumberQ] :=
  rpsi4 - 1/2 (l - 1) (l + 2) FixedFrequencyIntegrate[rpsi4/r, om0];

(****************************************************************)
(* ReadStrainCPM                                                *)
(****************************************************************)

ReadStrainCPM[sim_String, l_, m_, r_] :=
 Module[{components, psiEvenRe, psiEvenIm, psiOddRe, psiOddIm},
  components = 
   Table["Psi_" <> x <> "_Detector_Radius_" <> ToString[r] <> 
     ".00_l" <> ToString[l] <> "_m" <> ToString[m] <> 
     ".asc", {x, {"even_Re", "even_Im", "odd_Re", "odd_Im"}}];
  {psiEvenRe, psiEvenIm, psiOddRe, psiOddIm} = 
   ToDataTable[ReadColumnFile[sim, #]] & /@ components;

  (* TODO: This -1/4 seems necessary to make it agree with int int psi4 *)
  -1/4 1/(2 r) Sqrt[
    Factorial[l + 2]/Factorial[l - 2]] (psiEvenRe + I psiEvenIm + 
     I (psiOddRe + I psiOddIm))];

readHDF5Table[sim_String, file_String, dataset_String] :=
  Module[{files,dataSegments,data},
    files = FindSimulationFiles[sim, file];
    If[files === {}, Error["File "<>file<>" not found in simulation "<>sim]];
    dataSegments = Map[Check[ReadHDF5[#,{"Datasets", dataset}],Error["Failed to read dataset "<>#<>" from " <> filename <> " in simulation "<>sim],
     {h5mma::mlink}] &, files];
    data = MergeFiles[dataSegments]];

readHDF5Datasets[sim_String, file_String] :=
  Module[{files,dataSegments,data},
    files = FindSimulationFiles[sim, file];
    If[files === {}, Error["File "<>file<>" not found in simulation "<>sim]];
    dataSegments = Map[ReadHDF5[#,{"Datasets"}] &, files[[1;;1]]];
    data = MergeFiles[dataSegments]];

ReadStrainCPMHDF5[sim_String, l_, m_, r_] :=
  Module[{components, psiEvenRe, psiEvenIm, psiOddRe, psiOddIm, data,filename},
    filename = If[FindSimulationFiles[sim,"wavextractcpm.h5"] =!= {},
      "wavextractcpm.h5",
      "waveextractcpm.h5"];
      
  components = 
   Table["Psi_" <> x <> "_Detector_Radius_" <> ToString[NumberForm[r,{Infinity,2}]] <> 
     "_l" <> ToString[l] <> "_m" <> ToString[m],
     {x, {"even_Re", "even_Im", "odd_Re", "odd_Im"}}];
   data = {psiEvenRe, psiEvenIm, psiOddRe, psiOddIm} = 
   ToDataTable[Check[readHDF5Table[sim, filename, #],
     Error["Failed to read dataset "<>#<>" from " <> filename <> " in simulation "<>sim],
     {h5mma::mlink}]] & /@ components;
   
  (* TODO: This -1/4 seems necessary to make it agree with int int psi4 *)
  -1/4 1/(2 r) Sqrt[
    Factorial[l + 2]/Factorial[l - 2]] (psiEvenRe + I psiEvenIm + 
     I (psiOddRe + I psiOddIm))];

ReadStrainCPMHDF5Direct[sim_String, l_, m_, r_] :=
  Module[{components, data, hRe, hIm, filename},
    filename = If[FindSimulationFiles[sim,"wavextractcpm.h5"] =!= {},
      "wavextractcpm.h5",
      "waveextractcpm.h5"];
      
  components = 
   Table["h_" <> x <> "_Detector_Radius_" <> ToString[NumberForm[r,{Infinity,2}]] <> 
     "_l" <> ToString[l] <> "_m" <> ToString[m],
     {x, {"Re", "Im"}}];

   data = {hRe, hIm} = 
   Check[ToDataTable[readHDF5Table[sim, filename, #]],
     Error["Failed to read dataset "<>#<>" from " <> filename <> " in simulation "<>sim],
     {h5mma::mlink}] & /@ components;
   
    1/r(hRe + I hIm)];

ReadStrainRadiiWaveExtractCPMHDF5[sim_String] :=
  Module[{x, datasets},
    datasets = readHDF5Datasets[sim, "waveextractcpm.h5"];
    radii = Sort[(Union@@StringCases[datasets, "_Detector_Radius_" ~~ x : (NumberString|"inf") :> ToExpression@x])]];

ReadStrainWaveExtract[sim_String, l_, m_, r_] :=
 Module[{components, QEvenRe, QEvenIm, QOddRe, QOddIm, intQOdd, Qs},
  components = 
   Table["Q" <> x <> "_Detector_Radius_" <> ToString[NumberForm[r,{Infinity,2}]] <> "_l" <> 
     ToString[l] <> "_m" <> ToString[m] <> 
     ".asc", {x, {"even_Re", "even_Im", "odd_Re", "odd_Im"}}];
  Qs = {QEvenRe, QEvenIm, QOddRe, QOddIm} = 
    ToDataTable[ReadColumnFile[sim, #]] & /@ components;
  
  intQOdd = 
   AntiDerivative[QOddRe + I QOddIm, {MinCoordinate[QOddRe], 0}, 
    UseInputGrid -> True];
  (*This -1 seems necessary to make it agree with int int psi4 *)
  -1/(Sqrt[2] r) (QEvenRe + I QEvenIm - I intQOdd )];

(* Generic interface to reading strain data from a simulation.
   Currently only supports WaveExtractCPM HDF5 format. *)
ReadStrain[sim_String, l_, m_, r_, ord_ : 2, opts___] :=
  Replace[r, {
    _?NumberQ :>
    (* TODO: generalise this to other methods *)
    ReadStrainCPMHDF5Direct[sim, l, m, r],
    
    Infinity :>
    ReadSXSStrain[sim, l, m, ord, opts],

    _ :>
    Error["ReadStrain: Unrecognised radius "<>ToString[r]]}];

ReadStrainRadii[sim_String] :=
  ReadStrainRadiiWaveExtractCPMHDF5[sim];

ReadSchwarzschildRadius[sim_String, r_] :=
 Module[{},
  ToDataTable[
   ReadColumnFile[sim, 
    "Schwarzschild_Radius_Detector_Radius_" <> ToString[NumberForm[r,{Infinity,2}]] <> ".asc"]]];

ToSchwarzschildRetardedTime[rSch:(_DataTable|_?NumberQ), f_DataTable, MADM_] :=
  ToDataTable[Coordinate[f] - RadialToTortoise[rSch, MADM], f];

ReadWaveExtractRadii[sim_String] :=
  Module[{},
    Union[ToExpression[StringSplit[FileNameTake[#,-1],"_"][[5]]] & /@ FindSimulationFiles[sim,"Qeven_Re_Detector_Radius_*_l*_m*.asc"]]]

ReadSchwarzschildRadiusHDF5[sim_String, r_] :=
  Module[{components, psiEvenRe, psiEvenIm, psiOddRe, psiOddIm, data, filename, dataset},
    filename = "waveextractcpm.h5";
    dataset = "Schwarzschild_Radius_Detector_Radius_" <> ToString[NumberForm[r,{Infinity,2}]] <> "";  
    Check[ToDataTable[readHDF5Table[sim, filename, dataset]],
     Error["Failed to read dataset "<>#<>" from " <> filename <> " in simulation "<>sim],
     {h5mma::mlink}]];



(****************************************************************)
(* Psi4 phase errors                                            *)
(****************************************************************)

Psi4PhaseErrors[sims:{___String}] :=
  Module[{psi4s, phis, hs, phiErrs},
    If[Length[sims] < 2, Return[{}]];
    psi4s = Shifted[100 ReadPsi4[#, 2, 2, 100], -100] & /@ sims;
    phis = AlignPhases[Phase/@psi4s,200];
    hs = ReadCoarseGridSpacing /@ sims;
    Quiet[phiErrs = 
    Prepend[Table[(phis[[i]] - 
      phis[[i + 1]]) hs[[i+1]]^8/(hs[[i]]^8 - hs[[i + 1]]^8), {i, 1, 
        Length[sims] - 1}],(phis[[1]] - 
      phis[[2]]) hs[[1]]^8/(hs[[1]]^8 - hs[[2]]^8)],InterpolatingFunction::dmval] // WithResampling];

resOfSim[s_String]:=
  StringReplace[SimulationResolutionLabelFromName[s],{"Lev"->"", "n="->""}];

Psi4PhaseErrorPlot[sims:{__String}, opts:OptionsPattern[]] :=
  Psi4PhaseErrorPlot[Psi4PhaseErrors[sims], opts, Resolutions -> (resOfSim/@sims)];

(* TODO: this is duplicated from OrbitalPhaseErrorPlot *)
Options[Psi4PhaseErrorPlot] = {"Resolutions" -> Automatic};
Psi4PhaseErrorPlot[phiErrs:{___DataTable}, opts:OptionsPattern[]] :=
  Module[{legend},
    legend = If[OptionValue[Resolutions] === Automatic,
      None,
      Map[Subscript["\[CapitalDelta]\[Phi]",#] &, OptionValue[Resolutions]]];

    PresentationListLinePlot[Log10 /@ Abs /@ phiErrs, FilterRules[opts,Options[PresentationListLinePlot]], 
      PlotRange -> {{0, All}, {-5, 2}}, Axes -> None,
      PlotLegend -> legend,
      LegendPosition -> {Right, Bottom}, GridLines -> Automatic]];

(****************************************************************)
(* Waveform phase errors                                        *)
(****************************************************************)

WaveformPhaseErrors[sims:{_String..}] :=
  Module[{},
    WaveformPhaseErrors[ReadStrain[#,2,2,Infinity,2] & /@ sims,
      SimulationResolutionFromName/@sims]];

WaveformPhaseErrors[waveforms:{_DataTable..}, resolutions:{_Association..}] :=
  Module[{phis, method, phiErrs, hs, p=8},
    If[Length[waveforms] < 2, Return[{}]];
    phis = AlignPhases[Phase/@waveforms,200];
    
    method = resolutions[[1]]["Method"];

    Which[
      method === "FiniteDifference",
      hs = 1/resolutions[[All,"NumberOfCells"]];
      
      Quiet[phiErrs = 
        Prepend[Table[(phis[[i]] - phis[[i + 1]]) hs[[i+1]]^p/(hs[[i]]^p - hs[[i + 1]]^p),
          {i, 1, Length[waveforms] - 1}],
          (phis[[1]] - phis[[2]]) hs[[1]]^p/(hs[[1]]^p - hs[[2]]^p)],
        InterpolatingFunction::dmval] // WithResampling,

      method === "Spectral",
      levs = resolutions[[All,"Level"]];

      If[Union[Differences[Sort[levs]]] =!= {1},
        Error["Cannot compute errors when levels are not adjacent"]];

      (* TODO: Figure out how to determine the expected convergence
         rate; we are assuming it is e here. *)

      Quiet[phiErrs = 
        Append[Table[(phis[[i]] - phis[[i + 1]]),
          {i, 1, Length[waveforms] - 1}],
          Exp[-1] (phis[[-2]] - phis[[-1]])],
        InterpolatingFunction::dmval] // WithResampling,

    True,
    Error["WaveformPhaseErrors: Unrecognised numerical method "<>ToString[method]]]];

Options[WaveformPhaseErrorPlot] = {"Resolutions" -> Automatic, "Radius" -> Infinity};

WaveformPhaseErrorPlot[sims:{__String}, opts:OptionsPattern[]] :=
  WaveformPhaseErrorPlot[WaveformPhaseErrors[sims], opts, Resolutions -> (resOfSim/@sims)];

WaveformPhaseErrorPlot[phiErrs:{___DataTable}, opts:OptionsPattern[]] :=
  Module[{legend},
    legend = If[OptionValue[Resolutions] === Automatic,
      None,
      Map[Subscript["\[CapitalDelta]\[Phi]",#] &, OptionValue[Resolutions]]];

    PresentationListLinePlot[Log10 /@ Abs /@ phiErrs, FilterRules[{opts},Options[PresentationListLinePlot]], 
      PlotRange -> {{0, All}, {-5, 2}}, Axes -> None,
      If[legend =!= None, PlotLegend -> legend, {}],
      LegendPosition -> {Right, Bottom}, GridLines -> Automatic]];

(****************************************************************)
(* StrainPlot                                                   *)
(****************************************************************)

StrainPlot[simsp : {_String ...}] :=
  Module[{strains, sims},
    (* Only currently supports simulations which have exported data *)
    (* Include only those simulations for which we have exported strain data *)
    sims = Select[simsp, 
      FileExistsQ[
        FileNameJoin[{FindRunDir[#], "exported", 
          "rhOverM_Asymptotic_GeometricUnits.h5"}]] &];
    strains = 
    ReadSXSStrain[FileNameJoin[{#, "exported"}], 2, 2, 2] & /@ sims;
    StrainPlot[strains, FileNameTake[#, -1] & /@ sims]];

StrainPlot[strains:{_DataTable...}, labels_List] :=
  PresentationListLinePlot[Re /@ strains, PlotRange -> All, 
    PlotLegend -> labels];

StrainPlot[strains : {_DataTable ...}, labels : {_String ...}] :=
  PresentationListLinePlot[Re /@ strains, PlotLegend -> labels];

(****************************************************************)
(* StrainPhaseErrorPlot                                         *)
(****************************************************************)

StrainPhaseErrorPlot[simsp : {_String ...}] :=
  Module[{strainFile, sims, strains},

    strainFile[sim_] :=
    Replace[Select[{FileNameJoin[{FindRunDir[sim],"rhOverM_Asymptotic_GeometricUnits.h5"}],
      FileNameJoin[{FindRunDir[sim],"exported","rhOverM_Asymptotic_GeometricUnits.h5"}]},
      FileExistsQ],{{}:>None,l_:>First[l]}];

    sims = Select[simsp, strainFile[#] =!= None &];

    strains = 
    ReadSXSStrain[#, 2, 2, 2] & /@ sims;

    StrainPhaseErrorPlot[strains, FileNameTake[#, -1] & /@ sims]];

firstOrError[l_] :=
  If[Length[l] === 0, Error["Empty list"], First[l]];

firstOrNone[l_] :=
  If[Length[l] === 0, None, First[l]];

readResolution[sim_String] :=
  firstOrNone@Join[
    StringCases[FileNameTake[sim,-1], "n"~~(n:NumberString) :> ToExpression[n]],
    StringCases[FileNameTake[sim,-1], __~~"_"~~(n:NumberString) :> ToExpression[n]]]

StrainPhaseErrorPlot[strains : {_DataTable ...}, labels : {_String ...}] :=
  Module[{ns, errs},
    ns = readResolution/@labels;
    errs = WaveformPhaseErrors[strains, 1/ns];
    WaveformPhaseErrorPlot[errs, Resolutions -> ns]];

(****************************************************************)
(* WaveformPlot                                                 *)
(****************************************************************)

WaveformPlot[simsp:{_String...}] :=
  Module[{strainFile, sims, strains, legend},
    sims = simsp;
    strains = ReadStrain[#, 2, 2, Infinity, 2] & /@ sims;
    legend = SimulationResolutionLabelFromName /@ sims;
    Association[
      "Plot" -> WaveformPlot[strains, PlotLegend -> legend],
      "Title" -> "Strain (l=2,m=2)",
      "Filename" -> "WaveformPlot"]];

Options[WaveformPlot] = Options[PresentationListLinePlot];
WaveformPlot[waveforms:{_DataTable...}, opts:OptionsPattern[]] :=
  Module[{tPeak, peakRange},
    If[Length[waveforms] === 0, Error["WaveformPlot: empty waveform list"]];
    tPeak = LocateMaximum[Abs[waveforms[[-1]]]];
    peakRange = tPeak+{-100,100};
    GraphicsRow[MapThread[Function[{pr},
      Show@@MapThread[Function[{fn,styles},
        PresentationListLinePlot[fn /@ waveforms, PlotRange -> pr, ImageSize->350,
          GridLines -> {peakRange, None}, PlotStyle -> styles,
          FilterRules[{opts},Options[PresentationListLinePlot]]]],
        {{Re,Abs},{PresentationPlotStyles,PresentationPlotStyles}}]],
      {{{All,All},{peakRange,All}}}]]];
End[];

EndPackage[];
