(* ::Package:: *)

(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

(* A package for dealing with numerical relativity data *)

BeginPackage["NR`", {"BHCoordinates`", "Convergence`", "DataRegion`", "DataTable`",
  "Horizons`", "Kicks`", "Memo`", "Parameters`", "Plotting`", "ReadHDF5`", "RunFiles`",
  "SystemStatistics`", "Timers`"}];

ReadADMMass::usage = "ReadADMMass[run] reads the total ADM mass of the spacetime in run as computed by the TwoPunctures thorn.";
ReadPunctureADMMasses::usage = "ReadPunctureADMMasses[run] reads the ADM masses of the punctures in run as computed by the TwoPunctures thorn.";
ReadPunctureADMMassParameters::usage  = "ReadPunctureADMMassParameters[run] reads the ADM masses of the punctures in run as requested by the target_M_plus and target_M_minus parameters of the TwoPunctures thorn.";
InitialPosition::usage = "InitialPosition[run, bh] returns a vector containing the initial coordinate position of BH numbered bh";


ReadHamiltonianConstraintNorm::usage = "ReadHamiltonianConstraintNorm[run] reads the norm of the Hamiltonian constraint in run.";
FilterDCT::usage = "FilterDCT[d, numModes, range1, range2] filters the data in d using a discrete fourier transform, allowing a maximum of numModes modes. Only data in range1 is used in filtering and only data in range2 is actually returned filtered.";

Data;
FitFunction;

ReadPunctureADMMasses;
ReadPunctureADMMasses2;
PercentageDifference;

ReturnValue;
FittedFunction;
Eccentricity;
FitEcc;
FitEccOm;
FitParameters;
ToFixedWidth;

LRange;

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

ReadTwoPuncturesData::usage = "ReadTwoPuncturesData[file, col] reads a data file output by the standalone TwoPunctures code by Marcus Ansorg and returns a DataRegion containing the data in column col. col can be 1, 2 or 3 for the coordinates or >= 4 for the data.";

Begin["`Private`"];

RunDirectory := Global`RunDirectory;



ReadADMMass[runName_String] :=
  Module[{massMDFiles, output, lines},
    massMDFiles = FindRunFile[runName, "ADM_mass_tot.asc"];
    If[massMDFiles =!= {},
      Return[ReadList[massMDFiles[[1]], Real][[1]]],
      (* Else *)
      output = StandardOutputOfRun[runName];
      If[Length[output] < 1,
        Throw["Cannot find standard output for run "<>runName]];

      lines = Select[ReadList[output[[1]], String], StringMatchQ[#, __ ~~ "total ADM mass is" ~~ __] &, 1];
      If[Length[lines] < 1,
        lines = Select[ReadList[output[[1]], String], StringMatchQ[#, __ ~~ "ADM mass is" ~~ __] &, 1];
        If[Length[lines] < 1,
        Throw["Cannot find ADM mass in standard output of run "<>runName]]];
      ToExpression@Last@StringSplit[lines[[1]]]]];


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


RunCost[length_, speed_, nProcs_] :=
 {CPUHours -> nProcs length/speed // N,
  WallTimeDays -> length/speed/24 // N};


PercentageDifference[ap_,bp_] :=
  Module[{a,b},
    {a,b} = Sort[{ap,bp}];
    100 Abs[a-b]/a];

ReadHamiltonianConstraintNorm[run_] :=
  ReadColumnFile[run, "ctgconstraints::hamiltonian_constraint.norm2.asc", {2,3}];


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

DefineMemoFunction[ReadPunctureADMMassParameters[run_String],
  ToExpression/@{LookupParameter[run, "TwoPunctures::target_M_plus"], 
   LookupParameter[run, "TwoPunctures::target_M_minus"]}];


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
 Plus @@ ReadPunctureADMMassParameters[run];

DefineMemoFunction[InitialSpinAngularMomentum[run_],
 Module[{sp, sm},
  sp = Table[
     ToExpression@
      LookupParameter[run, 
       "TwoPunctures::par_s_plus[" <> ToString[i] <> "]", "0"], {i, 0,
       2}];
  sm = Table[
     ToExpression@
      LookupParameter[run, 
       "TwoPunctures::par_s_minus[" <> ToString[i] <> "]", "0"], {i, 
      0, 2}];
  sp + sm]];

InitialLinearMomentum[run_, idx_] :=
  Module[{suffix},
    suffix = If[idx == 0, "plus", "minus"];
    Table[
      ToExpression@
        LookupParameter[run, "TwoPunctures::par_P_"<>suffix<>"["<>ToString@d<>"]", 0],
      {d,0,2}]];

InitialOrbitalAngularMomentum[run_] :=
 Module[{xp, xm, pyp, pym, initialL},
  xp = (ToExpression@LookupParameter[run, "TwoPunctures::par_b"] + 
      ToExpression@
       LookupParameter[run, "TwoPunctures::center_offset[0]"]);
  xm = (-ToExpression@LookupParameter[run, "TwoPunctures::par_b"] + 
      ToExpression@
       LookupParameter[run, "TwoPunctures::center_offset[0]"]);
  pyp = (ToExpression@
      LookupParameter[run, "TwoPunctures::par_P_plus[1]"]);
  pym = (ToExpression@
      LookupParameter[run, "TwoPunctures::par_P_minus[1]"]);
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
  {mp, mm} = ReadPunctureADMMassParameters[run];
  If[Norm@S0 > Norm@S1,
   S0/mp^2,
   S1/mm^2]]];

MassRatio[run_] :=
 Module[{mp, mm},
  {mp, mm} = ReadPunctureADMMassParameters[run];
  Return[If[mp < mm, mp/mm, mm/mp]]];

SymmetricMassRatio[run_] :=
 Module[{q = MassRatio[run]},
  q/(1 + q)^2];

DefineMemoFunction[InitialSeparation[run_],
  First@DepVar@ReadBHSeparation[run]];

DefineMemoFunction[InitialPosition[run_, bh_],
  First@DepVar@ReadBHCoordinates[run, bh]];

End[];

EndPackage[];
