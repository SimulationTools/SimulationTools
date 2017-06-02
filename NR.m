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

(* A package for dealing with numerical relativity data *)

BeginPackage["SimulationTools`NR`",
 {
  "SimulationTools`ColumnFile`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`Horizons`",
  "SimulationTools`Memo`",
  "SimulationTools`Parameters`",
  "SimulationTools`Plotting`",
  "SimulationTools`TwoPunctures`",
  "SimulationTools`ArgumentChecker`",
  If[$VersionNumber >= 10, "GeneralUtilities`", Unevaluated[Sequence[]]]
 }];

ReadHamiltonianConstraintNorm(*::usage = "ReadHamiltonianConstraintNorm[sim] reads the norm of the Hamiltonian constraint in sim."*);

Data;
FitFunction;
PercentageDifference;
ReturnValue;
FittedFunction;
Eccentricity;
FitEcc;
FitEccOm;
FitParameters;
ToFixedWidth;
LRange;
InitialDimensionlessSpin;
BinaryEccentricityFromSeparation;
AlignTimeAndPhaseOfPhases;
AlignTimeAndPhaseOfWaveforms;
TimeAndPhaseShiftOfPhase;

Begin["`Private`"];

(*--------------------------------------------------------------------
  Model fitting
  --------------------------------------------------------------------*)

Clear[FitFunction];

FitFunction[d_, f_, paramSpecs_, opts:OptionsPattern[]] :=
  FitFunction[d, f, paramSpecs, FindMinimum, Automatic, opts];

(* Take a table d of data from a simulation and work out values of a set
   of parameters ps which cause the data to match best to a given
   function f over a given interval tMin to tMax. *)
Options[FitFunction] = {"NMinimizeOptions" -> {}};
FitFunction[d_List, f_, paramSpecs_, method_, subMethod_, OptionsPattern[]] :=
  Module[{squareDiff,lastFitMessageTime, it, pList, pList2, p2, p3, paramSpecs2, fit, fit2, bestSqDiff=Infinity, bestParams={}, bestFn = Function[t, 0], paramHistory = {}, sqDiffHistory = {}},
    squareDiff[params__?NumberQ] :=
      Module[{fSoln, diffs, sqDiff, interval},
(*        FitFunctionParamPath = Append[FitFunctionParamPath, {it, params}];*)
        (* Print["squareDiff: ", {params}]; *)
        fSoln = f[params];
        If[fSoln === Indeterminate, Return[100]];
        interval = Last[d][[1]] - First[d][[1]];
        diffs = Map[#[[2]] - fSoln[#[[1]]] &, d];
        Map[If[!NumberQ[#], Return[100]] &, diffs];
        sqDiff = Sqrt[diffs . diffs / Length[diffs]];
        (* Print["sqDiff = ", sqDiff]; *)
(*        sqDiff = (diffs . diffs / Length[diffs]);*)
        paramHistory = Append[paramHistory, {params}];
        sqDiffHistory = Append[sqDiffHistory, sqDiff];
        (* TODO: don't use a global variable here *)
        Global`$FitHistory = Thread[{paramHistory, sqDiffHistory}];
        If[sqDiff < bestSqDiff,
          bestSqDiff = sqDiff;
          bestParams = {params};
          bestFn = fSoln;
          Global`$BestFit = {bestSqDiff, bestParams, bestFn}];

        If[SessionTime[] - lastFitMessageTime > 2,
          (* Print["Updating plot"]; *)
          Global`$FitStatusPlot = Show[
            ListLinePlot[d,Frame->True,PlotRange->{{d[[1,1]],d[[-1,1]]},{Min[d[[All,2]]],Max[d[[All,2]]]}}, PlotStyle->Directive[Gray,Thickness[0.002]],PlotLabel->Style[bestParams->bestSqDiff,Blue]],
            Plot[{bestFn[t],fSoln[t]},{t,d[[1,1]],d[[-1,1]]}, PlotStyle->{Directive[Dashed,Blue,Thickness[0.002]], Directive[Dashed,Darker@Green,Thickness[0.002]]}]];

          (* Global`$FitParamPlot = ListPointPlot3D[paramHistory, PlotRange -> All, *)
          (*   AxesLabel ->Map[First,paramSpecs], Filling -> Bottom]; *)

          (* Global`$FitParamPlot = Graphics3D[Map[{ColorData["TemperatureMap"][Log10[1/#[[2]]]], Point[#[[1]]]} &, Thread[{paramHistory, sqDiffHistory}]],PlotRange -> All, *)
          (*   AxesLabel ->Map[First,paramSpecs]]; *)

          lastFitMessageTime = SessionTime[]; 
          (* Print[ToString[it]<> " "<>ToString[sqDiff,CForm]<>" "<>ToString[Map[ToString[#,CForm] &, {params}]]] *)];
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
                  (* AccuracyGoal -> Infinity, PrecisionGoal->3,*)   Method->subMethod, OptionValue[NMinimizeOptions]], {NMinimize::cvmit}]
    ];

    If[Head[fit] === FindMinimum,
      Error["FindMinimum failed"]];

    If[Head[fit] === NMinimize,
      Error["NMinimize failed"]];

    fit2 = fit /. MapThread[(#1 -> #2) &, {pList2, pList}];

    Print["fit = ", fit2];
    Return[fit2];
  ];

RunCost[length_, speed_, nProcs_] :=
 {"CPUHours" -> nProcs length/speed // N,
  "WallTimeDays" -> length/speed/24 // N};

PercentageDifference[ap_,bp_] :=
  Module[{a,b},
    {a,b} = Sort[{ap,bp}];
    100 Abs[a-b]/a];

ReadHamiltonianConstraintNorm[run_] :=
  ReadColumnFile[run, "ctgconstraints"~~("-"|"::")~~"hamiltonian_constraint.norm2.asc", {2,3}];

Options[FitEcc] = {"ReturnValue" -> FittedFunction};

FitEcc[sep_DataTable, int : {t1_, t2_}, opts : OptionsPattern[]] :=
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
   _, Error["Unknown option given to FitEcc"]]
  ];

Options[FitEccOm] = {"ReturnValue" -> FittedFunction};

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
   "Association", Association["Eccentricity" -> e/.eccFit, "FittedFunction" -> Function[tp,Evaluate[eccSepFitted/.t->tp]], "Parameters" -> Thread[{"a","b","e","n","phi"} -> eccFit[[All,2]]]],
   _, Error["Unknown option given to FitEcc"]]
  ];

BinaryEccentricityFromSeparation[sep_DataTable] :=
  Module[{model, a, f, t, e, n, l0, params, tStart, tLength, tInterval,
    plot, guessData, fit, fittedData},
    model = a (1 + f t) (1 + e Cos[n t + l0]); 
    params = {{a, 15}, {e, 0.2}, {n, 
      2 Pi/420}, {l0, -1.8}, {f, -0.00001}};
    tStart = 400; 
    tLength = 1000; tInterval = tStart ;; tStart + tLength;
    guessData = 
    model /. Apply[Rule, params, 1] /. t -> Coordinate[sep];
    fit = FindFit[ToList[Slab[sep, tInterval]], model, params, t];
    fittedData = model /. fit /. t -> Coordinate[sep];
    plot = PresentationListLinePlot[{sep, fittedData}, 
      PlotLegend -> {"NR", "Fit"}, LegendPosition -> {Left, Bottom}, 
      GridLines -> {List @@ tInterval, None}];
    Association[
      "FitParameters" -> (fit /. {e -> "e", a -> "a", n -> "n", 
        l0 -> "l0", f -> "f"}), "Plot" -> plot, 
      "Eccentricity" -> Abs[e /. fit]]];


ToFixedWidth[n_Integer, width_Integer] :=
  StringJoin[PadLeft[Characters[ToString[n]], width, "0"]];

DefineMemoFunction[InitialDimensionlessSpin[run_],
 Module[{S0, S1, mp, mm},
  S0 = First@DepVar@ReadIHSpin[run, 0];
  S1 = First@DepVar@ReadIHSpin[run, 1];
  {mp, mm} = ReadPunctureADMMassParameters[run];
  If[Norm@S0 > Norm@S1,
   S0/mp^2,
   S1/mm^2]]];

Clear[WaveformFrequencyDifference];
StandardDefinition[WaveformFrequencyDifference]=True;
WaveformFrequencyDifference[{om1_DataTable, om2_DataTable}, 
  deltat_?NumberQ, {t1_, t2_}] :=
 Module[{om1b, om2b, diff, omsSlabbed, eps, tJunk2},
  (* If[MaxCoordinate[om2] + deltat < t2 ||  *)
   (*   MinCoordinate[om2] + deltat > t1, Return[10000]]; *)
   eps = 1;
   tJunk2 = MinCoordinate[om2] + 200;
  If[MinCoordinate[om2] + deltat > t2-eps || 
    MaxCoordinate[om2] + deltat < t1+eps || tJunk2 + deltat > t2-eps, Return[10000]];
   om1b = Slab[om1, t1 ;; t2];
   (* If[deltat === 3050, *)
   (*   Print["deltat = ", deltat]; *)
   (*   Print[{om2, t1, t2}]; *)
   (*   Print[{100+deltat, MaxCoordinate[om2]+deltat}]; *)
   (*   ]; *)
   om2b = Slab[Shifted[Slab[om2,tJunk2;;All], deltat], t1 ;; t2];
   (* If[$debug == True, *)
   (* Print[ListLinePlot[Slab[om2,tJunk2;;All]]]]; *)

   (*   Print["slabbed"];*)
  (*Print[deltat];*)(*
  Print[{Interval[{t1,t2}],MinCoordinate[om2]+
  deltat}];
  If[!IntervalMemberQ[Interval[{t1,t2}],MinCoordinate[om2]+
  deltat]||!IntervalMemberQ[Interval[{t1,t2}],MaxCoordinate[om2]+
  deltat],
  Print["Out of range"];Return[10000]];
                       *)

   omsSlabbed = ResampleDataTables[{om1b, om2b}];
   (* If[$debug == True, *)
   (* Print[ListLinePlot[omsSlabbed]]]; *)
   
   diff = GridNorm[Subtract@@omsSlabbed] / Sqrt[GridNorm[omsSlabbed[[1]]] GridNorm[omsSlabbed[[2]]]];
   (* Print["diff = ", diff]; *)
   diff];

(* Options[WaveformFrequencyAlignmentTime] = {"JunkTime" -> Automatic}; *)
WaveformFrequencyAlignmentTime[{om1_DataTable, om2_DataTable}, {t1_, 
   t2_} , deltatGuessX_(*, opts:OptionsPattern[]*)] :=
  Module[{min,deltat, omsChopped, newDeltatGuess, diffs},

    (* omsChopped = Slab[#,(MinCoordinate[#]+200);;All] & /@ {om1, om2}; *)
    newDeltatGuess = (t1 - Mean[CoordinateRange[om2]]);
    (* Print["newDeltatGuess = ", newDeltatGuess]; *)

    diffs = DeleteCases[Table[{deltat, WaveformFrequencyDifference[{om1, om2}, 
      deltat, {t1, t2}]}, {deltat, -10000, 10000, 10}], {_,10000}];
(* Print["ordering = ", Ordering[Abs[diffs[[All,2]]], 1][[1]]]; *)
    deltatGuess = diffs[[Ordering[Abs[diffs[[All,2]]], 1],1]][[1]];
    (* Print["diffs = ", diffs]; *)
    (* Print[ListLinePlot[diffs]]; *)
    (* Print["deltatGuess = ",deltatGuess]; *)
    min = 
    FindMinimum[
     WaveformFrequencyDifference[{om1, om2}, 
      deltat, {t1, t2}], {deltat, deltatGuess}];
    (* Print["Frequency alignment time: ",min]; *)
  If[ListQ[min],
   min[[2, 1, 2]],
   Error["Failed to find minimum"]]];

WaveformPhaseAlignment[{phi1_DataTable, phi2_DataTable}, {t1_, t2_}] :=
  Module[{min, deltaPhi, f, dpGuess, tGuess, samples},

    tCommon = {Max[{t1,MinCoordinate[phi1],MinCoordinate[phi2]}],
      Min[{t2,MaxCoordinate[phi1],MaxCoordinate[phi2]}]};
    
    (* dpGuess = Interpolation[phi1,tGuess] - Interpolation[phi2,tGuess]; *)

    dpGuess = Mean[Slab[phi1,Span@@tCommon]] - Mean[Slab[phi2,Span@@tCommon]];
    
    (* Print["dpGuess = ", dpGuess]; *)
    samples = {};
    f[dp_?NumberQ] :=
      Module[{d},
        (* Print["dp=",dp]; *)
        d = WaveformFrequencyDifference[{phi1, phi2 + dp}, 0, {t1, t2}];
        AppendTo[samples,{dp,d}];
        d];
    
    min = FindMinimum[f[deltaPhi], {deltaPhi, dpGuess, dpGuess+1}];

      (* Print[ListLinePlot[samples,PlotRange->{All,{0,All}}, PlotMarkers->Automatic]]; *)
      (* Print["WaveformPhaseAlignment: min=",min]; *)
  If[ListQ[min],
   min[[2, 1, 2]],
   Error["Failed to find minimum"]]];

TimeAndPhaseShiftOfPhase[phis:{phi1_DataTable, phi2_DataTable}, tWindow:{t1_, t2_}] :=
  Module[{deltat,deltaPhi},
    deltat = WaveformFrequencyAlignmentTime[NDerivative[1]/@phis, tWindow, 0];
    Block[{$debug = True},
      deltaPhi = WaveformPhaseAlignment[{phi1,Shifted[phi2,deltat]},tWindow]];
    {deltat, deltaPhi}];

AlignTimeAndPhaseOfPhases[phis:{phi1_DataTable, phi2_DataTable}, tWindow:{t1_, t2_}] :=
  Module[{deltat,deltaPhi},
    {deltat,deltaPhi} = TimeAndPhaseShiftOfPhase[phis,tWindow];
    {phi1,Shifted[phi2,deltat] + deltaPhi}];

AlignTimeAndPhaseOfWaveforms[hs:{h1_DataTable, h2_DataTable}, tWindow:{t1_, t2_}] :=
  Module[{deltat,deltaPhi,phis},
    phis = Phase/@hs;
    {deltat,deltaPhi} = TimeAndPhaseShiftOfPhase[phis,tWindow];
    {hs[[1]],Shifted[hs[[2]],deltat] Exp[I deltaPhi]}];

End[];

EndPackage[];
