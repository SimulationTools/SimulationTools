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
  "SimulationTools`TwoPunctures`",
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
  ReadColumnFile[run, "ctgconstraints::hamiltonian_constraint.norm2.asc", {2,3}];

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

End[];

EndPackage[];
