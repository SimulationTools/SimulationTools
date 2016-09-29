(* Copyright 2010-2016 Ian Hinder and Barry Wardell

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

BeginPackage["SimulationTools`EccentricityReduction`",
 {
  "SimulationTools`ColumnFile`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`Horizons`",
  "SimulationTools`IniFile`",
  "SimulationTools`Memo`",
  "SimulationTools`Parameters`",
  "SimulationTools`Plotting`",
  "SimulationTools`RunFiles`",
  "SimulationTools`TwoPunctures`",
  "SimulationTools`ArgumentChecker`",
  If[$VersionNumber >= 10, "GeneralUtilities`", Unevaluated[Sequence[]]]
 }];

QuasiCircularParametersFromPostNewtonian;
EccentricityReductionParameters;
ReduceEccentricity;
BinaryEccentricityFromSeparationDerivative::usage = "BinaryEccentricityFromSeparationDerivative[sep, {t1, t2}] returns an association containing information about the eccentricity of a binary with separation sep.";
EccentricityParameterSpacePlot;

Begin["`Private`"];

(* EccentricityReductionParameters[sim_String] := *)
(*   Module[{mdFiles}, *)
(*     mdFiles = FindSimulationFiles[sim, "TwoPunctures.bbh"]; *)
(*     Association@Thread[{"Separation", "RadialMomentum", "TangentialMomentum"} -> *)
(*       (ToExpression@IniVariable[mdFiles[[1]], #] & /@ *)
(*       {"initial-separation", "initial-bh-momentum1x", "initial-bh-momentum1y"})]]; *)

EccentricityReductionParameters[sim_String] :=
  Module[{b, pr, pPhi},
    {b, pr, pPhi} = ImportString[ReadSimulationParameter[sim, #], "Table"][[1,1]] & /@ {"TwoPunctures::par_b", "TwoPunctures::par_P_plus[0]", "TwoPunctures::par_P_plus[1]"};
    Association["Separation" -> 2b, "RadialMomentum" -> pr, "TangentialMomentum" -> pPhi,
      "OrbitalAngularMomentum" -> 2b pPhi]];

QuasiCircularParametersFromPostNewtonian[{m_, q_, chi1_, chi2_, om_}] :=
  Module[{eta, m1, m2, S1, S2, 
   S\[ScriptL], \[CapitalSigma]\[ScriptL], \[Delta]M, mu, en, 
   l, \[CapitalDelta], x, nu, enNum, lNum, xRule, rNum, rExpr, rDotExpr, 
   rDotNum, r, rp0, \[Gamma], rDot, prExpr, prNum},
  (* TODO: the m-dependence of the expressions below is only correct for m=
  1 *)
  (* TODO: replace these symbols (\[CapitalSigma] etc) with names like "Sigma" *)
  (* TODO: make sure expressions are directly taken from named papers, mostly the Blanchet living review *)

  eta = q/(1. + q)^2.;
  m1 = (1 + Sqrt[1 - 4 eta])/2;
  m2 = m - m1;
  S1 = m1^2 chi1;
  S2 = m2^2 chi2;
  
  S\[ScriptL] = S1 + S2;
  \[CapitalSigma]\[ScriptL] = S2/m2 - S1/m1;
  \[Delta]M = m1 - m2;
  mu = eta;
  
  (* Energy *)
  en = -((mu x)/
       2) (1 + x (-(3/4) - 1/12 eta) + 
        x^2 (-(27/8) + 19/8 eta - 1/24 eta^2) + 
        x^3 (-(675/64) + (34445/576 - 205/96 \[Pi]^2) eta - 
           155/96 eta^2 - 35/5184 eta^3) + 
        x^(3/2) (14/3 S\[ScriptL] + 2 \[Delta]M \[CapitalSigma]\[ScriptL]) + 
        x^(5/2) ((11 - 61/9 eta) S\[ScriptL] + \[Delta]M (3 - 
              10/3 eta) \[CapitalSigma]\[ScriptL]) + 
        x^(7/2) ((135/4 - 367/4 eta + 
              29/12 eta^2) S\[ScriptL] + \[Delta]M (27/4 - 39 eta + 
              5/4 eta^2) \[CapitalSigma]\[ScriptL]) - 
        1/2 (1 + (m1 - m2) - 2 eta) chi1^2 x^2 - 
        1/2 (1 + (m2 - m1) - 2 eta) chi2^2 x^2 - 
        2 eta chi1 chi2 x^2 + (-((65 chi1^2)/36) - (
           65 \[CapitalDelta] chi1^2)/36 + (275 nu chi1^2)/36 + 
           145/36 \[CapitalDelta] nu chi1^2 - (35 nu^2 chi1^2)/
           18 - (11 nu chi1 chi2)/3 - 
           14/9 nu^2 chi1 chi2 - (65 chi2^2)/36 + (
           65 \[CapitalDelta] chi2^2)/36 + (275 nu chi2^2)/36 - 
           145/36 \[CapitalDelta] nu chi2^2 - (35 nu^2 chi2^2)/
           18) x^3) /. nu -> eta /. \[CapitalDelta] -> Sqrt[
     1 - 4 eta];

  (* Angular momentum (not divided by mu) *)
  l = eta/x^(
      1/2) (1 + x (3/2 + 1/6 eta) + 
        x^2 (27/8 - 19/8 eta + 1/24 eta^2) + 
        x^3 (135/16 + (-6889/144 + 41/24 \[Pi]^2) eta + 31/24 eta^2 + 
           7/1296 eta^3) + 
        x^(3/2) (-(35/6) S\[ScriptL] - 
           5/2 \[Delta]M \[CapitalSigma]\[ScriptL]) + 
        x^(5/2) ((-(77/8) + 427/72 eta) S\[ScriptL] + \[Delta]M (-(21/8) + 
              35/12 eta) \[CapitalSigma]\[ScriptL]) + 
        x^(7/2) ((-(405/16) + 1101/16 eta - 
              29/16 eta^2) S\[ScriptL] + \[Delta]M (-(81/16) + 
              117/4 eta - 15/16 eta^2) \[CapitalSigma]\[ScriptL]) + (1/
           2 + (m1 - m2)/2 - eta) chi1^2 x^2 + (1/2 + (m2 - m1)/
           2 - eta) chi2^2 x^2 + 
        2 eta chi1 chi2 x^2 + ((13 chi1^2)/9 + (
           13 \[CapitalDelta] chi1^2)/9 - (55 nu chi1^2)/9 - 
           29/9 \[CapitalDelta] nu chi1^2 + (14 nu^2 chi1^2)/9 + (
           7 nu chi1 chi2)/3 + 17/18 nu^2 chi1 chi2 + (
           13 chi2^2)/9 - (13 \[CapitalDelta] chi2^2)/9 - (
           55 nu chi2^2)/9 + 29/9 \[CapitalDelta] nu chi2^2 + (
           14 nu^2 chi2^2)/
           9) x^3) /. nu -> eta /. \[CapitalDelta] -> Sqrt[1 - 4 eta];
  
  (* Based on Blanchet Eq.193 *)
  
  rExpr = m/x + 1/3 m (-3 + nu) + 1/36 m nu (171 + 4 nu) x + 
     m (-((37 nu^2)/12) + (2 nu^3)/
        81 + nu (-(24257/2520) + (41 Pi^2)/192) + 
        22/3 nu Log[r/rp0]) x^2 /. {nu -> eta, rp0 -> r};
  (* TODO: Not sure what to do with r,rp0 here *)

  (* TODO: add spin effects to this.  We can get these from Blanchet,
     but need to do some manipulations. *)

  (* Blanchet 227a *)
  rDotExpr = -64/5 m^3 nu/
      r^3 (1 + \[Gamma] (-1751/336 - 7/4 nu)) /. nu -> eta;
  
  (* Derived myself (ICH) in notebook "ETBBH 11 - Eccentricity
     reduction 2" from the Lagrangian in Blanchet.  This should be in
     ADMTT coordinates, and I'm not sure to what PN order it is.
     Go back and checked. *)

  prExpr = mu rDot + 1/(2 m r) mu rDot (6 m^2 + 4 m mu + m om^2 r^3 - 
       3 mu om^2 r^3 + (m - 3 mu) r rDot^2);
  
  xRule = {x -> (om^(2/3))};
  enNum = en /. xRule;
  lNum = l /. xRule;
  rNum = rExpr /. xRule;
  rDotNum = rDotExpr /. {r -> rNum, \[Gamma] -> 1/rNum};
  prNum = prExpr /. {rDot -> rDotNum, r -> rNum};
  
  Association["Energy" -> enNum, "OrbitalAngularMomentum" -> lNum,
   "Separation" -> rNum, "TotalAngularMomentum" -> lNum + S1 + S2,
   "RadialVelocity" -> rDotNum,
   "RadialMomentum" -> prNum]];


(**********************************************************)
(* BinaryEccentricityFromSeparationDerivative             *)
(**********************************************************)

Options[BinaryEccentricityFromSeparationDerivative] =
  {"AdditionalOscillations" -> False, "FixEccentricFrequency" -> False,
    "UpdateTime" -> 0,
    "QuadraticInspiral" -> False,
    "Inspiral" -> "Polynomial",
    "CorrectedSemiMajorAxis" -> False,
    "MeanMotionGuess" -> 0.021112225715565695`,
    "SemiMajorAxisGuess" -> 10,
    "EccentricityGuess" -> -0.01};

BinaryEccentricityFromSeparationDerivative[sep_DataTable, 
  window : {t1_, t2_}, opts : OptionsPattern[]] := 
 Module[{model, a, a1, f, t, ae, n, l0, c, params, tStart, tLength, 
   tInterval, plot, guessData, fit, fittedData, sepDot, flip, e, 
   deltaD0, deltapr, mu, sepModel, sepFit, sepParams, aFit, n2, psi, 
   n3, residual, ecc, residualNorm, fittedP, completePeriod, circData},

  (* Notation:

     a    Semi-major axis
     P    Radial period (of "eccentric oscillations")
     n    Mean motion n = 2 Pi/P
     r    Separation of the bodies (distance to origin in reduced one-body problem)
     e    Radial eccentricity
     t    Time
     l0   Initial mean anomaly ("phase of eccentric oscillations", or "orientation of the ellipse")
     a1,f Parameters modelling the inspiral
     pr   Radial momentum
     D    Initial separation, r(0)
     L    Orbital angular momentum

     *)

  (* Method:

     - Measure eccentricity in rDot by comparing with a
       Newtonian-inspired model with free parameters
     - Update the separation and pr such that if the orbit was Newtonian,
       the eccentricity would be eliminated, to leading order, keeping L fixed *)

  (* Define the model for rDot.  For a Newtonian orbit, we would have

       r = a(1+e cos(n t + l0))   [TODO: the sign of e may need to be flipped to match
                                   convention]

     to leading order in e.  So

       rDotN = - a e n sin(n t + l0).

     When radiation reaction is added, a, e and n evolve in time,
     leading to a gradual decrease in r.  Model this by adding a
     quadratic inspiral in r, i.e. a linear inspiral in rDot.

       rDot = a1 + f t - a e n sin(n t + l0).

     *)


   model = - ae n Sin[n t + l0];

  (* Choose initial guesses for the fit parameters *)
   params = {{l0, -4.5 + Pi}, {ae, OptionValue[SemiMajorAxisGuess] OptionValue[EccentricityGuess]},{n,OptionValue[MeanMotionGuess]}};

   Replace[OptionValue[Inspiral],
     {"Polynomial" :>
       (model = model + a1 + f t;
         params = Join[params, {{f, -0.00}, {a1, 0}}];
       If[OptionValue[QuadraticInspiral],
         model = model + f2 t^2;
         params = Join[params, {f2, 0}]]),
       "PN" :>
       (If[!OptionValue[CorrectedSemiMajorAxis],
         Error["Inspiral -> PN requires CorrectedSemiMajorAxis -> True"]];
         model = model + a1 (tc-t)^(-3/4) + f(tc-t)^(-9/8);
         params = Join[params, {{f,0}, {a1,0},{tc,600*2}}]),
       _ :> Error["Unrecognised value for option Inspiral in BinaryEccentricityFromSeparationDerivative"]}];
   
 (* Note: a e appears as a single parameter in the model, so we
    cannot determine them independently from rDot. *)


  (* TODO: allow these initial fit guesses to be passed in as options to the function.*)

  (* TODO: determine the initial guesses intelligently from properties
     of the simulation, e.g. the expected initial frequency and
     separation. *)

  (* It might be useful to add high frequency oscillations to the fit
     function to model orbital-period gauge oscillations in rDot
     caused by shift dynamics, to help the fit find the eccentric
     oscillations better. *)

  If[OptionValue[AdditionalOscillations],
   model = model + n2 Sin[2 n3 t + psi];
   params = Join[params, {{n2, 10^-4}, {psi, 0}, {n3, 2 Pi/200}}]];

   (* If the eccentricity is very small, other oscillations with very
      wrong periods might be picked up accidentally.  In that case,
      allow n = 2Pi/P to be passed in as an option, and use this value
      rather than fitting for it.  If L is kept fixed during
      eccentricity reduction, then to leading order in e, n does not
      change with e, so we can expect n to be the same from one
      iteration to the next. *)
  If[OptionValue[FixEccentricFrequency] =!= False,
   model = model /. n -> OptionValue[FixEccentricFrequency];
   params = DeleteCases[params, {n, _}]];

  tStart = t1;
  tLength = t2 - t1; tInterval = tStart ;; tStart + tLength;
  sepDot = NDerivative[1][sep];
  guessData = 
   model /. Apply[Rule, params, 1] /. t -> Coordinate[sepDot];

  (* Fit the numerical rDot to the model *)
  fit = FindFit[ToList[Slab[sepDot, tInterval]], model, params, t];

  (* TODO: detect messages about inability to find a good fit, and
     return an error *)

  fittedData = model /. fit /. t -> Coordinate[sepDot];
  residual = (model /. fit /. 
      t -> Coordinate[Slab[sepDot, tInterval]]) - 
  Slab[sepDot, tInterval];
  residualNorm = GridNorm[residual]/-Subtract@@CoordinateRange[residual];

  circData = (model /. ae->0) /. fit /. t -> Coordinate[sepDot];
   
  (* For convenience, make the eccentricity positive and adjust the
     mean anomaly to compensate *)
  flip = (ae /. fit) < 0;
  If[flip,
   fit = fit /. {(l0 -> l0x_) :> (l0 -> l0x + N@Pi), (ae -> 
         aex_) :> (ae -> -aex)}];
  
  (* The rDot model only determines a e, but we want to know e itself.
     So now we fit to r to determine a, and use that to measure e. *)
  sepModel = Chop[Integrate[model /. fit, t] + const];
  sepParams = {{const, 0}};
  sepFit = 
   FindFit[ToList[Slab[sep, tInterval]], sepModel, sepParams, t];

  (* Determine the eccentricity from ae and the separation fit for a *)

   If[OptionValue[CorrectedSemiMajorAxis],
     sepCircModel = Chop[Integrate[model /. Join[{ae->0},fit], t] + const];
     aFit = sepCircModel /. Join[sepFit, {t->0}],
     (* else *)
     aFit = const /. sepFit];

  ecc = ae/aFit /. fit;

  (* Due to dynamics in the slicing, "t=0" in the model might not
     correspond to "t=0" in the simu lation.  Allow the time at which
     to perform the update to be passed in.  This might come from a
     very eccentric initial simulation. *)
  tUpdate = OptionValue[UpdateTime];

  (* Calculate how D = r(0) should be modified to eliminate the
     existing eccentricity *)
  deltaD0 = -D[Integrate[model /. ae -> a e, t] + c /. t -> tUpdate, e] ae/
      a //. fit;

  (* TODO: a might be redundant in the above; maybe we can reduce ae
     instead *)

  (* Calculate how pr, the radial momentum, should be modified to
     eliminate the eccentricity.  Approximate pr = mu rDot, i.e. use
     the Newtonian relation.  *)

  deltapr = -"mu" D[model /. ae -> a e /. t -> tUpdate, e] ae/a //. fit;

  (* This is probably a large source of error in the method currently,
     and should be replaced with the PN expression. The below
     expressions should be checked and the dependence on om
     eliminated. *)

    (*   prPN = mu rDot +  *)
    (*   1/(2 m r) mu rDot (6 m^2 + 4 m mu + m om^2 r^3 -  *)
    (*      3 mu om^2 r^3 + (m - 3 mu) r rDot^2) *)

  (* Generate a diagnostic plot of the fit *)
  plot = PresentationListLinePlot[{sepDot, fittedData, (*guessData, *)circData}, 
    PlotLegend -> {"NR", 
      SequenceForm["Fit (e=", ScientificForm[ecc, 2], ")"], 
      (*"Initial fit guess",*) "Circular fit"}, LegendPosition -> {Left, Bottom}, 
    GridLines -> {List @@ tInterval, None}, PlotRange -> All(*{{0,1.2 t2}, #[Slab[sepDot,t1;;t2]] & /@ {Min,Max}}*)];

  (* Fitted period *)
  fittedP = 2Pi/(n/.fit);
  completePeriod =
    If[t2-t1 < fittedP,
      Print["Warning: fitted radial period ", fittedP, " is shorter than fit window of length ", t2-t1]; False,
      (* else *)
      True];
  
  Association[
    "FitParameters" -> (fit /. {ae -> "ae", a -> "a", n -> "n", 
      l0 -> "l0", f -> "f", a1 -> "a1"}), "Plot" -> plot, "Eccentricity" -> ecc, 
    "DeltaD0" -> deltaD0, "DeltaPr" -> deltapr, 
    "Residual" -> residual, "ResidualNorm" -> residualNorm,
    "PericentreTime" -> (-l0/n) /.fit, "CompletePeriod" -> completePeriod]];

Options[ReduceEccentricity] = {};
ReduceEccentricity[sim_String, newEcc_Association, opts:OptionsPattern[]] :=
 Module[{D0, pr0, deltax, x0, mu},
  D0 = 2 ToExpression@
     ReadSimulationParameter[sim, "TwoPunctures::par_b"];
  pr0 = ToExpression@
    ReadSimulationParameter[sim, "TwoPunctures::par_P_plus[0]"];
  x0 = {D0, pr0};
  mu = SymmetricMassRatio[sim] TotalMass[sim];
  deltax = Values[newEcc[[{"DeltaD0", "DeltaPr"}]]] /. "mu" -> mu;
  x0 + deltax];

EccentricityParameterSpacePlot[params_List, es_List] :=
 Module[{Ds, prs, eData, eModel, fit, fitFn, e0, d2edpr2, pr, pr0, 
   d2edd2, d, d0, d2edprdd},
  Ds = params[[All, 1]];
  prs = params[[All, 2]];
  eData = Thread[{Ds, prs, es^2}];
  eModel = 
   e0 + d2edpr2 (pr - pr0)^2 + d2edd2 (d - d0)^2 + 
    d2edprdd (pr - pr0) (d - d0);
  fit = FindFit[eData[[1 ;; All]], 
    eModel, {{pr0, prs[[-1]]}, {d0, Ds[[-1]]}, {d2edpr2, 0}, {d2edd2, 
      0}, {d2edprdd, 0}, {e0, 0}}, {d, pr}];
  fitFn = eModel /. fit;
  Show[ContourPlot[
    Log10[Sqrt[Abs@fitFn]], {d, Min[Ds] - 0.1, Max[Ds] + 0.1}, {pr, 
     Min[prs] - 0.0001, Max[prs] + 0.0001}, 
    Contours -> Range[-4, 1, 0.1], PlotLegends -> Automatic], 
   ListPlot[params, PlotStyle -> Black]]];

End[];

EndPackage[];
