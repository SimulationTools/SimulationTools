(* ::Package:: *)

(* Copyright 2010-2016 Ian Hinder, Barry Wardell and Eliu Huerta

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
  "SimulationTools`Binary`",
  "SimulationTools`BHCoordinates`",
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
PostNewtonianEvolution;
EccentricityReductionParameters;
ReduceEccentricity;
BinaryEccentricityFromSeparationDerivative::usage = "BinaryEccentricityFromSeparationDerivative[sep, {t1, t2}] returns an association containing information about the eccentricity of a binary with separation sep.";
EccentricityParameterSpacePlot;
SimulationEccentricityAnalysis;

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
   rDotNum, r, rp0, raxder, \[Gamma], \[Gamma]ammaE, rDot, drdx, dEdt, dEdx, a1, a2, a3, a4, a5, e4, e5, rax, j4, j5, V, prExpr1, prExpr2, prExpr3, prExpr, prNum, \[Nu], omDotExpr},
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
  \[Nu] = eta;
  nu = eta;
  e4 = -(123671/5760)+(9037 \[Pi]^2)/1536+(896 \[Gamma]ammaE)/15+(-(498449/3456)+(3157 \[Pi]^2)/576) \[Nu]+(301 \[Nu]^2)/1728+(77 \[Nu]^3)/31104+(1792 Log[2])/15;
  e5 = -55.13;
  j4 = -(5/7)e4+64/35;
  j5 = -(2/3)e5-4988/945-656/135 eta;
  a1 = -2.18522;
  a2 = 1.05185;
  a3 = -2.43395;
  a4 = 0.400665;
  a5 = -5.9991;
  \[Gamma]ammaE = 0.5772156649;
  
  (* Energy EAH: this new version now includes corrections up to 5PN order in the QC sector*)
  en = -((mu x)/
       2) (1 + x (-(3/4) - 1/12 eta) + 
        x^2 (-(27/8) + 19/8 eta - 1/24 eta^2) + 
        x^3 (-(675/64) + (34445/576 - 205/96 \[Pi]^2) eta - 
           155/96 eta^2 - 35/5184 eta^3) + x^4*((-3969/128) + eta*e4 + (448*eta*Log[x]/15)) +
            x^5*((-45927/512) + eta*e5 + ((-4988/35) - (656*eta/5))*eta*Log[x]) + 
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

  (* Angular momentum (not divided by mu) EAH: this version include corrections up to 5PN order *)
  l = eta/x^(
      1/2) (1 + x (3/2 + 1/6 eta) + 
        x^2 (27/8 - 19/8 eta + 1/24 eta^2) + 
        x^3 (135/16 + (-6889/144 + 41/24 \[Pi]^2) eta + 31/24 eta^2 + 
           7/1296 eta^3) + 
           x^4*((2835/128) + eta*j4 - (64*eta*Log[x]/3))+ 
           x^5*((15309/256) + eta*j5 + ((9976/105) + (1312*eta/15))*eta*Log[x])+
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
  
  (* Based on Blanchet Eq.193. EAH: new version below is from Blanchet but now includes spin corrections *)
  (*Note that below rax=m/r *)
  rax =(x+x^2-(eta x^2)/3+5/3 S\[ScriptL] x^(5/2)+x^3-(65 eta x^3)/12+10/3 S\[ScriptL] x^(7/2)+8/9 eta S\[ScriptL] x^(7/2)+x^4-(2203 eta x^4)/2520+(229 eta^2 x^4)/36+
  (eta^3 x^4)/81-41/192 eta \[Pi]^2 x^4+(175 S\[ScriptL]^2 x^4)/18+5 S\[ScriptL] x^(9/2)-127/12 eta S\[ScriptL] x^(9/2)-6 eta^2 S\[ScriptL] x^(9/2)+Sqrt[1-4 eta] x^(5/2) \[CapitalSigma]\[ScriptL]+
  2 Sqrt[1-4 eta] x^(7/2) \[CapitalSigma]\[ScriptL]+35/3 Sqrt[1-4 eta] S\[ScriptL] x^4 \[CapitalSigma]\[ScriptL]+3 Sqrt[1-4 eta] x^(9/2) \[CapitalSigma]\[ScriptL]-61/6 Sqrt[1-4 eta] eta x^(9/2) \[CapitalSigma]\[ScriptL]-
  8/3 Sqrt[1-4 eta] eta^2 x^(9/2) \[CapitalSigma]\[ScriptL]+(7 x^4 \[CapitalSigma]\[ScriptL]^2)/2-14 eta x^4 \[CapitalSigma]\[ScriptL]^2-22/3 eta x^4 Log[r/rp0]);
  rExpr = m/rax;
  
  (*rExpr = m/x + 1/3 m (-3 + nu) + 1/36 m nu (171 + 4 nu) x + 
     m (-((37 nu^2)/12) + (2 nu^3)/
        81 + nu (-(24257/2520) + (41 Pi^2)/192) + 
        22/3 nu Log[r/rp0]) x^2 /. {nu -> eta, rp0 -> r};*)
  (* TODO: Not sure what to do with r,rp0 here *)

  (* TODO: add spin effects to this.  We can get these from Blanchet,
     but need to do some manipulations. EAH: I have done this. See above*)

  (* Blanchet 227a *)
  (*EAH: To compute rDotExpr including spin corrections, I use rExpr, derived above, and then E(x,S) and the energy flux including HO spin corrections*)
  
  raxder = (1+2 x-(2 eta x)/3+25/6 S\[ScriptL] x^(3/2)+3 x^2-(65 eta x^2)/4+35/3 S\[ScriptL] x^(5/2)+28/9 eta S\[ScriptL] x^(5/2)+4 x^3-(2203 eta x^3)/630+(229 eta^2 x^3)/9+
  (4 eta^3 x^3)/81-41/48 eta \[Pi]^2 x^3+(350 S\[ScriptL]^2 x^3)/9+45/2 S\[ScriptL] x^(7/2)-381/8 eta S\[ScriptL] x^(7/2)-27 eta^2 S\[ScriptL] x^(7/2)+5/2 Sqrt[1-4 eta] x^(3/2) \[CapitalSigma]\[ScriptL]+7 Sqrt[1-4 eta] x^(5/2) \[CapitalSigma]\[ScriptL]+
  140/3 Sqrt[1-4 eta] S\[ScriptL] x^3 \[CapitalSigma]\[ScriptL]+27/2 Sqrt[1-4 eta] x^(7/2) \[CapitalSigma]\[ScriptL]-183/4 Sqrt[1-4 eta] eta x^(7/2) \[CapitalSigma]\[ScriptL]-12 Sqrt[1-4 eta] eta^2 x^(7/2) \[CapitalSigma]\[ScriptL]+14 x^3 \[CapitalSigma]\[ScriptL]^2-56 eta x^3 \[CapitalSigma]\[ScriptL]^2-88/3 eta x^3 Log[r/rp0]);
  
  drdx = - raxder/(rax*rax);
  
  dEdx = (-(1/2) x (-(3/4)-eta/12-4 chi1 chi2 eta x+2 (-(27/8)+(19 eta)/8-eta^2/24) x-chi1^2 (1-2 eta+m1-m2) x-chi2^2 (1-2 eta-m1+m2) x+
  3 (-((65 chi1^2)/36)-(65 chi2^2)/36-65/36 chi1^2 Sqrt[1-4 eta]+65/36 chi2^2 Sqrt[1-4 eta]+(275 chi1^2 eta)/36-(11 chi1 chi2 eta)/3+(275 chi2^2 eta)/36+
  145/36 chi1^2 Sqrt[1-4 eta] eta-145/36 chi2^2 Sqrt[1-4 eta] eta-(35 chi1^2 eta^2)/18-14/9 chi1 chi2 eta^2-(35 chi2^2 eta^2)/18) x^2+
  3 (-(675/64)-(155 eta^2)/96-(35 eta^3)/5184+eta (34445/576-(205 \[Pi]^2)/96)) x^2+(448 eta x^3)/15+(-(4988/35)-(656 eta)/5) eta x^4+3/2 Sqrt[x] ((14 S\[ScriptL])/3+2 \[Delta]M \[CapitalSigma]\[ScriptL])+
  5/2 x^(3/2) ((11-(61 eta)/9) S\[ScriptL]+(3-(10 eta)/3) \[Delta]M \[CapitalSigma]\[ScriptL])+7/2 x^(5/2) ((135/4-(367 eta)/4+(29 eta^2)/12) S\[ScriptL]+(27/4-39 eta+(5 eta^2)/4) \[Delta]M \[CapitalSigma]\[ScriptL])+
  4 x^3 (-(3969/128)+e4 eta+448/15 eta Log[x])+5 x^4 (-(45927/512)+e5 eta+(-(4988/35)-(656 eta)/5) eta Log[x]))+1/2 (-1-(-(3/4)-eta/12) x+
  2 chi1 chi2 eta x^2-(-(27/8)+(19 eta)/8-eta^2/24) x^2+1/2 chi1^2 (1-2 eta+m1-m2) x^2+1/2 chi2^2 (1-2 eta-m1+m2) x^2-
  (-((65 chi1^2)/36)-(65 chi2^2)/36-65/36 chi1^2 Sqrt[1-4 eta]+65/36 chi2^2 Sqrt[1-4 eta]+(275 chi1^2 eta)/36-(11 chi1 chi2 eta)/3+(275 chi2^2 eta)/36+
  145/36 chi1^2 Sqrt[1-4 eta] eta-145/36 chi2^2 Sqrt[1-4 eta] eta-(35 chi1^2 eta^2)/18-14/9 chi1 chi2 eta^2-(35 chi2^2 eta^2)/18) x^3-
  (-(675/64)-(155 eta^2)/96-(35 eta^3)/5184+eta (34445/576-(205 \[Pi]^2)/96)) x^3-x^(3/2) ((14 S\[ScriptL])/3+2 \[Delta]M \[CapitalSigma]\[ScriptL])-x^(5/2) ((11-(61 eta)/9) S\[ScriptL]+(3-(10 eta)/3) \[Delta]M \[CapitalSigma]\[ScriptL])-
  x^(7/2) ((135/4-(367 eta)/4+(29 eta^2)/12) S\[ScriptL]+(27/4-39 eta+(5 eta^2)/4) \[Delta]M \[CapitalSigma]\[ScriptL])-x^4 (-(3969/128)+e4 eta+448/15 eta Log[x])-x^5 (-(45927/512)+e5 eta+(-(4988/35)-(656 eta)/5) eta Log[x]))); 
  dEdt = (1+(-(1247/336)-(35 eta)/12) x+4 \[Pi] x^(3/2)+(-(44711/9072)+(9271 eta)/504+(65 eta^2)/18) x^2+(-(8191/672)-(583 eta)/24) \[Pi] x^(5/2)+
  (-(16285/504)+(214745 eta)/1728+(193385 eta^2)/3024) \[Pi] x^(7/2)+x^(3/2) (-4 S\[ScriptL]-5/4 Sqrt[1-4 eta] \[CapitalSigma]\[ScriptL]+x ((-(9/2)+(272 eta)/9) S\[ScriptL]+Sqrt[1-4 eta] (-(13/16)+(43 eta)/4) \[CapitalSigma]\[ScriptL])+
  x^2 ((476645/6804+(6172 eta)/189-(2810 eta^2)/27) S\[ScriptL]+Sqrt[1-4 eta] (9535/336+(1849 eta)/126-(1501 eta^2)/36) \[CapitalSigma]\[ScriptL])+
  x^(3/2) (-16 \[Pi] S\[ScriptL]-31/6 Sqrt[1-4 eta] \[Pi] \[CapitalSigma]\[ScriptL])+x^(5/2) ((-((3485 \[Pi])/96)+(13879 eta \[Pi])/72) S\[ScriptL]+Sqrt[1-4 eta] (-((7163 \[Pi])/672)+(130583 eta \[Pi])/2016) \[CapitalSigma]\[ScriptL]))+
  x^3 (6643739519/69854400-(94403 eta^2)/3024-(775 eta^3)/324+(16 \[Pi]^2)/3+eta (-(134543/7776)+(41 \[Pi]^2)/48)-(1712 \[Gamma]ammaE)/105-856/105 Log[16 x]));
  rDotExpr = -(32/5)*nu*x^5*(drdx*dEdt/dEdx);
  
  xDotExpr = -1/(dEdx) (32/5 nu x^5) dEdt;

  (* Derived myself (ICH) in notebook "ETBBH 11 - Eccentricity
     reduction 2" from the Lagrangian in Blanchet.  This should be in
     ADMTT coordinates, and I'm not sure to what PN order it is.
     Go back and checked. EAH: the below expression now goes to order 1/c^6*)

  V = Sqrt[rDot^2 + (om*r)^2];
  prExpr1 = 1 + (1/2)*(1 - 3*eta)*V^2 + (m/r)*(3+2*eta);
  prExpr2 = ((3/8-(21 eta)/8+(39 eta^2)/8)*V^4 + 
  (m/r)*(-(1/2) (-7+10 eta+14 eta^2) V^2-(-1+eta) eta rDot^2)+(m/r)^2 (4+eta+4 eta));
  prExpr3 = ((5/16-(59 eta)/16+(119 eta^2)/8-(323 eta^3)/16)V^6 + 
  (m/r)*(1/8 ((33-160 eta+129 eta^2+270 eta^3) V^4+2 eta (8-39 eta+42 eta^2) V^2 rDot^2+9 (1-2 eta) eta^2 rDot^4)) + 
  (m/r)^2*(1/24 (-3 (-94+81 eta+108 eta^2+180 eta^3) V^2+eta (77-38 eta-132 eta^2) rDot^2)) + (m/r)^3*(13/4-(89 eta)/12+(5 eta^2)/4+8 eta^3+(eta \[Pi]^2)/16));
  prExpr = mu*rDot*(prExpr1 + prExpr2 + prExpr3);
  
  xRule = {x -> (om^(2/3))};
  enNum = en /. xRule;
  lNum = l /. xRule;
  rNum = rExpr /. xRule;
  rDotNum = rDotExpr /. {r -> rNum, \[Gamma] -> 1/rNum};
  prNum = prExpr /. {rDot -> rDotNum, r -> rNum} //. xRule;
  omDotExpr = (D[x^(3/2),x] xDotExpr) /. xRule;
  
  Association["Energy" -> enNum, "OrbitalAngularMomentum" -> lNum,
   "Separation" -> rNum, "TotalAngularMomentum" -> lNum + S1 + S2,
   "RadialVelocity" -> rDotNum,
   "RadialMomentum" -> prNum,
   "OmegaDot" -> omDotExpr]/.{rp0->2,r->10}
];

PostNewtonianEvolution[{M_, q_, chi1_, chi2_, om0_}] :=
 Module[{pn, omEqs, soln, omSoln, phiSoln, tMax, om, phi, t},
  pn = QuasiCircularParametersFromPostNewtonian[{M, q, chi1, chi2, om}];
  omEqs = {om'[t] == (Normal@Series[Rationalize[pn["OmegaDot"]], {om, 0, 7}] /. 
  om -> om[t]), 
    phi'[t] == om[t], om[0] == om0, phi[0] == 0};
  soln = NDSolve[
     Join[omEqs, {WhenEvent[om[t] == 0.1, "StopIntegration"]}], {om, 
      phi}, {t, 0, Infinity}][[1]];
  omSoln = om /. soln;
  phiSoln = phi /. soln;
  tMax = omSoln[[1, 1, 2]];
  
  <|"Omega" -> omSoln, "Phi" -> phiSoln, 
   "NumberOfOrbits" -> (phiSoln[tMax] - phiSoln[0])/(2 Pi), 
   "TimeToMerger" -> tMax|>];

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
    "OrbitalFrequency" -> None,
    "SemiMajorAxisGuess" -> 10,
    "EccentricityGuess" -> -0.01};

BinaryEccentricityFromSeparationDerivative[sep_DataTable, 
  window : {t1_, t2_}, opts : OptionsPattern[]] := 
 Module[{model, a, a1, f, t, ae, n, l0, c, params, tStart, tLength, 
   tInterval, plot, guessData, fit, fittedData, sepDot, flip, e, 
   deltaD0, deltapr, mu, sepModel, sepFit, sepParams, aFit, n2, psi, 
   n3, residual, ecc, residualNorm, fittedP, completePeriod, circData,
   fitOptions = {PrecisionGoal -> 6, AccuracyGoal -> 6, MaxIterations -> 100},
   convFail, meanMotion, norm},

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
         model = model + a1 (tc-t)^(-3/4) + f (tc-t)^(-9/8);
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

   norm[d_DataTable] :=
     d/Mean[d];
   
  (* Fit the numerical rDot to the model *)
   convFail = Quiet[Check[fit = FindFit[ToList[Slab[sepDot, tInterval]], model, params, t,
     fitOptions], FindFit::cvmit, FindFit::cvmit],FindFit::cvmit] === FindFit::cvmit;

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
   FindFit[ToList[Slab[sep, tInterval]], sepModel, sepParams, t,
     fitOptions];

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

   meanMotion = If[OptionValue[FixEccentricFrequency] =!= False,
     OptionValue[FixEccentricFrequency],
     n/.fit];

  (* Fitted period *)
  fittedP = 2Pi/(meanMotion/.fit);
  completePeriod =
    If[t2-t1 < fittedP,
      Print["Warning: fitted radial period ", fittedP, " is longer than fit window of length ", t2-t1]; False,
      (* else *)
      True];
   failed=False;
   radPeriodTooShort = False;
   If[OptionValue[OrbitalFrequency] =!= None && (n/.fit) > OptionValue[OrbitalFrequency],
     Print["WARNING: Fitted mean motion is greater than the orbital frequency; probably fitted gauge oscillations."];
     radPeriodTooShort = True;
     failed = True];
   
  Association[
    "FitParameters" -> (fit /. {ae -> "ae", a -> "a", n -> "n", 
      l0 -> "l0", f -> "f", a1 -> "a1", tc -> "tc"}), "Plot" -> plot, "Eccentricity" -> ecc, 
    "DeltaD0" -> deltaD0, "DeltaPr" -> deltapr, 
    "Residual" -> residual, "ResidualNorm" -> residualNorm,
    "PericentreTime" -> (-l0/meanMotion) /.fit, "CompletePeriod" -> completePeriod,
    "OrbitalFrequency"->OptionValue[OrbitalFrequency],
    "Failed" -> failed, "RadialPeriodTooShort" ->radPeriodTooShort,
    "ConvergenceFailure" ->convFail, "MeanMotion" -> meanMotion, "Separation" -> sep]];

(* Analyse an eccentricity reduction series.  Each result is passed to
   the next analysis so that the mean motion can be used in case it
   cannot be determined automatically. *)
SimulationEccentricityAnalysis[sims_List] :=
  Drop[FoldList[SimulationEccentricityAnalysis[#2, #1] &, None, sims],1];

(* Analyse the eccentricity of a simulation *)
SimulationEccentricityAnalysis[sim_String, prevEcc_: None] :=
 Module[{sep, om0, D0, params, ecc, eccMeasured, nextD, nextPr, ecc2, 
   result, eccFitWindow, eps, calcEcc},
   
  (* Any unhandled message leads to a failure report *)
  result = Check[
    (* This is a simple way to remove high frequency noise which
       sometimes causes the fit to fail to converge *)
    sep = Resampled[ReadBinarySeparation[sim], {5.0}];
    om0 = InitialOrbitalFrequency[sim]; 
    D0 = BinaryBlackHoleParameters[sim]["D"]; 
    params = EccentricityReductionParameters[sim];
    
    eccFitWindow = 150 + {0, 2*2 Pi/om0};
    eps = 5; 
    If[MaxCoordinate[sep] + eps < eccFitWindow[[2]], 
     Print["WARNING: Simulation ", sim, " too short (", 
      MaxCoordinate[sep], 
      ") for fitting in " <> ToString[eccFitWindow]]];
    
    calcEcc[opts_List] :=
     Module[{e},
      e = 
       BinaryEccentricityFromSeparationDerivative[sep, eccFitWindow, 
        opts, Inspiral -> "PN", CorrectedSemiMajorAxis -> True, 
        MeanMotionGuess -> om0, SemiMajorAxisGuess -> D0, 
        OrbitalFrequency -> om0];
      If[e["RadialPeriodTooShort"] ,
       Print["Radial period too short"];
       If[ ! MemberQ[opts[[All, 1]], "FixEccentricFrequency"] && 
         prevEcc =!= None,
        Print["Using previous mean motion"];
        calcEcc[
         Join[{FixEccentricFrequency -> prevEcc["MeanMotion"]}, opts]],
        Print["No previous mean motion, or it didn't help"];
        Join[e, <|"Failed" -> True|>]],
       If[e["ConvergenceFailure"],
        Print["Convergence failure"];
        If[! MemberQ[opts, Inspiral -> "Polynomial"],
         Print["Using linear inspiral"];
         calcEcc[Join[{Inspiral -> "Polynomial"}, opts]],
         Print["Linear inspiral didn't help"];
         Join[e, <|"Failed" -> True|>]],
        e]]];
    
    ecc = calcEcc[{}];
    eccMeasured = ecc["Eccentricity"];
    {nextD, nextPr} = ReduceEccentricity[sim, ecc];
    ecc2 = 
     Join[ecc, <|"NextD" -> nextD, "NextPr" -> nextPr, 
       "RadialPeriod" -> (2 Pi/ecc["MeanMotion"]), 
       "OrbitalPeriod" -> 2 Pi/om0, 
       "Simulation" -> sim|>];, $Failed];
  Join[ecc2, <|"Failed" -> (result === $Failed || ecc2["Failed"])|>]];

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
