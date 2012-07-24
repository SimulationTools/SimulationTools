(* Copyright 2010-2012 Ian Hinder, Barry Wardell and Aaryn Tonita

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

BeginPackage["Kicks`",
 {
  "DataRepresentations`",
  "DataTable`",
  "Error`",
  "Memo`",
  "NR`",
  "Profile`"
 }];

ReadKick::usage = "Kick[sim, dir, r, lMax] computes the kick component in km/s from the multipolar decomposition of Psi4 on a sphere at radius r in direction dir (an integer from 1 to 3) using modes up to l = lMax";
ReadKickVector::usage = "KickVector[sim, r, lMax] computes the vector kick in km/s from the multipolar decomposition of Psi4 on a sphere at radius r using modes up to l = lMax"
ReadLinearMomentumRadiated::usage = "LinearMomentumRadiated[sim, dir, r, lMax] computes the linear momentum radiated as a function of time from the multipolar decomposition of Psi4 on a sphere at radius r in direction dir (an integer from 1 to 3) using modes up to l = lMax.  It returns a DataTable.  The result is an integral of the result of LinearMomentumFlux with an initial boundary condition of zero.";
ReadLinearMomentumFlux::usage = "LinearMomentumFlux[sim, dir, r, lMax] computes the linear momentum flux as a function of time from the multipolar decomposition of Psi4 on a sphere at radius r in direction dir (an integer from 1 to 3) using modes up to l = lMax.  It returns a DataTable.  The result is the derivative of the result of LinearMomentumRadiated.";
AngularMomentumFlux(*::usage = "WARNING: THIS FUNCTION IS IN DEVELOPMENT AND HAS NOT BEEN TESTED. AngularMomentumFlux[sim, dir, r, lMax] computes the angular momentum flux as a function of time from the multipolar decomposition of Psi4 on a sphere at radius r in direction dir (an integer from 1 to 3) using modes up to l = lMax.  It returns a DataTable."*);
(* SpinWeightedSphericalHarmonic::usage = "SpinWeightedSphericalHarmonic[s, l, m, \[Theta], \[Phi]] gives the spin weighted spherical harmonic."; *)

Kick = ReadKick;
KickVector = ReadKickVector;
LinearMomentumRadiated = ReadLinearMomentumRadiated
LinearMomentumFlux = ReadLinearMomentumFlux

Begin["`Private`"];

(************************************************************************************)
(* Utility functions *)
(************************************************************************************)

(* Compute the spin-weighted spherical harmonic sYlm(th,ph).  This is
   taken from Goldberg et al. and as such does not include the
   Cordon-Shortley phase.  This is the opposite convention to
   Mathematica's SphericalHarmonicY, which does include the phase.

   When putting the below into Mathematica, it simplifies the expression
   to something it can handle more easily. We use this simplified expression
   in the actual definition.

SpinWeightedSphericalHarmonic[s_, l_, m_, th_, ph_] :=
  (Factorial[l + m] Factorial[
       l - m] (2 l + 1) Factorial[l + s]^(-1) Factorial[
        l - s]^(-1) (4 Pi)^(-1))^(1/2) (Sin[th/2])^(2 l) Sum[
    Binomial[l - s, r] Binomial[l + s, r + s - m] (-1)^(l - r - s) Exp[
      I m ph] Cot[th/2]^(2 r + s - m), {r, Max[m - s, 0], Min[l - s, l + m]}];
*)

SpinWeightedSphericalHarmonic[s_, l_, m_, th_, ph_] :=
  (Sqrt[((1 + 2*l)*(l - m)!*(l + m)!)/((l - s)!*(l + s)!)]*
  Piecewise[
    {{(-1)^(l - s)*E^(I*m*ph)*Binomial[l + s, -m + s]*
        Cot[th/2]^(-m + s)*HypergeometricPFQ[{-l - m, -l + s}, {1 - m + s}, -Cot[th/2]^2], m <= s}},
      (-1)^(l - m)*E^(I*m*ph)*Binomial[l - s, m - s]*
        Cot[th/2]^(m - s)*HypergeometricPFQ[{-l + m, -l - s}, {1 + m - s}, -Cot[th/2]^2]
  ]*Sin[th/2]^(2*l))/(2*Sqrt[Pi]);

SpeedOfLight = 299792458.;

integratePsi4[run_, l_, m_, r_] :=
  integratePsi4[Function[{lp,mp,rp}, ReadPsi4[run, lp, mp, rp]], l, m, r];

integratePsi4[psi4Reader_, l_, m_, r_] :=
 Profile["integratePsi4", IntegrateDataTableZeroEnd[Profile["ReadPsi4", psi4Reader[l, m, r]]]];

integratePsi4Twice[psi4Reader_,l_,m_,r_] :=
 IntegrateDataTableZeroEnd[integratePsi4[psi4Reader,l,m,r]];

unitVectorComponent[1, th_, ph_] =
  Sin[th] Cos[ph];

unitVectorComponent[2, th_, ph_] =
  Sin[th] Sin[ph];

unitVectorComponent[3, th_, ph_] =
  Cos[th];

(* Given a function f, return a list of the form {{{l,m,l',m'},
   f[{l,m,l',m'}]}, ...} for all valid combinations of the mode
   indices from l = 2 to l = lMax. *)
tabulateModePairs[f_, lMax_] :=
  Module[{l,m,lp,mp},
    Flatten[Table[{{l,m,lp,mp}, f[{l,m,lp,mp}]},
                  {l,2,lMax}, {m,-l,l}, {lp,2,lMax}, {mp,-lp,lp}], 3]];

(* Given a function f, return a list of the form {{{l,m},f[{l,m}]},
   ...}  for all valid combinations of the mode indices from l = 2 to
   l = lMax. *)
tabulateModes[f_, lMax_] :=
  Module[{l,m},
    Flatten[Table[{{l,m}, f[{l,m}]},
                  {l,2,lMax}, {m,-l,l}], 1]];

(* This is a direct computation of the integrals of spin-weighted
   spherical harmonics needed when computing the linear momentum
   flux. This should produce identical results to harmonicOverlap, but
   it is much slower.  Used in code checking. *)
harmonicOverlapDirect[{l_, m_, lp_, mp_}, dir_] := 
 Profile["harmonicOverlapDirect",
 Integrate[unitVectorComponent[dir, th, ph] *
           spinWeightedSphericalHarmonic[-2, l, m, th, ph] *
           Conjugate[spinWeightedSphericalHarmonic[-2, lp, mp, th, ph] ] *
           Sin[th],
           {th, 0, Pi}, {ph, 0, 2 Pi}]];

(* The integral of three spin-weighted spherical harmonics can be
  expressed in terms of Wigner 3-J symbols, which are known to
  Mathematica.  This is taken from
  http://en.wikipedia.org/wiki/Wigner_3-j_symbols (02-Mar-2011).
  http://en.wikipedia.org/w/index.php?title=Wigner_3-j_symbols&oldid=411796339 *)
harmonicOverlapThree[{l1_,m1_,s1_},{l2_,m2_,s2_},{l3_,m3_,s3_}] :=
  Sqrt[(2l1+1)(2l2+1)(2l3+1)/(4Pi)] * 
  Quiet[ThreeJSymbol[{l1,m1},{l2,m2},{l3,m3}] * 
        ThreeJSymbol[{l1,-s1},{l2,-s2},{l3,-s3}],
    {ClebschGordan::phy,ClebschGordan::tri}];

(* Rather than computing the harmonic overlaps by integration in
   Mathematica, which is very slow for large numbers of overlaps, we
   instead reduce the problem to the evaluation of Wigner 3J symbols
   which are known to Mathematica and are very fast.  We require the
   integral of two spin-weighted spherical harmonics (one
   complex-conjugated) with the x, y and z components of the unit
   vector, n^i.  Each of these components can be expressed simply in
   terms of spherical harmonics, which allows us to use the formula
   for the integral of three spherical harmonics in terms of Wigner 3J
   symbols.  In order to express Y* in terms of *, we use the relation
   which flips the sign of m and the spin-weight when
   complex-conjugating the harmonic, and introduces a sign factor.
   Note that equation 2.6 in Goldberg et al. is incorrect (the Ylm on
   the RHS should be Yl-m).  *)

(* Compute \int{ _{-2}Y_{lm} _{-2} Y_{l'm'}^* nx dOm}
   with nx = Sin[th] Cos[ph] = -(_0Y_{1-1} - _0Y_{11}) Sqrt[2Pi/3] *)
harmonicOverlap[{l_, m_, lp_, mp_}, 1] := 
 Profile["harmonicOverlap",
    - (-1)^(mp-2) Sqrt[2Pi/3] (harmonicOverlapThree[{l,m,-2},{lp,-mp,2},{1,-1,0}] - 
                             harmonicOverlapThree[{l,m,-2},{lp,-mp,2},{1,+1,0}])];

(* Compute \int{ _{-2}Y_{lm} _{-2} Y_{l'm'}^* ny dOm}
   with ny = Sin[th] Sin[ph] = -I (_0Y_{1-1} - _0Y_{11}) Sqrt[2Pi/3] *)
harmonicOverlap[{l_, m_, lp_, mp_}, 2] := 
 Profile["harmonicOverlap",
    -I (-1)^(mp-2) Sqrt[2Pi/3] (harmonicOverlapThree[{l,m,-2},{lp,-mp,2},{1,-1,0}] + 
                               harmonicOverlapThree[{l,m,-2},{lp,-mp,2},{1,+1,0}])];

(* Compute \int{ _{-2}Y_{lm} _{-2} Y_{l'm'}^* nz dOm}
   with nz = \cos(th) = 2 Sqrt[Pi/3] _0Y_{10} *)
harmonicOverlap[{l_, m_, lp_, mp_}, 3] := 
 Profile["harmonicOverlap",
    (-1)^(mp-2) 2 Sqrt[Pi/3] harmonicOverlapThree[{l,m,-2},{lp,-mp,2},{1,0,0} ]];

(************************************************************************************)
(* Linear momentum *)
(************************************************************************************)

Kick[run_String, dir_Integer, r_, lMax_Integer] :=
   SpeedOfLight/1000*
     Last@DepVar@LinearMomentumRadiated[run,dir,r,lMax];

DefineMemoFunction[KickMemo[run_String, dir_Integer, r_, lMax_Integer],
  Kick[run,dir,r,lMax]];

KickVector[run_String, r_, lMax_Integer] :=
  Table[Kick[run, dir, r, lMax], {dir, 1, 3}];

KickVectorMemo[run_String, r_, lMax_Integer] :=
  Table[KickMemo[run, dir, r, lMax], {dir, 1, 3}];

LinearMomentumRadiated[run_, dir_, r_, lMax_] :=
  IntegrateDataTableZeroStart[LinearMomentumFlux[run,dir,r,lMax]];

(* Call the "reader" version of LinearMomentumFlux with Psi4 from a run *)
LinearMomentumFlux[run_String, dir_, r_, lMax_] :=
  LinearMomentumFlux[
    Function[{l,m,rp}, ReadPsi4[run, l, m, rp]], 
    dir, r, lMax];

(* Compute the linear momentum flux from Psi4.  This code has been
   heavily optimised. TODO: include equations and references. *)
LinearMomentumFlux[psi4Reader_, dir_, r_, lMax_] :=
  Module[{fluxTerm, intPsi4Cache1, intPsi4Cache, allOverlaps, nonZeroOverlaps, terms,
          sum, nElems, tmp, nList, pDot},
    mon = {run, dir, r, "Computing overlaps"};

    (* Compute all the required overlaps of the harmonics with the
       unit vector in the direction "dir" between the mode indices l,
       m, l', m' *)
    allOverlaps = tabulateModePairs[harmonicOverlap[#, dir] &, lMax];

    nonZeroOverlaps = Select[allOverlaps, (#[[2]] =!= 0) &];
    nElems = 10^100;
    nList = {};

    (* Compute all the modes up to lMax *)
    tabulateModes[
      (mon = {run, dir, r, "Integrating",#};
       tmp = integratePsi4[psi4Reader,Apply[Sequence,#],r];
       intPsi4Cache1[#] = DepVar[tmp];
       nElems = Min[nElems,Length[intPsi4Cache1[#]]];
       nList = Append[nList, {#, nElems}]) &, 
      lMax];

    tabulateModes[
      (mon = {run, dir, r, "Resetting ranges",#};
       intPsi4Cache[#] = Take[intPsi4Cache1[#], nElems]) &, 
      lMax];

    fluxTerm[{{l_, m_, lp_, mp_},o_}] :=
      Profile["fluxComp", 
        (mon={run, dir, r, "Computing flux", l,m,lp,mp};
         intPsi4Cache[{l,m}] * Conjugate[intPsi4Cache[{lp,mp}]] * N[o])];

    terms = Map[fluxTerm, nonZeroOverlaps];
    mon = {run, dir, r, "Adding terms..."};
    sum = Profile["addTerms",
            MakeDataTable[MapThread[List, {Take[IndVar[integratePsi4[psi4Reader,2,2,r]],nElems],
                                           Apply[Plus, terms]}]]];
    mon = {run, dir, r, "Real part..."};
    pDot = N[r^2/(16 Pi)] Re[sum];
    mon = "";
    Return[pDot]];


(************************************************************************************)
(* Angular momentum *)
(************************************************************************************)

(* Only the z component of the angular momentum flux is implemented at
   the moment.  UNTESTED. TODO: give reference. *)
AngularMomentumFlux[run_String, dir_, r_, lMax_] :=
  AngularMomentumFlux[
    Function[{l,m,rp}, ReadPsi4[run, l, m, rp]], 
    dir, r, lMax];

(* Only the z component of the angular momentum flux is implemented at
   the moment. UNTESTED. *)
AngularMomentumFlux[psi4Reader_, 3, r_, lMax_] :=
  Module[{l,m},
    r^2/(4.0 Pi) Sum[m *
      Im[Conjugate[integratePsi4Twice[psi4Reader,l,m,r]] * 
                   integratePsi4[psi4Reader,l,m,r]], {l,2,lMax}, {m,-l,l}]];

End[];

EndPackage[];
