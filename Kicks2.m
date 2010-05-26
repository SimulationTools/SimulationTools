(* ::Package:: *)

BeginPackage["Kicks2`", {"NR`", "DataTable`", "Memo`", "Profile`"}];

Kick;
LinearMomentumRadiated;
LinearMomentumFlux;
harmonicOverlap;

(*Begin["`Private`"];*)

spinWeightedSphericalHarmonic[s_, l_, m_, th_, ph_] = 
  (Factorial[l + m] Factorial[
       l - m] (2 l + 1) Factorial[l + s]^(-1) Factorial[
        l - s]^(-1) (4 Pi)^(-1))^(1/2) (Sin[th/2])^(2 l) Sum[
    Binomial[l - s, r] Binomial[l + s, r + s - m] (-1)^(l - r - s) Exp[
      I m ph] Cot[th/2]^(2 r + s - m), {r, Max[m - s, 0], Min[l - s, l + m]}];
(* This is taken from Goldberg et al. and as such does NOT include the 
   Cordon-Shortley phase *)

SpeedOfLight = 299792458.;

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

integratePsi4[run_, l_, m_, r_] :=
  integratePsi4[Function[{l,m,rp}, ReadPsi4[run, l, m, rp]], l, m, r];

integratePsi4[psi4Reader_, l_, m_, r_] :=
 Profile["integratePsi4", IntegrateDataTableZeroEnd[Profile["ReadPsi4", psi4Reader[l, m, r]]]];

integratePsi4Twice[psi4Reader_,l_,m_,r_] :=
 IntegrateDataTableZeroEnd[integratePsi4[psi4Reader,l,m,r]];

(*LinearMomentumFlux[run_, dir_, r_, lMax_] :=
  Module[{l,m,lp,mp,fluxTerm,intPsi4Cache},
    Table[mon={"Integrating",l,m}; intPsi4Cache[l,m] = integratePsi4[run,l,m,r], {l,2,lMax},{m,-l,l}];
    fluxTerm[l_, m_, lp_, mp_] :=
      Module[{overlap = harmonicOverlap[l,m,lp,mp,dir]},
        If[overlap === 0, 0.0, intPsi4Cache[l,m] * 
                          Conjugate[intPsi4Cache[lp,mp]] * N[overlap]]];
    N[r^2/(16 Pi)] Re@Sum[mon={"Adding",l,m,lp,mp}; fluxTerm[l,m,lp,mp], 
                          {l,2,lMax}, {m,-l,l}, {lp,2,lMax}, {mp,-lp,lp}]];*)

tabulateModePairs[f_, lMax_] :=
  Module[{l,m,lp,mp},
    Flatten[Table[{{l,m,lp,mp}, f[{l,m,lp,mp}]},
                  {l,2,lMax}, {m,-l,l}, {lp,2,lMax}, {mp,-lp,lp}], 3]];

tabulateModes[f_, lMax_] :=
  Module[{l,m},
    Flatten[Table[{{l,m}, f[{l,m}]},
                  {l,2,lMax}, {m,-l,l}], 1]];

LinearMomentumFlux[run_String, dir_, r_, lMax_] :=
  LinearMomentumFlux[
    Function[{l,m,rp}, ReadPsi4[run, l, m, rp]], 
    dir, r, lMax];

LinearMomentumFlux[psi4Reader_, dir_, r_, lMax_] :=
  Module[{l,m,lp,mp,fluxTerm,intPsi4Cache1,intPsi4Cache,allOverlaps,nonZeroOverlaps,terms,
          sum, nElems, tmp, nList, pDot},
    mon = {run, dir, r, "Computing overlaps"};
    allOverlaps = tabulateModePairs[harmonicOverlap[#, dir] &, lMax];
    nonZeroOverlaps = Select[allOverlaps, (#[[2]] =!= 0) &];
    nElems = 10^100;
    nList = {};
    tabulateModes[
      (mon = {run, dir, r, "Integrating",#};
       tmp = integratePsi4[psi4Reader,Apply[Sequence,#],r];
       intPsi4Cache1[#] = DepVar[tmp];
       nElems = Min[nElems,Length[intPsi4Cache1[#]]];
       nList = Append[nList, {#, nElems}]) &, 
      lMax];

(*    Print["nElems = ", nElems];
    Print["nList = ", nList]; *)
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

harmonicOverlapDirect[{l_, m_, lp_, mp_}, dir_] := 
 Profile["harmonicOverlapDirect",
 Integrate[unitVectorComponent[dir, th, ph] *
           spinWeightedSphericalHarmonic[-2, l, m, th, ph] *
           Conjugate[spinWeightedSphericalHarmonic[-2, lp, mp, th, ph] ] *
           Sin[th],
           {th, 0, Pi}, {ph, 0, 2 Pi}]];

harmonicOverlapThree[{l1_,m1_,s1_},{l2_,m2_,s2_},{l3_,m3_,s3_}] :=
  (* (-1)^(m1+s1) *) Sqrt[(2 l1+1)(2l2+1)(2l3+1)/(4Pi)] * 
  Quiet[ThreeJSymbol[{l1,m1},{l2,m2},{l3,m3}] * 
        ThreeJSymbol[{l1,-s1},{l2,-s2},{l3,-s3}], {ClebschGordan::phy,ClebschGordan::tri}];
(* The integral of three spin-weighted spherical harmonics comes from
the Wikipedia article on 3 j-m symbols.  It contains a factor of
(-1)^(m1+s1) which appears very unsymmetric.  When included, I get
sign differences with the integrals computed symbolically.  When I
exclude it, I get complete agreement. The integral without the factor
is also in Alcubierre's NR book, but from the unit vector expressions
in SHs, he seems to be using the CS phase, which is inconsistent. *)

harmonicOverlap[{l_, m_, lp_, mp_}, 1] := 
 Profile["harmonicOverlap",
    - (-1)^(mp-2) Sqrt[2Pi/3] (harmonicOverlapThree[{l,m,-2},{lp,-mp,2},{1,-1,0}] - 
                             harmonicOverlapThree[{l,m,-2},{lp,-mp,2},{1,+1,0}])];

harmonicOverlap[{l_, m_, lp_, mp_}, 2] := 
 Profile["harmonicOverlap",
    -I (-1)^(mp-2) Sqrt[2Pi/3] (harmonicOverlapThree[{l,m,-2},{lp,-mp,2},{1,-1,0}] + 
                               harmonicOverlapThree[{l,m,-2},{lp,-mp,2},{1,+1,0}])];

(* \int{ _{-2}Y_{lm} _{-2} Y_{l'm'}^* \cos(th) dOm}

  _{-2} Y_{l'm'}^* = {-1}^{mp-2} _{2} Y_{l'-m'}
  \cos(th)         = 2 Sqrt[Pi/3] 0Y10
  
=> \int{ _{-2}Y_{lm}   {-1}^{mp-2} _{2} Y_{l'-m'}    2 Sqrt[Pi/3] 0Y10   dOm} *)
harmonicOverlap[{l_, m_, lp_, mp_}, 3] := 
 Profile["harmonicOverlap",
    (-1)^(mp-2) 2 Sqrt[Pi/3] harmonicOverlapThree[{l,m,-2},{lp,-mp,2},{1,0,0} ]];

unitVectorComponent[1, th_, ph_] =
  Sin[th] Cos[ph];

unitVectorComponent[2, th_, ph_] =
  Sin[th] Sin[ph];

unitVectorComponent[3, th_, ph_] =
  Cos[th];

(************************************************************************************)
(* Angular momentum *)
(************************************************************************************)
AngularMomentumFlux[run_String, dir_, r_, lMax_] :=
  AngularMomentumFlux[
    Function[{l,m,rp}, ReadPsi4[run, l, m, rp]], 
    dir, r, lMax];

AngularMomentumFlux[psi4Reader_, 3, r_, lMax_] :=
  Module[{l,m},
    r^2/(4.0 Pi) Sum[m *
      Im[Conjugate[integratePsi4Twice[psi4Reader,l,m,r]] * 
                   integratePsi4[psi4Reader,l,m,r]], {l,2,lMax}, {m,-l,l}]];

(*End[];*)

EndPackage[];
