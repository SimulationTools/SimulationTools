
BeginPackage["Kicks2`", {"NR`", "DataTable`", "Memo`", "Profile`"}];

Kick;
LinearMomentumRadiated;
LinearMomentumFlux;

(*Begin["`Private`"];*)

spinWeightedSphericalHarmonic[s_, l_, m_, th_, ph_] = 
  (Factorial[l + m] Factorial[
       l - m] (2 l + 1) Factorial[l + s]^(-1) Factorial[
        l - s]^(-1) (4 Pi)^(-1))^(1/2) (Sin[th/2])^(2 l) Sum[
    Binomial[l - s, r] Binomial[l + s, r + s - m] (-1)^(l - r - s) Exp[
      I m ph] Cot[th/2]^(2 r + s - m), {r, Max[m - s, 0], Min[l - s, l + m]}];
(* This is taken from Golberg et al. and as such does NOT include the Cordon-Shortley phase *)

SpeedOfLight = 299792458.;

Kick[run_, dir_, r_, lMax_] :=
   SpeedOfLight/1000*
     Last@DepVar@LinearMomentumRadiated[run,dir,r,lMax];

LinearMomentumRadiated[run_, dir_, r_, lMax_] :=
  IntegrateDataTableZeroStart[LinearMomentumFlux[run,dir,r,lMax]];

integratePsi4[run_, l_, m_, r_] :=
 Profile["integratePsi4", IntegrateDataTableZeroEnd[Profile["ReadPsi4", ReadPsi4[run, l, m, r]]]];

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

LinearMomentumFlux[run_, dir_, r_, lMax_] :=
  Module[{l,m,lp,mp,fluxTerm,intPsi4Cache,allOverlaps,nonZeroOverlaps,terms,
          sum},
    mon = "Computing overlaps";
    allOverlaps = tabulateModePairs[harmonicOverlap[#, dir] &, lMax];
    nonZeroOverlaps = Select[allOverlaps, (#[[2]] =!= 0) &];

    tabulateModes[
      (mon = {"Integrating",#};
       intPsi4Cache[#] = DepVar[integratePsi4[run,Apply[Sequence,#],r]]) &, 
      lMax];

    fluxTerm[{{l_, m_, lp_, mp_},o_}] :=
      Profile["fluxComp", 
        (mon={"Computing flux", l,m,lp,mp};
         intPsi4Cache[{l,m}] * Conjugate[intPsi4Cache[{lp,mp}]] * N[o])];

    terms = Map[fluxTerm, nonZeroOverlaps];
    mon = "Adding terms...";
    sum = Profile["addTerms",
            MakeDataTable[MapThread[List, {IndVar[integratePsi4[run,2,2,r]],
                                           Apply[Plus, terms]}]]];
    mon = "Real part...";
    N[r^2/(16 Pi)] Re[sum]];

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
exclude it, I get complete agreement. *)

harmonicOverlap[{l_, m_, lp_, mp_}, 3] := 
 Profile["harmonicOverlap",
    (-1)^(mp-2) 2 Sqrt[Pi/3] harmonicOverlapThree[{l,m,-2},{lp,-mp,2},{1,0,0}]];

unitVectorComponent[1, th_, ph_] =
  Sin[th] Cos[ph];

unitVectorComponent[2, th_, ph_] =
  Sin[th] Sin[ph];

unitVectorComponent[3, th_, ph_] =
  Cos[th];

(*End[];*)

EndPackage[];
