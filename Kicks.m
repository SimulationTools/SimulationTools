
BeginPackage["Kicks`", {"RunFiles`", "NR`", "DataTable`", "Memo`"}];

KickZ;
IntegratePsi4;
LinearMomentumFluxZModeMode;
SpeedOfLight = 299792458.;
IntegrateDataTableZeroStart;

Begin["`Private`"];

SpinWeightedSphericalHarmonic[s_, l_, m_, th_, 
   ph_] = (Factorial[l + m] Factorial[
       l - m] (2 l + 1) Factorial[l + s]^(-1) Factorial[
        l - s]^(-1) (4 Pi)^(-1))^(1/2) (Sin[th/2])^(2 l) Sum[
    Binomial[l - s, r] Binomial[l + s, r + s - m] (-1)^(l - r - s) Exp[
      I m ph] Cot[th/2]^(2 r + s - m), {r, Max[m - s, 0], Min[l - s, l + m]}];

overlap[l_, m_, lp_, mp_] := 
 Integrate[Cos[th] SpinWeightedSphericalHarmonic[-2, l, m, th, ph] Conjugate[
    SpinWeightedSphericalHarmonic[-2, lp, mp, th, ph] ] Sin[th], {th, 0, 
   Pi}, {ph, 0, 2 Pi}];

overlaps = << "/Users/ian/Projects/kicks/overlaps8.m";

cachedOverlap[l_, m_, lp_, mp_] :=
 Module[{result},
  result = overlapCache[{l, m, lp, mp}];
  If[Head[result] === overlapCache,
   Throw["Overlap cache does not include " <> ToString[{l, m, lp, mp}]]];
  result];

cachedOverlap[1, _, _, _] := 0;

cachedOverlap[_, _, 1, _] := 0;

defineOverlap[ov_] :=
 Module[{comb, val},
  {comb, val} = Apply[List, ov];
  overlapCache[Evaluate[comb]] = val];

Scan[defineOverlap, overlaps];

IntegrateDataTableZeroStart = integrateDataTableZeroStart;

integrateDataTableZeroStart[d_DataTable] := 
  integrateDataTable[d, {DataTableRange[d][[1]], 0}];

integrateDataTableZeroEnd[d_DataTable] := 
  integrateDataTable[d, {DataTableRange[d][[2]], 0}];

integrateDataTable[d_DataTable, {tbc_, fbc_}] :=
 Module[{tMin, tMax, dFn, gFn, g, t, dt, gTb},
  {tMin, tMax} = DataTableRange[d];
  If[tbc < tMin || tbc > tMax,
   Throw["integrateDataTable: boundary condition is not within range of \
DataTable"]];
  dt = Spacing[d];
  dFn = Interpolation[d];
  gFn = g /. 
    NDSolve[{D[g[t], t] == dFn[t], g[tbc] == fbc}, {g}, {t, tMin, tMax}][[
     1]];
  gTb = MakeDataTable[Table[{t, gFn[t]}, {t, tMin, tMax, dt}], 
    ListAttributes[d]]];

DefineMemoFunction[IntegratePsi4[runName_, n_, l_, m_, r_],
 integrateDataTableZeroEnd[ReadPsi4[ResName[runName, n], l, m, r]]];

LinearMomentumFluxZModeMode = linearMomentumFluxZModeMode

DefineMemoFunction[linearMomentumFluxZModeMode[runName_, n_, l_, m_, lp_, mp_, r_],
 Module[{hDot, hDotp, o, result, x},
  o = N[cachedOverlap[l, m, lp, mp]];
  If[o == 0,
   (* Don't bother reading every mode and integrating it if the contribution \
isn't going to be counted *)
   Return[MapData[0 &, ReadPsi4[ResName[runName, n], 2, 2, r]]]];
  
  {hDot, hDotp} = 
   List[IntegratePsi4[runName, n, l, m, r], 
    IntegratePsi4[runName, n, lp, mp, r]];
  
  (* These are all real anyway, because the os only couple m with m, 
  and the only complex part factor in the Ylms is  *)
  result =
   r^2/(16. Pi) Re[hDot Conjugate[hDotp]] o;
  Return[result]
  ]];

(* This is how I originally did it
linearMomentumFluxZ[runName_, n_, l_, m_, r_] :=
 Module[{},
  Re[linearMomentumFluxZ[runName, n, l, m, l, m, r]] + 
   2 Re[linearMomentumFluxZ[runName, n, l, m, l + 1, m, r]]] *)

(* This is how Nils does it *)
linearMomentumFluxZ[runName_, n_, l_, m_, r_] :=
 Module[{},
  linearMomentumFluxZModeMode[runName, n, l, m, l - 1, m, r] +
   linearMomentumFluxZModeMode[runName, n, l, m, l, m, r] + 
   linearMomentumFluxZModeMode[runName, n, l, m, l + 1, m, r]];

linearMomentumFluxZ[runName_, n_, lMax_, r_] :=
 Module[{},
  AddAttribute[
   Apply[Plus, 
    Flatten[Table[
      linearMomentumFluxZ[runName, n, l, m, r], {l, 2, lMax}, {m, -l, l}]]], 
   NPoints -> n]];

(* This is my new way, which should give the same answer as Nils' way *)

linearMomentumFluxZ[runName_, n_, lMax_, r_] :=
 Module[{flux},
  flux = Sum[If[l>2,Sum[linearMomentumFluxZModeMode[runName,n,l,m,l-1,m,r], 
                        {m,-(l-1),l-1}],0] + 
             Sum[linearMomentumFluxZModeMode[runName,n,l,m,l,m,r] +
                 linearMomentumFluxZModeMode[runName,n,l,m,l+1,m,r], {m,-l,l}],
             {l, 2, lMax}];
  AddAttribute[flux, NPoints -> n]];

KickZ[runName_, n_, lMax_, r_] :=
 SpeedOfLight/1000*
  Last[DepVar[
    integrateDataTableZeroStart[linearMomentumFluxZ[runName, n, lMax, r]]]];

KickZ[run_, n_, l_] :=
 Module[{},
  ExtrapolatedValue /. 
   ExtrapolateScalarFull[1, 
    Table[{r, KickZ[run, n, l, r]}, {r, 30, 70, 10}]]];

kickZWithRadiusExtrapolationError[run_, n_, l_] :=
 Module[{v1, v2},
  v1 = ExtrapolatedValue /. 
    ExtrapolateScalarFull[1, 
     Table[{r, KickZ[run, n, l, r]}, {r, 30, 70, 10}]];
  v2 = ExtrapolatedValue /. 
    ExtrapolateScalarFull[2, 
     Table[{r, KickZ[run, n, l, r]}, {r, 30, 70, 10}]];
  Return[{v1, v2 - v1}];
  ];

kickZWithResolutionAndRadiusError[run_, l_, ns_] :=
 Module[{kicksAndRadErrs, kicks, radErrs, p, kickRich, kick},
  kicksAndRadErrs = 
   Table[kickZWithRadiusExtrapolationError[run, n, l], {n, ns}];
  kicks = Map[First, kicksAndRadErrs];
  radErrs = Map[Last, kicksAndRadErrs];
  p = ConvergenceRate[kicks, 1/ns];
  kickRich = RichardsonExtrapolate[Drop[kicks, 1], Drop[1/ns, 1], 4];
  kick = Last[kicks];
  Return[{kick, kickRich - kick, Last[radErrs], p}];
  ];

kickZExtData[run_, n_, l_, p_] := {Data, ExtrapolatedCurve} /. 
  ExtrapolateScalarFull[p, 
   Table[{r, KickZ[run, n, l, r]}, {r, 30, 70, 10}]];

End[];

EndPackage[];
