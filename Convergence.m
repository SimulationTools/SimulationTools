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

BeginPackage["SimulationTools`Convergence`",
 {
  "SimulationTools`ArgumentChecker`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`DataRegion`",
  "SimulationTools`Error`"
 }];

ConvergenceMultiplier::usage = "ConvergenceMultiplier[{h1,h2,h3},p] computes the expected ratio (f[h1]-f[h2])/(f[h2]-f[h3]) when f[h] has a Taylor expansion f[h] = O[h^p].";
ConvergenceRate::usage = "ConvergenceRate[{f1,f2,f3}, {h1,h2,h3}] computes the convergence rate, p, of f[h] assuming f[h] = O[h^p].  f1, f2 and f3 can either be real numbers or DataTables, and the returned value will be of the same type.";
RichardsonExtrapolant::usage = "RichardsonExtrapolant[{f1, f2}, {h1, h2}, p] gives the order p Richardson extrapolant of f1 and f2 at h = 0 assuming that f[h] = O[h^p]. f1 and f2 can either be real numbers or DataTables, and the returned value will be of the same type.";
ConvergenceRateConstantRatio;

LoadConvergenceSeries;
RescaledErrors;
ResolutionCode;
RichardsonExtrapolate3(*::usage = "RichardsonExtrapolate3[{f1, f2, f3}, {h1, h2, h3}, p] gives the order p Richardson extrapolant of f1, f2 and f3 at h = 0 assuming that f[h] = O[h^p]. f1, f2 and f3 can either be real numbers or DataTables, and the returned value will be of the same type."*);
RichardsonExtrapolationError(*::usage = "RichardsonExtrapolationError[{f1, f2, f3}, {h1, h2, h3}, p] gives the difference between the order p Richardson extrapolant of f1, f2 and f3 and of f2 and f3 at h = 0 assuming that f[h] = O[h^p]. This gives an error estimate for the latter.  f1, f2 and f3 can either be real numbers or DataTables, and the returned value will be of the same type.  NOTE: currently this function is only implemented for DataTables and the hs are computed from the NPoints attribute."*);
ResName;
NPoints;
RichardsonError;
RichardsonRelativeError;
RescaledDifferences;

RichardsonExtrapolate = RichardsonExtrapolant;
ConvergenceOrder;
SimulationResolutionLabelFromName;

Begin["`Private`"];

(*--------------------------------------------------------------------
  Convergence
  --------------------------------------------------------------------*)
ResolutionCode[n_Integer] := 
 Module[{}, 
  If[! (Mod[n, 4] === 0), 
   Error["Number of points must be a multiple of 4"]];
  If[n < 16, Error["Number of points must be at least 16"]];
  If[n > 116, Error["Number of points must be less than 116"]];
  Return[FromCharacterCode[n/4 - 4 + 97]]];

ResName[s_String, n_] := s <> ResolutionCode[n];

DocumentationBuilder`SymbolDescription["ConvergenceMultiplier"] =
  "compute a convergence multiplier given specified resolutions and convergence order";

ConvergenceMultiplier[{h1_, h2_, h3_}, p_] :=
  Module[{eq, eqs, f, f0, f1, h, c},
    eq = f[h] == f0 + f1 h^p;
    eqs = {eq /. h -> h1, eq /. h -> h2, eq /. h -> h3};
    Return[Simplify[c /. Solve[
     f[h1] - f[h2] == c (f[h2] - f[h3]) 
       /. (eqs /. Equal -> Rule), c][[1]] // N]]];

Options[LoadConvergenceSeries] = {"Downsample" -> False, "Interpolate" -> False};

LoadConvergenceSeries[runBase_,ns:{n1_,n2_,n3_},reader_,namer_, opts___] :=
  Module[{downsample = Downsample /. {opts} /. Options[LoadConvergenceSeries],
          interpolate = Interpolate /. {opts} /. Options[LoadConvergenceSeries],
          fs, len, tables, dts, gcd, dsFacs, dt, tsWithAttrs},
    
    fs = Map[reader, Map[runBase <> namer[#] &, ns]];
    dts = Map[Spacing, fs];
(*    Print[dts];*)
    If[!(Length[Union[dts]] === 1) && !downsample && !interpolate,
       downsample = True;
(*       Print["Automatically downsampling"] *) ];

    If[downsample,
      gcd = Apply[GCD, ns];
(*      Print[gcd];*)
      dsFacs = Map[#/gcd&, ns];
(*      Print[dsFacs];*)
      fs = MapThread[Downsample[#1, #2] &, {fs, dsFacs}]];

    If[interpolate,
      dt = Apply[Min, dts];
      fs = Map[MakeInterpolatingDataTable[#, dt] &, fs]];

    len = Apply[Min, Map[Length, fs]];
    tables = Map[Take[#, len] &, fs];
    tsWithAttrs = MapThread[AddAttribute[#1, NPoints->#2] &, {tables, ns}];

    Return[tsWithAttrs];
  ];

RescaledErrors[p_, ds:List[DataTable[__]..]] :=
  Module[{d1, d2, d3, ns, hs, d12, d23, cm, ds2, dts, ranges},
    dts = Map[Spacing, ds];
    ranges = Map[DataTableRange, ds];
    If[!Apply[Equal, dts] || !Apply[Equal,ranges], 
      ds2 = ResampleDataTables[ds],
      ds2 = ds];
    {d1, d2, d3} = ds2;
    ns = Map[ReadAttribute[#, NPoints] &, ds2];
    hs = Map[1/#&, ns];
    d12 = d1 - d2;
    d23 = d2 - d3;
    cm = ConvergenceMultiplier[hs, p];
    Return[{d12/cm, d23}];
  ];

RescaledDifferences[fs_, hs_, p_] :=
 {fs[[1]]~Sub~
   fs[[2]], (fs[[2]]~Sub~fs[[3]]) ConvergenceMultiplier[hs, p]}

ConvergenceRateEquations = Table[CRF[i] == CRf0 + CRf1 CRh[i]^CRp, {i, 1, 3}];
ConvergenceRatePEquation = Eliminate[ConvergenceRateEquations, {CRf0, CRf1}];

RichardsonExtrapolationEquation = Eliminate[Take[ConvergenceRateEquations, 2], {CRf1}];
RichardExtrapolationExpression = CRf0 /. Solve[RichardsonExtrapolationEquation, CRf0][[1]];


ConvergenceRateEquations3 = Table[CRF[i] == CRf0 + CRf1 CRh[i]^CRp + CRf2 CRh[i]^(CRp+1), {i, 1, 3}];

RichardsonExtrapolationEquation3 = Eliminate[ConvergenceRateEquations3, {CRf1, CRf2}];
RichardExtrapolationExpression3 = CRf0 /. Solve[RichardsonExtrapolationEquation3, CRf0][[1]];

StandardDefinition[ConvergenceRate] = True;

DocumentationBuilder`SymbolDescription["ConvergenceRate"] =
  "compute the convergence rate as a function of time";

ConvergenceRate[{F1_?NumberQ, F2_, F3_}, {h1_, h2_, h3_}] := 
 Module[{rateEq, rate}, 
  rateEq = 
   ConvergenceRatePEquation /. {CRF[1] -> F1, CRF[2] -> F2, 
      CRF[3] -> F3, CRh[1] -> h1, CRh[2] -> h2, CRh[3] -> h3} // N;

  (* This can happen if F1, F2 and F3 are all 0 *)
  If[rateEq === True,
    Return[None];
  ];

  rate = Quiet[Check[CRp /. FindRoot[rateEq, {CRp, 1, 15}], None, {FindRoot::"cvmit", FindRoot::"lstol"}], {FindRoot::"cvmit", FindRoot::"lstol"}];
  If[rate < 0.1 || rate > 14.9, Return[None], Return[rate]]];

StandardDefinition[ConvergenceRateSlow] = True;

ConvergenceRateSlow[fs:{f1_, f2_, f3_}, hs:{h1_, h2_, h3_}] :=
  Module[{eq, eqs, el, a0, a1},
    eq = f == a0 + a1 h^p;
    eqs = MapThread[eq /.{f -> #1, h -> #2} &, {fs, hs}];
    el = Eliminate[eqs, {a0, a1}];
    p /. FindRoot[el, {p, 1, 10}]];

ConvergenceRate[ds:{DataTable[__]..}] :=
  Module[{hs, dts, ds2, ranges},
    dts = Map[Spacing, ds];
    ranges = Map[DataTableRange, ds];
    If[!Apply[Equal, dts] || !Apply[Equal,ranges], 
      ds2 = ResampleDataTables[ds],
      ds2 = ds];
    hs = Map[1.0/ReadAttribute[#, NPoints] &, ds2];
    MapThreadData[ConvergenceRate[{#1, #2, #3}, hs] &, ds2]];

ConvergenceRate[ds:{DataTable[__]..}, hs_List] :=
  Module[{dts, ds2, ranges},
    dts = Map[Spacing, ds];
    ranges = Map[DataTableRange, ds];
    If[!Apply[Equal, dts] || !Apply[Equal,ranges], 
      ds2 = ResampleDataTables[ds],
      ds2 = ds];
    MapThreadData[ConvergenceRate[{#1, #2, #3}, hs] &, ds2]];

ConvergenceRateConstantRatio[{d1_, d2_, d3_}, {h1_, h2_, h3_}] := 
  (* TODO: give an error or make it work if h1/h2 != h2/h3 *)
  Module[
    {rho = h2/h1}, 
    Quiet[
      Log[(d2-d3)/(d1-d2)]/Log[rho] /.
      {_Complex -> None, 
       Indeterminate -> None, Infinity -> None},
      {Power::infy, 
       Infinity::indet}]];

StandardDefinition[RichardsonExtrapolate] = True;

RichardsonExtrapolant[F1_, F2_, h1_, h2_, p_] :=
  Module[{},
    Return[RichardExtrapolationExpression /. {CRp -> p, CRF[1] -> F1, CRF[2] -> F2, 
      CRh[1] -> h1, CRh[2] -> h2}//N];
  ];

RichardsonExtrapolant[{F1_, F2_}, {h1_, h2_}, p_] :=
  Module[{},
    Return[RichardExtrapolationExpression /. {CRp -> p, CRF[1] -> F1, CRF[2] -> F2, 
      CRh[1] -> h1, CRh[2] -> h2}//N];
  ];

(* TODO: This should work on any DataRepresentation or numeric type function *)
RichardsonExtrapolant[ds:{d1_DataTable, d2_DataTable}, p_] :=
  Module[{ns, hs, dts, ranges, ds2},
    dts = Map[Spacing, ds];
    ranges = Map[DataTableRange, ds];
    If[!Apply[Equal, dts] || !Apply[Equal,ranges], 
      ds2 = ResampleDataTables[ds],
      ds2 = ds];

    ns = Map[ReadAttribute[#, NPoints] &, ds2];
    hs = Map[1/#&, ns];
    Return[MapThreadData[RichardsonExtrapolate[#1,#2, hs[[1]], hs[[2]], p] &, ds2]]];

RichardsonExtrapolant[ds:{d1_DataTable, d2_DataTable}, hs:{h1_, h2_}, p_] :=
  Module[{dts, ranges, ds2},
    If[h1 <= h2,
       Error["Grid spacings (and data) in RichardsonExtrapolate should be given in order of increasing resolution"]];
    dts = Map[Spacing, ds];
    ranges = Map[DataTableRange, ds];
    If[!Apply[Equal, dts] || !Apply[Equal,ranges],
      ds2 = ResampleDataTables[ds],
      ds2 = ds];
    Return[MapThreadData[RichardsonExtrapolate[#1,#2, hs[[1]], hs[[2]], p] &, ds2]]];

RichardsonExtrapolant[ds:{d1_DataTable, d2_DataTable, d3_DataTable}, p_] :=
  RichardsonExtrapolate[{d2,d3},p];

RichardsonExtrapolate[{F1_, F2_, F3_}, {h1_, h2_, h3_}, p_] :=
  RichardsonExtrapolate[F2, F3, h2, h3, p];

StandardDefinition[RichardsonExtrapolate3] = True;

RichardsonExtrapolate3[F1_, F2_, F3_, h1_, h2_, h3_, p_] :=
  Module[{},
    Return[RichardExtrapolationExpression3 /. {CRp -> p, CRF[1] -> F1, CRF[2] -> F2, CRF[3] -> F3,
      CRh[1] -> h1, CRh[2] -> h2, CRh[3] -> h3}//N];
  ];

RichardsonExtrapolate3[ds:{d1_DataTable, d2_DataTable, d3_DataTable}, p_] :=
  Module[{ns, hs},
    ns = Map[ReadAttribute[#, NPoints] &, ds];
    hs = Map[1/#&, ns];
    RichardsonExtrapolate3[ds,hs]];

RichardsonExtrapolate3[ds:{d1_DataTable, d2_DataTable, d3_DataTable}, hs:{h1_,h2_,h3_}, p_] :=
  Module[{},
    Return[MapThreadData[RichardsonExtrapolate3[#1,#2,#3, hs[[1]], hs[[2]], hs[[3]], p] &,
                         ResampleDataTables[{d1,d2,d3}]]]];

RichardsonExtrapolationError[ds:{d1_DataTable, d2_DataTable, d3_DataTable}, p_] :=
    RichardsonExtrapolate[Drop[ds,1], p] - RichardsonExtrapolate3[ds, p];

RichardsonError[fs:{f1_,f2_}, hs:{h1_, h2_}, p_] :=
  Sub[f2,If[p==="exponential", f1, RichardsonExtrapolate[fs,hs,p]]];

RichardsonRelativeError[fs:{f1_,f2_}, hs:{h1_, h2_}, p_] :=
  RichardsonError[fs,hs,p]/f2;


ConvergenceOrder[{f1_?NumberQ, f2_?NumberQ, 
   f3_?NumberQ}, {h1_?NumberQ, h2_?NumberQ, h3_?NumberQ}] :=
  If[! (h1 > h2 > h3), 
  Error["ConvergenceOrder: Grid spacings ("<>ToString[{h1,h2,h3}]<>") must be monotonically increasing"],
  If[f1 == f2 == f3, 0.,
   If[(f1 - f2) (f3 - f2) >= 0,(* Non-monotonic input values *) None,
    Module[{p},
     Replace[
      Quiet[Check[FindRoot[(f1 - f2)/(f2 - f3) == ((h1/h2)^p - 
           1)/(1 - (h2/h3)^-p), {p, 1}], None, {FindRoot::lstol}],FindRoot::lstol],
      {{p -> p0_} :> p0,
       None :> None,
       x_ :> 
        Error["Could not determine convergence order: " <> 
          ToString[x]]}]]]]];

ConvergenceOrder[{d1_DataTable, d2_DataTable, d3_DataTable}, {h1_, h2_, h3_}] :=
 MapThread[ConvergenceOrder[{##}, {h1, h2, h3}] &, {d1, d2, d3}];

ConvergenceOrder[{d1_DataRegion, d2_DataRegion, d3_DataRegion}, {h1_, h2_, h3_}] :=
 MapThread[ConvergenceOrder[{##}, {h1, h2, h3}] &, {d1, d2, d3}/.None->0.];

ConvergenceOrder[{d12_?NumberQ, d23_?NumberQ}, {h1_?NumberQ, h2_?NumberQ, h3_?NumberQ}] :=
  If[! (h1 > h2 > h3), 
  Error["ConvergenceOrder: Grid spacings must be monotonically increasing"],
  If[d12 == d23 == 0., 0.,
   If[-d12 d23 >= 0,(* Non-monotonic input values *) None,
    Module[{p},
     Replace[
      Quiet[Check[FindRoot[d12/d23 == ((h1/h2)^p - 
           1)/(1 - (h2/h3)^-p), {p, 1}], None, {FindRoot::lstol}],FindRoot::lstol],
      {{p -> p0_} :> p0,
       None :> None,
       x_ :> 
        Error["Could not determine convergence order: " <> 
          ToString[x]]}]]]]];

SimulationResolutionLabelFromName[sim_String] :=
  Module[{},

    (* Need to handle the following cases:

       [/path/to/]sim_NNN => NNN

       [/path/to/]sim/Ev/LevN -> N

       [/path/to/]sim/nNN => NN

       I think we can always strip the dirname of the simulation name,
       if present. *)

    simName = FileNameTake[sim, -1];

    StringReplace[simName, {
      StartOfString ~~ __ ~~ "_" ~~ n:(DigitCharacter..) ~~ EndOfString :> "n="<>n,
      StartOfString ~~ "Lev" ~~ n:(DigitCharacter..) ~~ EndOfString :> "Lev"<>n,
      StartOfString ~~ "n" ~~ n:(DigitCharacter..) ~~ EndOfString :> "n="<>n}]];

End[];

EndPackage[];
