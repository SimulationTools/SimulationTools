(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["Convergence`", {"RunFiles`", "DataTable`", "Memo`"}];

ConvergenceMultiplier::usage = "ConvergenceMultiplier[{h1,h2,h3},p] computes the expected ratio (f[h1]-f[h2])/(f[h2]-f[h3]) when f[h] has a Taylor expansion f[h] = O[h^p].";
LoadConvergenceSeries;
RescaledErrors;
ConvergenceRate::usage = "ConvergenceRate[{f1,f2,f3}, {h1,h2,h3}] computes the convergence rate, p, of f[h] assuming f[h] = O[h^p].  f1, f2 and f3 can either be real numbers or DataTables, and the returned value will be of the same type.";
ResolutionCode;
RichardsonExtrapolate::usage = "RichardsonExtrapolate[{f1, f2}, {h1, h2}, p] gives the order p Richardson extrapolant of f1 and f2 at h = 0 assuming that f[h] = O[h^p]. f1 and f2 can either be real numbers or DataTables, and the returned value will be of the same type.";
RichardsonExtrapolate3::usage = "RichardsonExtrapolate3[{f1, f2, f3}, {h1, h2, h3}, p] gives the order p Richardson extrapolant of f1, f2 and f3 at h = 0 assuming that f[h] = O[h^p]. f1, f2 and f3 can either be real numbers or DataTables, and the returned value will be of the same type.";
RichardsonExtrapolationError::usage = "RichardsonExtrapolationError[{f1, f2, f3}, {h1, h2, h3}, p] gives the difference between the order p Richardson extrapolant of f1, f2 and f3 and of f2 and f3 at h = 0 assuming that f[h] = O[h^p]. This gives an error estimate for the latter.  f1, f2 and f3 can either be real numbers or DataTables, and the returned value will be of the same type.  NOTE: currently this function is only implemented for DataTables and the hs are computed from the NPoints attribute.";
ResName;
NPoints;

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

ConvergenceMultiplier[{h1_, h2_, h3_}, p_] :=
  Module[{eq, eqs, f, f0, f1, h, c},
    eq = f[h] == f0 + f1 h^p;
    eqs = {eq /. h -> h1, eq /. h -> h2, eq /. h -> h3};
    Return[c /. Solve[
     f[h1] - f[h2] == c (f[h2] - f[h3]) 
       /. (eqs /. Equal -> Rule), c][[1]] // N]];

Options[LoadConvergenceSeries] = {Downsample -> False, Interpolate -> False};

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

ConvergenceRateEquations = Table[CRF[i] == CRf0 + CRf1 CRh[i]^CRp, {i, 1, 3}];
ConvergenceRatePEquation = Eliminate[ConvergenceRateEquations, {CRf0, CRf1}];

RichardsonExtrapolationEquation = Eliminate[Take[ConvergenceRateEquations, 2], {CRf1}];
RichardExtrapolationExpression = CRf0 /. Solve[RichardsonExtrapolationEquation, CRf0][[1]];


ConvergenceRateEquations3 = Table[CRF[i] == CRf0 + CRf1 CRh[i]^CRp + CRf2 CRh[i]^(CRp+1), {i, 1, 3}];

RichardsonExtrapolationEquation3 = Eliminate[ConvergenceRateEquations3, {CRf1, CRf2}];
RichardExtrapolationExpression3 = CRf0 /. Solve[RichardsonExtrapolationEquation3, CRf0][[1]];

ConvergenceRate[{F1_?NumberQ, F2_, F3_}, {h1_, h2_, h3_}] := 
 Module[{rateEq, rate}, 
  rateEq = 
   ConvergenceRatePEquation /. {CRF[1] -> F1, CRF[2] -> F2, 
      CRF[3] -> F3, CRh[1] -> h1, CRh[2] -> h2, CRh[3] -> h3} // N;
  rate = Quiet[Check[CRp /. FindRoot[rateEq, {CRp, 1, 15}], None, {FindRoot::"cvmit", FindRoot::"lstol"}], {FindRoot::"cvmit", FindRoot::"lstol"}];
  If[rate < 0.1 || rate > 14.9, Return[None], Return[rate]]];

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

RichardsonExtrapolate[F1_, F2_, h1_, h2_, p_] :=
  Module[{},
    Return[RichardExtrapolationExpression /. {CRp -> p, CRF[1] -> F1, CRF[2] -> F2, 
      CRh[1] -> h1, CRh[2] -> h2}//N];
  ];

RichardsonExtrapolate[{F1_, F2_}, {h1_, h2_}, p_] :=
  Module[{},
    Return[RichardExtrapolationExpression /. {CRp -> p, CRF[1] -> F1, CRF[2] -> F2, 
      CRh[1] -> h1, CRh[2] -> h2}//N];
  ];

RichardsonExtrapolate[ds:{d1_DataTable, d2_DataTable}, p_] :=
  Module[{ns, hs, dts, ranges, ds2},
    dts = Map[Spacing, ds];
    ranges = Map[DataTableRange, ds];
    If[!Apply[Equal, dts] || !Apply[Equal,ranges], 
      ds2 = ResampleDataTables[ds],
      ds2 = ds];

    ns = Map[ReadAttribute[#, NPoints] &, ds2];
    hs = Map[1/#&, ns];
    Return[MapThreadData[RichardsonExtrapolate[#1,#2, hs[[1]], hs[[2]], p] &, ds2]]];

RichardsonExtrapolate[ds:{d1_DataTable, d2_DataTable}, hs:{h1_, h2_}, p_] :=
  Module[{dts, ranges, ds2},
    dts = Map[Spacing, ds];
    ranges = Map[DataTableRange, ds];
    If[!Apply[Equal, dts] || !Apply[Equal,ranges],
      ds2 = ResampleDataTables[ds],
      ds2 = ds];
    Return[MapThreadData[RichardsonExtrapolate[#1,#2, hs[[1]], hs[[2]], p] &, ds2]]];

RichardsonExtrapolate[ds:{d1_DataTable, d2_DataTable, d3_DataTable}, p_] :=
  RichardsonExtrapolate[{d2,d3},p];

RichardsonExtrapolate[{F1_, F2_, F3_}, {h1_, h2_, h3_}, p_] :=
  RichardsonExtrapolate[F2, F3, h2, h3, p];

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

End[];

EndPackage[];
