
(*--------------------------------------------------------------------
  Functional Programming
  --------------------------------------------------------------------*)

SetAttributes[DefineMemoFunction, HoldAll];

Memo[f_] :=
   Function[
       If[Head[DataCache[f, {##}]] === DataCache,
          DataCache[f, {##}] = Apply[f,{##}]];
       DataCache[f, {##}]];

DefineMemoFunction[name_[args___], body_] :=
  Module[{f},
    SetDelayed[f[args], body];
    name = Memo[f]];

SetAttributes[Redefine, HoldAll];

Redefine[f_[args___], newDef_] :=
  Module[{},
    Unprotect[f];
    f[args] := newDef;
    Protect[f]];

(* List functions *)

Phase[tb:{{_, {_, _}}...}] :=
  Module[{phaseTb,x,y,t,previousPhase, i, currentPhase = 0, cycles =
          0, nPoints},
  nPoints = Length[tb];
  phaseTb = Table[i, {i, 1, nPoints}];
  For[i = 1, i <= nPoints, i++,
   t = tb[[i, 1]];
   x = tb[[i, 2, 1]];
   y = tb[[i, 2, 2]];
   currentPhase = ArcTan[x, y];
   If[currentPhase - previousPhase > Pi, cycles--];
   If[currentPhase - previousPhase < -Pi, cycles++];
   previousPhase = currentPhase;
   phaseTb[[i]] = {t, 2 Pi cycles + currentPhase}];
  Return[phaseTb]];

Downsample[l_List, n_Integer] :=
  Take[l, {1, Length[l], n}];

(* DataTable definitions *)

(* Make sure that we preserve all attributes where it makes sense *)

Format[DataTable[l_, attrs___]] := "DataTable"["..."];

MakeDataTable[l_List] :=
  DataTable[l];

ToList[DataTable[l_, ___]] := l;

DepVar[DataTable[l_, ___]] :=
  Map[#[[2]]&, l];

IndVar[DataTable[l_, ___]] :=
  Map[#[[1]]&, l];

MakeDataTable[xs_, ys_] :=
  MakeDataTable[MapThread[List, {xs,ys}]];

MapData[f_, DataTable[l_, ___]] :=
  DataTable[Map[{#[[1]], f[#[[2]]]}&, l]];

ApplyToList[f_, d_DataTable] :=
  d /. DataTable[l_, x___] -> DataTable[f[l], x];

commonAttributes[ds:List[DataTable[__]..]] :=
  Module[{attrs},
    attrs = Map[ListAttributes, ds];
    Print[attrs];
    Return[Apply[Intersection, attrs]]];

MapThreadData[f_, ds:List[DataTable[__]...]] :=
  Module[{lists, vals, xs, fOfVals},
    lists = Map[ToList, ds];
    vals = Map[DepVar, ds];
    xs = IndVar[First[ds]];
    fOfVals = MapThread[f, vals];
    MakeDataTable[xs,fOfVals]];

Redefine[Plus[d1:DataTable[__], d2:DataTable[__]],
  MapThreadData[#1+#2&, {d1,d2}]];

Redefine[Plus[Times[a_Real|a_Integer|a_Complex, d:DataTable[__]],
  MapData[a + # &, d]]];

Redefine[Times[a_Real|a_Integer|a_Complex, d:DataTable[__]],
  MapData[a * # &, d]];

Redefine[Length[DataTable[d_,___]],
  Length[d]];

Redefine[Take[d:DataTable[___], args__],
  d /. DataTable[l_, x___] :> DataTable[Take[l,args],x]];

AddAttribute[d:DataTable[x__], name_ -> val_] :=
  DataTable[x, name -> val];

ReadAttribute[d:DataTable[l_, attrs___], name_] :=
  Module[{val},
    val = name /. {attrs};
    If[val === name,
      Throw["Attribute "<>ToString[name]<>" not found in "<>ToString[d]]];
    Return[val]];

ListAttributes[d:DataTable[l_, attrs___]] :=
  {attrs};

Downsample[d_DataTable, n_Integer] :=
  ApplyToList[Downsample[#, n] &, d];

(* File reading *)

FileInRun[runName_String, fileName_] :=
  RunDirectory <> "/" <> runName <> "-all/" <> fileName;

ReadColumnFile[fileName_String, cols_List] :=
  Module[{list},
    If[FileType[fileName] === None, Throw["File " <> name <> " not found"]];
    list = ReadList[fileName, Real, RecordLists->True];
    Return[Map[Extract[#, Map[List, cols]] &, list]]];

(* Simulation data *)

ReadPsi4[runName_String, l_?NumberQ, m_?NumberQ, rad_?NumberQ] :=
  Module[{fileName, list, psi4},
    fileName = FileInRun[runName, "Ylm_WEYLSCAL4::Psi4ri_l" <>
             ToString[l] <> "_m" <> ToString[m] <> "_r" <> 
             ToString[rad] <> ".00.asc"];
    list = ReadColumnFile[fileName, {1,2,3}];
    psi4 = Map[{#[[1]], #[[2]] + I #[[3]]}&, list];
    Return[AddAttribute[MakeDataTable[psi4], RunName -> runName]]];

DefineMemoFunction[ReadMinTrackerCoordinates[runName_String, tracker_Integer],
  Module[{name},
      Print["Reading..."];
      fileName = FileInRun[runName, "MinTracker" <> ToString[tracker] <> ".asc"];
      list = ReadColumnFile[fileName, {2,3,4,5}];
      list2 = Map[{#[[1]], {#[[2]], #[[3]], #[[4]]}}&, list];
      Return[MakeDataTable[list2]]]];

ReadMinTrackerCoordinate[runName_String, tracker_Integer, coord_Integer] :=
  Module[{coords},
    coords = ReadMinTrackerCoordinates[runName, tracker];
    MapData[#[[coord]]&, coords]];

(* Data conversion *)

Phase[d:DataTable[__]] :=
    MakeDataTable[Phase[ToList[d]]];

ReadMinTrackerRadius[runName_String] :=
  Module[{x0, x1, rad},
    x0 = ReadMinTrackerCoordinates[runName, 0];
    x1 = ReadMinTrackerCoordinates[runName, 1];
    rad = MapThreadData[Norm[#1-#2] &, {x0, x1}];
    Return[rad];
  ];

ReadMinTrackerPhase[runName_String] :=
  Module[{x0, x1, rad},
    x0 = ReadMinTrackerCoordinates[runName, 0];
    x1 = ReadMinTrackerCoordinates[runName, 1];
    xyTrans = MapThreadData[Take[#1-#2,2] &, {x0, x1}]; (* Project into xy plane *)
    Return[Phase[xyTrans]];
  ];



(* Plotting *)

Redefine[ListPlot[d:DataTable[___], args___],
   ListPlot[ToList[d], args]];

Redefine[ListPlot[ds:List[DataTable[___]..], args___],
   ListPlot[Map[ToList,ds], args]];

Redefine[ListLinePlot[d:DataTable[___], args___],
   ListLinePlot[ToList[d], args]];

Redefine[ListLinePlot[ds:List[DataTable[___]..], args___],
   ListLinePlot[Map[ToList,ds], args]];

(* Convergence *)

ConvergenceMultiplier[{h1_, h2_, h3_}, p_] :=
  Module[{eq, eqs, f, f0, f1},
    eq = f[h] == f0 + f1 h^p;
    eqs = {eq /. h -> h1, eq /. h -> h2, eq /. h -> h3};
    Return[C /. Solve[
     f[h1] - f[h2] == C (f[h2] - f[h3]) 
       /. (eqs /. Equal -> Rule), C][[1]] // N]];

Options[LoadConvergenceSeries] = {Downsample -> False, Interpolate -> False};

LoadConvergenceSeries[runBase_,ns:{n1_,n2_,n3_},reader_,namer_, opts___] :=
  Module[{downsample = Downsample /. {opts} /. Options[LoadConvergenceSeries],
          interpolate = Interpolate /. {opts} /. Options[LoadConvergenceSeries],
          fs,len,tables},
    
    fs = Map[reader, Map[runBase <> namer[#] &, ns]];
    dts = Map[Spacing, fs];
    Print[dts];
    If[!(Length[Union[dts]] === 1) && !downsample && !interpolate,
       downsample = True;
       Print["Automatically downsampling"]];

    If[downsample,
      gcd = Apply[GCD, ns];
      Print[gcd];
      dsFacs = Map[#/gcd&, ns];
      Print[dsFacs];
      fs = MapThread[Downsample[#1, #2] &, {fs, dsFacs}]];

    If[interpolate,
      dt = Apply[Min, dts];
      fs = Map[MakeInterpolatingDataTable[#, dt] &, fs]];

    len = Apply[Min, Map[Length, fs]];
    tables = Map[Take[#, len] &, fs];
    tsWithAttrs = MapThread[AddAttribute[#1, NPoints->#2] &, {tables, ns}];

    Return[tsWithAttrs];
  ];

Spacing[d:DataTable[__]] :=
  Module[{ts},
    ts = IndVar[d];
    ts[[2]] - ts[[1]]];

MakeInterpolatingDataTable[d:DataTable[__], dt_] :=
  Module[{l, t1, t2, f, l2},
    l = ToList[d];
    t1 = First[l][[1]];
    t2 = Last[l][[1]];
    f = Interpolation[l];
    l2 = Table[{t, f[t]}, {t, t1, t2, dt}];
    d /. DataTable[_, x___] -> DataTable[l2, x]];

RescaledErrors[p_, ds:List[DataTable[__]..]] :=
  Module[{d1, d2, d3, ns, hs, d12, d23, cm},
    {d1, d2, d3} = ds;
    ns = Map[ReadAttribute[#, NPoints] &, ds];
    hs = Map[1/#&, ns];
    d12 = d1 - d2;
    d23 = d2 - d3;
    cm = ConvergenceMultiplier[hs, p];
    Return[{d12/cm, d23}];
  ];

ConvergenceRateEquations = Table[CRF[i] == CRf0 + CRf1 CRh[i]^CRp, {i, 1, 3}];
ConvergenceRatePEquation = Eliminate[ConvergenceRateEquations, {CRf0, CRf1}];

ConvergenceRate[{F1_?NumberQ, F2_, F3_}, {h1_, h2_, h3_}] := 
 Module[{rateEq, rate}, 
  rateEq = 
   ConvergenceRatePEquation /. {CRF[1] -> F1, CRF[2] -> F2, 
      CRF[3] -> F3, CRh[1] -> h1, CRh[2] -> h2, CRh[3] -> h3} // N;
  rate = Check[CRp /. FindRoot[rateEq, {CRp, 1, 10}], None, FindRoot::lstol];
  If[rate < 0.1 || rate > 9.9, Return[None], Return[rate]]];

ConvergenceRateSlow[fs:{f1_, f2_, f3_}, hs:{h1_, h2_, h3_}] :=
  Module[{eq, eqs, el, a0, a1},
    eq = f == a0 + a1 h^p;
    eqs = MapThread[eq /.{f -> #1, h -> #2} &, {fs, hs}];
    el = Eliminate[eqs, {a0, a1}];
    p /. FindRoot[el, {p, 1, 10}]];

ConvergenceRate[ds:{DataTable[__]..}] :=
  Module[{hs},
    hs = Map[1/ReadAttribute[#, NPoints] &, ds];
    MapThreadData[ConvergenceRate[{#1, #2, #3}, hs] &, ds]];
