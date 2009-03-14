
BeginPackage["DataTable`"];

DataTable;
MakeDataTable;
ToList;
DepVar;
IndVar;
MakeDataTable;
MapData;
ApplyToList;
MapThreadData;
Downsample;
MakeInterpolatingDataTable;
Phase;
AddAttribute;
ReadAttribute;
ListAttributes;
ShiftDataTable;
DataTableRange;
ResampleDataTable;
ResampleDataTables;
Spacing;
DataTableInterval;
NDerivative;
IntersectDataTables;

Begin["`Private`"];

Format[DataTable[l_, attrs___]] := "DataTable"["..."];

SetAttributes[Redefine, HoldAll];

Redefine[f_[args___], newDef_] :=
  Module[{},
    Unprotect[f];
    f[args] := newDef;
    Protect[f]];

SetAttributes[RedefineAsDataTable, HoldAll];

RedefineAsDataTable[f_[args___], newDef_] :=
  Module[{},
    Unprotect[f];
    DataTable /: f[args] := newDef;
    Protect[f]];

MakeDataTable[l_List] :=
  DataTable[l];

MakeDataTable[l_List, attrRules:{_ -> _}...] :=
  DataTable[l, Apply[Sequence,attrRules]];

MakeDataTable[f_InterpolatingFunction, dt_] :=
  Module[{tMin,tMax},
    tMin = f[[1]][[1]][[1]];
    tMax = f[[1]][[1]][[2]];
    MakeDataTable[Table[{t,f[t]},{t,tMin,tMax,dt}]]];                  

MakeDataTable[xs_List, ys_List] :=
  MakeDataTable[MapThread[List, {xs,ys}]];

ToList[DataTable[l_, ___]] := l;

DepVar[DataTable[l_, ___]] :=
  Map[#[[2]]&, l];

IndVar[DataTable[l_, ___]] :=
  Map[#[[1]]&, l];

MapData[f_, DataTable[l_, attrs___]] :=
  DataTable[Map[{#[[1]], f[#[[2]]]}&, l], attrs];

ApplyToList[f_, d_DataTable] :=
  d /. DataTable[l_, x___] -> DataTable[f[l], x];

commonAttributes[ds:List[DataTable[__]..]] :=
  Module[{attrs},
    attrs = Map[ListAttributes, ds];
(*    Print[attrs];*)
    Return[Apply[Intersection, attrs]]];

MapThreadData[f_, ds:List[DataTable[__]..]] :=
  Module[{lists, vals, xs, fOfVals, lengths},
    lists = Map[ToList, ds];
    lengths = Map[Length, lists];

    If[!Apply[Equal,lengths],
      Throw["MapThreadData: DataTables are not all of the same length"]];

    vals = Map[DepVar, ds];
    xs = IndVar[First[ds]];
    fOfVals = MapThread[f, vals];
    MakeDataTable[xs,fOfVals]];

(* FIXME: For some reason, this one doesn't work with more than two *)
RedefineAsDataTable[Plus[ds:(DataTable[__]...)],
  Module[{},
    MapThreadData[Plus[##] &, {ds}]]];

RedefineAsDataTable[Plus[a_Real|a_Integer|a_Complex, d:DataTable[__]],
  MapData[a + # &, d]];

RedefineAsDataTable[Times[a_Real|a_Integer|a_Complex, d:DataTable[__]],
  MapData[a * # &, d]];

RedefineAsDataTable[Times[d1:DataTable[__], d2:DataTable[__]],
  MapThreadData[Times, {d1, d2}]];

RedefineAsDataTable[Power[d:DataTable[__], n_Integer],
  MapData[Power[#,n] &, d]];

RedefineAsDataTable[Abs[d:DataTable[__]],
  MapData[Abs, d]];

RedefineAsDataTable[Conjugate[d:DataTable[__]],
  MapData[Conjugate, d]];

RedefineAsDataTable[Log[d:DataTable[__]],
  MapData[Log, d]];

RedefineAsDataTable[Length[DataTable[d_,___]],
  Length[d]];

RedefineAsDataTable[Take[d:DataTable[___], args__],
  d /. DataTable[l_, x___] :> DataTable[Take[l,args],x]];

RedefineAsDataTable[Drop[d:DataTable[___], args__],
  d /. DataTable[l_, x___] :> DataTable[Drop[l,args],x]];

RedefineAsDataTable[Re[d:DataTable[___]],
  MapData[Re, d]];

RedefineAsDataTable[Im[d:DataTable[___]],
  MapData[Im, d]];

AddAttribute[d:DataTable[x__], name_ -> val_] :=
  DataTable[x, name -> val];

AddAttributes[d:DataTable[x__], attrRules_List] :=
  DataTable[x, Apply[Sequence, attrRules]];

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

Phase[tb:List[{_, _}...]] :=
  Phase[Map[{#[[1]],{Re[#[[2]]], Im[#[[2]]]}} &, tb]];

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

Phase[d:DataTable[__]] :=
  ApplyToList[Phase, d];

(*    MakeDataTable[Phase[ToList[d]]];*)

Downsample[l_List, n_Integer] :=
  Take[l, {1, Length[l], n}];

MakeInterpolatingDataTable[d:DataTable[__], dt_] :=
  Module[{l, t1, t2, f, l2},
    l = ToList[d];
    t1 = First[l][[1]];
    t2 = Last[l][[1]];
    f = Interpolation[l];
    l2 = Table[{t, f[t]}, {t, t1, t2, dt}];
    d /. DataTable[_, x___] -> DataTable[l2, x]];

(* Plotting *)

Redefine[ListPlot[d:DataTable[___], args___],
   ListPlot[ToList[d], args]];

Redefine[ListPlot[ds:List[DataTable[___]..], args___],
   ListPlot[Map[ToList,ds], args]];

Redefine[ListLinePlot[d:DataTable[___], args___],
   ListLinePlot[ToList[d], args]];

Redefine[ListLinePlot[ds:List[DataTable[___]..], args___],
   ListLinePlot[Map[ToList,ds], args]];

Redefine[ListLogPlot[d:DataTable[___], args___],
   ListLogPlot[ToList[d], args]];

Redefine[ListLogPlot[ds:List[DataTable[___]..], args___],
   ListLogPlot[Map[ToList,ds], args]];

Redefine[Interpolation[d:DataTable[___], args___],
   Interpolation[ToList[d], args]];

ShiftDataTable[dt_, d : DataTable[__]] :=
 MakeDataTable[Map[{#[[1]] + dt, #[[2]]} &, ToList[d]]];

DataTableRange[dt:DataTable[__]] :=
  Module[{list = ToList[dt], t1, t2},
    t1 = First[list][[1]];
    t2 = Last[list][[1]];
    {t1,t2}];

ResampleDataTable[d:DataTable[__], {t1_, t2_, dt_}] :=
  Module[{f, dt1, dt2, l, l2},
    {dt1,dt2} = DataTableRange[d];
    If[t1 < dt1 || t2 > dt2 || t1 > t2 || dt < 0,
      Throw["ResampleDataTable: bad range spec " <> ToString[{t1,t2,dt}] <>
            " for DataTable with range " <> ToString[{dt1,dt2}]]];
    l = ToList[d];
    l2 = Select[l, Abs[#[[2]]] < 10^20 &];
    f = Interpolation[l2];
    AddAttributes[MakeDataTable[Table[{t, f[t]}, {t, t1, t2, dt}]], ListAttributes[d]]];

Spacing[d:DataTable[__]] :=
  Module[{ts},
    ts = IndVar[d];
    ts[[2]] - ts[[1]]];

ResampleDataTables[ds:{DataTable[__]...}] :=
  Module[{dts, dt, ranges, t1s, t2s, t1, t2},
    If[Length[ds] === 0, Return[{}]];
    dts = Map[Spacing, ds];
    dt = Apply[Min, dts];
    ranges = Map[DataTableRange, ds];
    t1s = Map[First, ranges];
    t2s = Map[Last, ranges];
    t1 = Apply[Max, t1s];
    t2 = Apply[Min, t2s];
    Map[ResampleDataTable[#, {t1, t2, dt}] &, ds]];

DataTableInterval[d_DataTable, {t1_, t2_}] :=
  d /. DataTable[l_, x___] :> DataTable[Select[l,#[[1]] >= t1 && #[[1]] < t2 &], x];

IntersectDataTables[d1_DataTable, d2_DataTable] :=
  Module[{d1Min, d1Max, d2Min, d2Max, dMin, dMax},
    {d1Min, d1Max} = DataTableRange[d1];
    {d2Min, d2Max} = DataTableRange[d2];

    dMin = Max[d1Min, d2Min];
    dMax = Min[d1Max, d2Max];

    Return[{DataTableInterval[d1,{dMin, dMax}], 
            DataTableInterval[d2,{dMin, dMax}]}]];

IntersectDataTables[{d1_DataTable, d2_DataTable}] :=
  Module[{d1Min, d1Max, d2Min, d2Max, dMin, dMax},
    {d1Min, d1Max} = DataTableRange[d1];
    {d2Min, d2Max} = DataTableRange[d2];

    dMin = Max[d1Min, d2Min];
    dMax = Min[d1Max, d2Max];

    Return[{DataTableInterval[d1,{dMin, dMax}], 
            DataTableInterval[d2,{dMin, dMax}]}]];

NDerivative[d_DataTable] :=
 Module[{diff, table1, table2, deriv},
  diff[{t1_, f1_}, {t2_, f2_}] :=
   {t1, (f2 - f1)/(t2 - t1)};
  table1 = Drop[ToList[d], 1];
  table2 = Drop[ToList[d], -1];
  deriv = MapThread[diff, {table2, table1}];
  Return[MakeDataTable[deriv]];
  ]


End[];

EndPackage[];
