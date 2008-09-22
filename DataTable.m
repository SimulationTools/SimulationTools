
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
NPoints;
ShiftDataTable;
DataTableRange;
ResampleDataTable;
ResampleDataTables;
Spacing;
DataTableInterval;
NDerivative;

Begin["`Private`"];

Format[DataTable[l_, attrs___]] := "DataTable"["..."];

SetAttributes[Redefine, HoldAll];

Redefine[f_[args___], newDef_] :=
  Module[{},
    Unprotect[f];
    f[args] := newDef;
    Protect[f]];

MakeDataTable[l_List] :=
  DataTable[l];

ToList[DataTable[l_, ___]] := l;

DepVar[DataTable[l_, ___]] :=
  Map[#[[2]]&, l];

IndVar[DataTable[l_, ___]] :=
  Map[#[[1]]&, l];

MakeDataTable[xs_, ys_] :=
  MakeDataTable[MapThread[List, {xs,ys}]];

MapData[f_, DataTable[l_, attrs___]] :=
  DataTable[Map[{#[[1]], f[#[[2]]]}&, l], attrs];

ApplyToList[f_, d_DataTable] :=
  d /. DataTable[l_, x___] -> DataTable[f[l], x];

commonAttributes[ds:List[DataTable[__]..]] :=
  Module[{attrs},
    attrs = Map[ListAttributes, ds];
(*    Print[attrs];*)
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

Redefine[Plus[a_Real|a_Integer|a_Complex, d:DataTable[__]],
  MapData[a + # &, d]];

Redefine[Times[a_Real|a_Integer|a_Complex, d:DataTable[__]],
  MapData[a * # &, d]];

Redefine[Abs[d:DataTable[__]],
  MapData[Abs, d]];

Redefine[Length[DataTable[d_,___]],
  Length[d]];

Redefine[Take[d:DataTable[___], args__],
  d /. DataTable[l_, x___] :> DataTable[Take[l,args],x]];

Redefine[Drop[d:DataTable[___], args__],
  d /. DataTable[l_, x___] :> DataTable[Drop[l,args],x]];

Redefine[Re[d:DataTable[___]],
  MapData[Re, d]];

Redefine[Im[d:DataTable[___]],
  MapData[Im, d]];

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

Phase[tb:List[{_, _Complex}...]] :=
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
  Module[{f, dt1, dt2},
    {dt1,dt2} = DataTableRange[d];
    If[t1 < dt1 || t2 > dt2 || t1 > t2 || dt < 0,
      Throw["ResampleDataTable: bad range spec " <> ToString[{t1,t2,dt}] <>
            " for DataTable with range " <> ToString[{dt1,dt2}]]];
    f = Interpolation[ToList[d]];
    MakeDataTable[Table[{t, f[t]}, {t, t1, t2, dt}]]];

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
