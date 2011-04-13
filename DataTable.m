(* ::Package:: *)

(* Copyright (C) 2010 Ian Hinder, Barry Wardell and Aaryn Tonita *)

BeginPackage["DataTable`", {"Profile`"}];

DataTable::usage = "DataTable[{{x,f},...}, attrs] is a one-dimensional table of data with attributes attrs.  attrs is of the form {attr -> value, ...}.  DataTable objects print as DataTable[] to avoid printing the content.";
MakeDataTable::usage = "MakeDataTable[{{x,f},...}, attrs] constructs a DataTable object out of the list and attributes passed.  attrs is of the form {attr -> value, ...}.  The independent variable, x, should be monotonically increasing and have a uniform spacing.  This is not currently checked.";
ToList::usage = "ToList[d] returns the list content of the DataTable d";
DepVar::usage = "DepVar[d] returns the dependent variable of the DataTable d.";
IndVar::usage = "IndVar[d] returns the independent variable of the DataTable d.";
MapData::usage = "MapData[f, d] maps f over the data (dependent variable) of the DataTable d";
MapIndVar::usage = "MapIndVar[f, d] maps f over the independent variable of the DataTable d";
ApplyToList::usage = "ApplyToList[f, d] maps f over the elements of the underlying list in DataTable d."
MapThreadData::usage = "MapThreadData[f, {d, ...}] threads f over the independent variables in the DataTable objects d, much like MapThread for lists.";
Downsample::usage = "Downsample[d, n] returns a version of DataTable d with only every nth element.";
MakeInterpolatingDataTable::usage = "MakeInterpolatingDataTable[d, dt] returns a resampled version of DataTable d which has been interpolated to have a spacing dt.  Deprecated: use ResampleDataTable instead.";
Phase::usage = "Phase[d] gives the phase of the complex variable in DataTable d.  The resulting phase will be continuous for smooth enough input data.";
AddAttribute::usage = "AddAttribute[d, attrname -> attrval] returns a copy of d with a new attribute added.";
AddAttributes::usage = "AddAttributes[d, {attrname -> attrvalue, ...}] returns a copy of DataTable d with new attributes added.";
ReadAttribute::usage = "ReadAttribute[d, attrname] returns the value of the named attribute from the DataTable d.";
ListAttributes::usage = "ListAttributes[d] returns a list of the attributes in the DataTable d in the form {attrname -> attrvalue, ...}.";
ShiftDataTable::usage = "ShiftDataTable[delta, d] returns a copy of DataTable d with the independent variable v replaced with v+delta.";
DataTableRange::usage = "DataTableRange[d] returns the first and last independent variable in DataTable d in the form {x1, x2}.";
ResampleDataTable::usage = "ResampleDataTable[d, {x1, x2, dx}, p] returns a copy of DataTable d in which the data has been interpolated with order p and runs from t1 to t2 with spacing dx.  p defaults to 3 if not specified, as in Interpolation.";
ResampleDataTables::usage = "ResampleDataTables[{d, ...}] returns the DataTables d after resampling to have a common range and spacing, which corresponds to the minimum spacing of the input set.";
Spacing::usage = "Spacing[d] returns the spacing of the independent variable in DataTable d.  This is a constant for the DataTable.";
DataTableInterval::usage = "DataTableInterval[d, {x1, x2}] returns a subset of the DataTable d in the range [x1, x2).";
DataTableDepVarInterval::usage = "DataTableDepVarInterval[d, {y1, y2}] returns a subset of the DataTable d in the range [y1, y2), where y is the dependent variable.";
NDerivative::usage = "NDerivative[d] returns the first derivative of the DataTable d.  This is first order accurate and the result omits the first and last points of d.";
IntersectDataTables::usage = "IntersectDataTables[{d1, d2, ...}] returns copies of the supplied set of DataTables but restricted to having their independent variables within the same range, which is the intersection of the ranges of the inputs.";
Frequency::usage = "Frequency[d] returns the first derivative of the complex phase of the DataTable d.";
IntegrateDataTable::usage = "IntegrateDataTable[d, {x, f}] returns the first integral, I, of the DataTable d, with the integration constant chosen such that I[x] = f.";
IntegrateDataTableZeroStart::usage = "IntegrateDataTableZeroStart[d] returns the first integral, I, of the DataTable d, with the integration constant chosen such that I[x1] = 0, where x1 is the lowest value of the independent variable in d.";
IntegrateDataTableZeroEnd::usage = "IntegrateDataTableZeroEnd[d] returns the first integral, I, of the DataTable d, with the integration constant chosen such that I[x2] = 0, where x2 is the highest value of the independent variable in d.";
Global`Sub::usage = "Sub[d1, d2] returns a DataTable corresponding to d1 - d2, where the dependent variables in d1 and d2 have been subtracted.  The DataTables are resampled and intersected in order to give a useful result if the ranges or spacings do not match.  Useful as the infix form; i.e. d1 ~Sub~ d2.";
Add::usage = "Add[d1, d2] returns a DataTable corresponding to d1 + d2, where the dependent variables in d1 and d2 have been subtracted.  The DataTables are resampled and intersected in order to give a useful result if the ranges or spacings do not match.  Useful as the infix form; i.e. d1 ~Add~ d2.";
Div::usage = "Div[d1, d2] returns a DataTable corresponding to d1 / d2, where the dependent variables in d1 and d2 have been divided.  The DataTables are resampled and intersected in order to give a useful result if the ranges or spacings do not match.  Useful as the infix form; i.e. d1 ~Div~ d2";
InterpolateWhereFunction::usage = "InterpolateWhereFunction[d,f] returns a new DataTable where the elements of d where the function returns true have been replaced by interpolated values."
Monotonise;
DataTableListLinePlot;

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
  DataTable[Developer`ToPackedArray[l]];

MakeDataTable[l_List, attrRules:{(_ -> _) ...}] :=
  DataTable[Developer`ToPackedArray[l], Apply[Sequence,attrRules]];

MakeDataTable[f_InterpolatingFunction, dt_] :=
  Module[{tMin,tMax},
    tMin = f[[1]][[1]][[1]];
    tMax = f[[1]][[1]][[2]];
    MakeDataTable[Table[{t,f[t]},{t,tMin,tMax,dt}]]];                  

(* This should be deprecated *)
MakeDataTable[xs_List, ys_List] :=
  MakeDataTable[MapThread[List, {xs,ys}]];

ToList[DataTable[l_, ___]] := l;

DepVar[DataTable[l_, ___]] :=
  Map[#[[2]]&, l];

IndVar[DataTable[l_, ___]] :=
  Map[#[[1]]&, l];

MapData[f_, DataTable[l_, attrs___]] :=
  DataTable[Map[{#[[1]], f[#[[2]]]}&, l], attrs];

MapIndVar[f_, DataTable[l_, attrs___]] :=
  DataTable[Map[{#[[1]], f[#[[1]]]}&, l], attrs];

ApplyToList[f_, d_DataTable] :=
  d /. DataTable[l_, x___] :> DataTable[f[l], x];

commonAttributes[ds:List[DataTable[__]..]] :=
  Module[{attrs},
    attrs = Map[ListAttributes, ds];
(*    Print[attrs];*)
    Return[Apply[Intersection, attrs]]];

MapThreadData[f_, ds:List[DataTable[__]..]] :=
  Module[{lists, vals, xs, fOfVals, lengths, tb, attrs},
    lists = Map[ToList, ds];
    lengths = Map[Length, lists];

    If[!Apply[Equal,lengths],
      Throw["MapThreadData: DataTables are not all of the same length"]];

    vals = Map[DepVar, ds];
    xs = IndVar[First[ds]];
    fOfVals = MapThread[f, vals];
    tb = MapThread[List, {xs,fOfVals}];
    attrs = Apply[Intersection, Map[ListAttributes, ds]];
    MakeDataTable[tb,attrs]];

RedefineAsDataTable[Plus[d1_DataTable, d2_DataTable],
  Profile["dtPlus",
    MapThreadData[Plus[##] &, {d1, d2}]]];

RedefineAsDataTable[Plus[a_Real|a_Integer|a_Complex, d:DataTable[__]],
  MapData[(a + #) &, d]];

RedefineAsDataTable[First[DataTable[l_,___]],
  l[[1]]];

RedefineAsDataTable[Last[DataTable[l_,___]],
  l[[-1]]];

RedefineAsDataTable[Times[a_Real|a_Integer|a_Complex, d:DataTable[__]],
  Profile["dtScalarTimes", MapData[a * # &, d]]];

RedefineAsDataTable[Times[d1:DataTable[__], d2:DataTable[__]],
  Profile["dtTimes",MapThreadData[Times, {d1, d2}]]];

RedefineAsDataTable[Dot[d1:DataTable[__], d2:DataTable[__]],
  MapThreadData[Dot, {d1, d2}]];

RedefineAsDataTable[Power[d:DataTable[__], n_Integer],
  MapData[Power[#,n] &, d]];

RedefineAsDataTable[Abs[d:DataTable[__]],
  MapData[Abs, d]];

RedefineAsDataTable[Norm[d:DataTable[__]],
  MapData[Norm, d]];

RedefineAsDataTable[Sqrt[d:DataTable[__]],
  MapData[Sqrt, d]];

RedefineAsDataTable[Conjugate[d:DataTable[__]],
  Profile["dtConjugate", MapData[Conjugate, d]]];

RedefineAsDataTable[Log[d:DataTable[__]],
  MapData[Log, d]];

RedefineAsDataTable[Log[b_, d:DataTable[__]],
  MapData[Log[b,#] &, d]];

RedefineAsDataTable[Log10[d:DataTable[__]],
  MapData[Log10, d]];

RedefineAsDataTable[Exp[d:DataTable[__]],
  MapData[Exp, d]];

RedefineAsDataTable[Length[DataTable[d_,___]],
  Length[d]];

DataTable /: Total[d_DataTable] := Total[DepVar[d]];

DataTable /: Max[d_DataTable] := Max[DepVar[d]];

DataTable /: PadRight[d_DataTable, n_] :=
  MakeDataTable[Transpose[{
    First[DataTableRange[d]] + (Range[n]-1) Spacing[d],
    PadRight[DepVar[d], n]}]];

DataTable /: PadLeft[d_DataTable, n_] :=
  MakeDataTable[Transpose[{
    First[DataTableRange[d]] - Spacing[d] (n-Length[d]+1) + Range[n] Spacing[d],
    PadLeft[DepVar[d], n]}]];

RedefineAsDataTable[Take[d:DataTable[___], args__],
  d /. DataTable[l_, x___] :> DataTable[Take[l,args],x]];

RedefineAsDataTable[Drop[d:DataTable[___], args__],
  d /. DataTable[l_, x___] :> DataTable[Drop[l,args],x]];

RedefineAsDataTable[Re[d:DataTable[___]],
  Profile["dtRe", MapData[Re, d]]];

RedefineAsDataTable[Im[d:DataTable[___]],
  MapData[Im, d]];

RedefineAsDataTable[FourierDCT[d_DataTable,args___],
  Module[{df, fs},
   (* I think these frequencies are right *)
   df = 2 Pi/-Subtract @@ DataTableRange[d];
   fs = Table[i df, {i, 0, Length[d] - 1}];
   MakeDataTable@MapThread[List, {fs, Abs@FourierDCT[DepVar@d,args]}]/
    N[Sqrt[Length[d]]]]];

RedefineAsDataTable[Fourier[d_DataTable,args___],
  Module[{amp, freq},
   amp = Fourier[DepVar[d],args];
   freq = Range[0, Length[d] - 1]/(Spacing[d]*Length[d]);
   Return[MakeDataTable[Transpose[{freq, amp}]]];
  ]
];

RedefineAsDataTable[InverseFourier[d_DataTable,args___],
  Module[{amp, freq},
   amp = InverseFourier[DepVar[d],args];
   freq = Range[0, Length[d] - 1]/(Spacing[d]*Length[d]);
   Return[MakeDataTable[Transpose[{freq, amp}]]];
  ]
];

RedefineAsDataTable[Length[d_DataTable],
	Length[ToList[d]]
];

InterpolateWhereFunction[d_DataTable, f_] :=
  Module[{dInterpolater},
  dInterpolater=Interpolation@MakeDataTable@DeleteCases[ToList[d],_?f];
  MakeDataTable[ToList[d]/. {t_,x_}:>{t,dInterpolater[t]}/;f[{t,x}]]];

Global`Sub[d1_DataTable, d2_DataTable, p_:8] :=
  Apply[Subtract, ResampleDataTables[{d1, d2}, p]];

Add[ds:(_DataTable..), p_Integer:8] :=
  Apply[Plus, ResampleDataTables[{ds}, p]];

Div[d1_DataTable, d2_DataTable] :=
  Apply[Divide, ResampleDataTables[{d1, d2}]];

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
   currentPhase = If[!(x==0 && y ==0), ArcTan[x, y], 0];
   If[currentPhase - previousPhase > Pi, cycles--];
   If[currentPhase - previousPhase < -Pi, cycles++];
   previousPhase = currentPhase;
   phaseTb[[i]] = {t, 2 Pi cycles + currentPhase}];
  Return[phaseTb]];

Phase[d:DataTable[__]] :=
  ApplyToList[Phase, d];

Frequency[d:DataTable[__]] :=
  NDerivative[Phase[d]];

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

DataTableListLinePlot[d:DataTable[___], args___] :=
  ListLinePlot[ToList[d], args];

Redefine[ListLinePlot[d:DataTable[___], args___],
   ListLinePlot[ToList[d], args]];

Redefine[ListLinePlot[ds:List[DataTable[___]..], args___],
   ListLinePlot[Map[ToList,ds], args]];

Redefine[ListLogPlot[d:DataTable[___], args___],
   ListLogPlot[ToList[d], args]];

Redefine[ListLogPlot[ds:List[DataTable[___]..], args___],
   ListLogPlot[Map[ToList,ds], args]];

Redefine[ListLogLogPlot[d:DataTable[___], args___],
   ListLogLogPlot[ToList[d], args]];

Redefine[ListLogLogPlot[ds:List[DataTable[___]..], args___],
   ListLogLogPlot[Map[ToList,ds], args]];

Redefine[Interpolation[d:DataTable[___], args___],
   Interpolation[ToList[d], args]];

ShiftDataTable[dt_, d : DataTable[__]] :=
 AddAttributes[MakeDataTable[Map[{#[[1]] + dt, #[[2]]} &, ToList[d]]], ListAttributes[d]];

DataTableRange[dt:DataTable[__]] :=
  Module[{list = ToList[dt], t1, t2},
    t1 = First[list][[1]];
    t2 = Last[list][[1]];
    {t1,t2}];

ResampleDataTable[d:DataTable[__], dt_?NumberQ, p_Integer] :=
  Module[{t1, t2},
    {t1, t2} = DataTableRange[d];
    ResampleDataTable[d, {t1, t2, dt}, p]
  ];

ResampleDataTable[d:DataTable[__], {t1_, t2_, dt_}] :=
   ResampleDataTable[d, {t1, t2, dt}, 8];

ResampleDataTable[d:DataTable[__], {t1_, t2_, dt_}, p_Integer] :=
  Module[{f, dt1, dt2},
    {dt1,dt2} = DataTableRange[d];
    If[t1 < dt1 || t2 > dt2 || t1 > t2 || dt < 0,
      Throw["ResampleDataTable: bad range spec " <> ToString[{t1,t2,dt}] <>
            " for DataTable with range " <> ToString[{dt1,dt2}]]];
    f = Interpolation[d, InterpolationOrder -> p];
    AddAttributes[MakeDataTable[Table[{t, f[t]}, {t, t1, t2, dt}]], ListAttributes[d]]];

Spacing[d:DataTable[__]] :=
  Module[{ts},
    ts = IndVar[d];
    Min[Drop[ts,1] - Drop[RotateRight[ts],1]]]

ResampleDataTables[ds:{DataTable[__]...}, p_:8] :=
  Module[{dts, dt, ranges, t1s, t2s, t1, t2},
    If[Length[ds] === 0, Return[{}]];
    dts = Map[Spacing, ds];
    dt = Apply[Min, dts];
    ranges = Map[DataTableRange, ds];
    t1s = Map[First, ranges];
    t2s = Map[Last, ranges];
    t1 = Apply[Max, t1s];
    t2 = Apply[Min, t2s];
    Map[ResampleDataTable[#, {t1, t2, dt}, p] &, ds]];

DataTableInterval[d_DataTable, {t1_, t2_}] := Module[{range, tMin, tMax},
  range = DataTableRange[d];
  tMin = If[SameQ[t1, All], range[[1]], t1];
  tMax = If[SameQ[t2, All], range[[2]], t2];
  d /. DataTable[l_, x___] :> DataTable[Select[l,#[[1]] >= tMin && #[[1]] < tMax &], x]
];

DataTableDepVarInterval[d_DataTable, {y1_, y2_}] :=
  d /. DataTable[data_, attrs___] :> DataTable[Select[data,#[[2]] >= y1 && #[[2]] < y2 &], attrs];

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

IntersectDataTables[ds:{(_DataTable)...}] :=
  Module[{ranges, mins, maxs, min, max, ds2},
    ranges = Map[DataTableRange, ds];
    mins = Map[First, ranges];
    maxs = Map[Last, ranges];

    min = Max[mins];
    max = Min[maxs];

    ds2 = Map[DataTableInterval[#,{min, max}] &, ds];
    ds2];


NDerivative[d_DataTable] :=
 Module[{diff, table1, table2, deriv},
  diff[{t1_, f1_}, {t2_, f2_}] :=
   {t1, (f2 - f1)/(t2 - t1)};
  table1 = Drop[ToList[d], 1];
  table2 = Drop[ToList[d], -1];
  deriv = MapThread[diff, {table2, table1}];
  Return[MakeDataTable[deriv]];
  ]

  Options[IntegrateDataTable] = {InterpolationOrder->3};

IntegrateDataTable[d_DataTable, {tbc_, fbc_}, opts:OptionsPattern[]] :=
 Module[{tMin, tMax, dFn, gFn, g, t, dt, gTb},
  {tMin, tMax} = DataTableRange[d];
  If[tbc < tMin || tbc > tMax,
   Throw["integrateDataTable: boundary condition is not within range of \
DataTable"]];
  dt = Spacing[d];
	dFn = Interpolation[d,InterpolationOrder->OptionValue[InterpolationOrder]];
  gFn = g /. 
    NDSolve[{D[g[t], t] == dFn[t], g[tbc] == fbc}, {g}, {t, tMin, tMax}, MaxSteps -> 1000000][[
     1]];
  gTb = MakeDataTable[Table[{t, gFn[t]}, {t, tMin, tMax, dt}], 
    ListAttributes[d]]];

IntegrateDataTableZeroStart[d_DataTable] := 
  IntegrateDataTable[d, {DataTableRange[d][[1]], 0}];

IntegrateDataTableZeroEnd[d_DataTable] := 
  IntegrateDataTable[d, {DataTableRange[d][[2]], 0}];

Monotonise[{}] := {};

Monotonise[{{a_, b_}}] := {{a, b}};

Monotonise[{{x1_, y1_}, {x2_, y2_}, rest___}] :=
 If[y1 < y2, {{x1, y1}, {x2, y2}}~Join~Monotonise[{rest}],
  {{x1, y1}, {x2, y2 + y1}}~Join~
   Monotonise[Map[{#[[1]], #[[2]] + y1} &, {rest}]]];

Monotonise[d_DataTable] :=
 MakeDataTable[Monotonise[ToList[d]]];

singleToList[d_DataTable] := MapData[If[SameQ[Head[#], List], #, {#}]&, d]

DataTable /: Join[ds:DataTable[__]...] := Module[{resampled, joineddata},
  resampled = ResampleDataTables[{ds}];
  joineddata=MapThread[Join, DepVar/@singleToList/@resampled];
  MakeDataTable[Thread[{IndVar[resampled[[1]]] ,joineddata}]]
];

DataTable /: Export[file_String, dt_DataTable, type___] := Export[file, Flatten/@ToList[dt], type];

End[];

EndPackage[];
