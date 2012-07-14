(* Copyright (C) 2010 Ian Hinder, Barry Wardell and Aaryn Tonita *)

BeginPackage["DataTable`",
 {
  "DataRepresentations`",
  "Error`",
  "Profile`"
 }];

DataTable::usage = "DataTable[{{x1,f1},{x2,f2},...,{xn,fn}}] is a one-dimensional table of data (fi) with an associated coordinate (xi).  DataTable objects print as DataTable[...] to avoid printing the potentially large data content.  The independent variables, xi, should be monotonically increasing real numbers and may have a variable increment.  The dependent variables, fi, can be of any type for which the basic mathematical operations (+, -, *, /) make sense.";
ToDataTable::usage = "ToDataTable[{{x1,f1},{x2,f2},...,{xn,fn}}] constructs a DataTable object out of the list passed. The independent variables, xi, should be monotonically increasing real numbers and may have a variable increment.  The dependent variables, fi, can be of any type for which the basic mathematical operations (+, -, *, /) make sense.
ToDataTable[dr] converts a 1-dimensional DataRegion into a DataTable.";

Phase::usage = "Phase[d] gives the phase of the complex variable in DataTable d.  The resulting phase will be continuous for sufficiently smooth input data.";
Frequency::usage = "Frequency[d] returns the first derivative of the complex phase of the DataTable d.";

UniformSpacingQ::usage = "UniformSpacingQ[d] returns True if the DataTable has a uniform grid spacing and False if the grid spacing is variable.  The grid spacings are considered uniform if they are equal up to a tolerance of 1e-5.";
MonotonicQ::usage = "MonotonicQ[d] returns True if the independent variable in the DataTable d is monotonically increasing";

CoordinateAtInterpolatedMax::usage = "CoordinateAtInterpolatedMax[d] finds the time at which a maximum occurs in the range of the DataTable d. This time is interpolated and may not coincide with a data point in the DataTable.";
InterpolatedMax::usage = "InterpolatedMax[d] returns the maximum value of the interpolant of a DataTable d";

InterpolatedWhere::usage = "InterpolatedWhere[d, f] returns a new DataTable where the elements of d where the function returns true have been replaced by interpolated values."
AntiDerivative::usage = "AntiDerivative[d, {x, f}] returns the first integral, I, of the DataTable d, with the integration constant chosen such that I[x] = f.";



MapList::usage = "MapList[f,d] maps f over the {t,f} pairs in the DataTable d.";
(* TODO: Remove this *)
ShiftDataTable::usage = "ShiftDataTable[delta, d] returns a copy of DataTable d with the independent variable v replaced with v+delta.";
(* TODO: Add Shifted[d,delta] which subtracts delta from the coordinate of d, shifting the data to increased values of the coordinate *)
(* TODO: Rename as Interval. *)
DataTableInterval::usage = "DataTableInterval[d, {x1, x2}] returns a subset of the DataTable d in the range [x1, x2).";
(* TODO: Add Slab to be compatible with DataRegion (maybe) *)
(* TODO: Remove this, and implement Select on DataTables instead?  Select[d, y1 < #2 < y2 &] *)
DataTableDepVarInterval::usage = "DataTableDepVarInterval[d, {y1, y2}] returns a subset of the DataTable d in the range [y1, y2), where y is the dependent variable.";
(* TODO: Rename as RestrictedToCommonInterval. *)
IntersectDataTables::usage = "IntersectDataTables[{d1, d2, ...}] returns copies of the supplied set of DataTables but restricted to having their independent variables within the same range, which is the intersection of the ranges of the inputs.";
(* TODO: Rename as FunctionInverse *)
InvertDataTable::usage = "InvertDataTable[d] returns a DataTable in which the dependent and independent variable of the DataTable d are swapped.  Note that this might lead to a non-monotonic (and hence invalid) DataTable.";
(* TODO: Rename as CoordinateAtMax *)
LocateMaximumPoint::usage = "LocateMaximumPoint[d] finds the time at which a maximum occurs in the DataTable d. This time is guaranteed coincide with a data point in the DataTable.";
(* TODO: Rename; this is the composition of d and p^-1.  We have Inverse already, maybe we should also have Composition? Then we wouldn't need this function as it would be easy: Composition[d, Inverse[p], {t1, t2, dp}].  Composition will likely need to interpolate. *)
FunctionOfPhase::usage = "FunctionOfPhase[d, p, {t1, t2}, dp] returns a DataTable consisting of the data of the DataTable d evaluated as a function of the DataTable p.  t1 and t2 are the coordinate ranges in p on which to evaluate d.  dp is the uniform grid spacing of p to use.  This function should be renamed, as p does not have to be a phase.";

(* TODO: Decide if non-monotonic DataTables are allowed/checked *)
(* TODO: Implement a Monotonic[d] to make a non-monotonic DataTable monotonic?  Or maybe this happens as an option to ToDataTable? *)


(****************************************************************)
(* Experimental                                                 *)
(****************************************************************)

(* TODO: Make these like DataRegions, i.e. Metadata. *)
AddAttribute;
AddAttributes;
(* This should just be Attribute[d], but that is too generic *)
ReadAttribute;
(* TODO: Could this be AttributeNames? *)
ListAttributes;

(* TODO: This also only operates on lists.  It might be good to have this defined on DataTables as well.  The list version should be made internal, but the DataTable version could be public. *)
PartitionTable;

(* TODO: Move to another package. This only works for uniform DataTables.  Should we have a separate Filtering package? Should it be Filtered? *)
FilterDCT;

(****************************************************************)
(* Deprecated *)
(****************************************************************)

MakeDataTable;
DepVar;
IndVar;
MapData;
MapIndVar;
ApplyToList;
MapThreadData;
Downsample;
Spacing;
DataTableRange;
DataTableNormL2;
MakeInterpolatingDataTable;
ResampleDataTable;
ResampleDataTables;
PhaseOfFrequency;
DataTableListLinePlot;
ShiftPhase;
AbsOfPhase;
UniformGridQ;
IntegrateDataTableZeroStart;
IntegrateDataTableZeroEnd;
LocateMaximum;
MaximumValue;
MakeUniform;
InterpolateWhereFunction;
IntegrateDataTable;

Begin["`Private`"];

(****************************************************************)
(* DataTable *)
(****************************************************************)

(* TODO: Remove this hack *)
DataTable /: Dimensions[d_DataTable] := {Length[ToListOfData[d]]};

SetAttributes[DataTable, {ReadProtected}];

DataTable /: MakeBoxes[d_DataTable, StandardForm] :=
 Module[{dims, range},
  dims  = Dimensions[d];
  range = CoordinateRanges[d];

  TagBox[
   RowBox[
    {"DataTable",
     "[",
     RowBox[
      {
       "\"<\"",
       "\[InvisibleSpace]",
       Sequence@@Riffle[dims, ","],
       "\[InvisibleSpace]",
       "\">\"",
       ",",
       ToBoxes[range]
      }],
     "]"
    }],
   DataTable,
   Editable -> False]
];

DataRepresentationQ[DataTable[l_, attrs___]] = True;


(**********************************************************)
(* CoordinateRanges                                       *)
(**********************************************************)

CoordinateRanges[d_DataTable] :=
  Module[{list = ToList[d], t1, t2},
    t1 = First[list][[1]];
    t2 = Last[list][[1]];
    {{t1,t2}}];


(**********************************************************)
(* CoordinateSpacings                                     *)
(**********************************************************)

CoordinateSpacings[d_DataTable] :=
 Module[{ts},
  ts = ToListOfCoordinates[d];

  (* TODO: use Differences here *)
  (* TODO: Check all spacings are even *)
  {Min[Drop[ts,1] - Drop[RotateRight[ts],1]]}
];


(**********************************************************)
(* Functions which should just see a regular data List.   *)
(* These fall into two categories:                        *)
(* 1. Those which take a single DataRegion and return a   *)
(*    DataRegion. We assume all functions which are       *)
(*    Listable and NumericFunction fall into this         *)
(*    category.                                           *)
(* 2. Those which take a single DataRegion and return     *)
(*    something else. We assume all functions which are   *)
(*    NumericFunction fall into this category.            *)
(**********************************************************)

$NonDataTableFunctions =
  {ArrayDepth, Dimensions, Total, Mean, Position, Extract};

$DataTableFunctions =
  {FilterNaNs};

(* TODO: maybe simplify this by splitting out into a version for OneIdentity functions and a version for others *)
DataTable /: f_Symbol[x___, d_DataTable, y___] /;
 MemberQ[$DataTableFunctions, f] ||
 (MemberQ[Attributes[f], NumericFunction] && MemberQ[Attributes[f], Listable]) :=
 Module[{args},
  (* TODO: Check DataTables are on the same grid *)
  args = {x, d, y} /. dt_DataTable :> ToListOfData[dt];
  ds = Cases[{x, d, y}, _DataTable, Infinity];
  attrs = Apply[Intersection, Map[ListAttributes, ds]];
  ToDataTable[Transpose[{ToListOfCoordinates[d], f@@args}], ListAttributes[d]]
];

DataTable /: f_Symbol[x___, d_DataTable, y___] /;
 MemberQ[$NonDataTableFunctions, f] || MemberQ[Attributes[f], NumericFunction] :=
 Module[{},
  f[x, ToListOfData[d], y]
];


(**********************************************************)
(* MaxCoordinates                                         *)
(**********************************************************)

MaxCoordinates[d_DataTable] :=
  Module[{list = ToList[d], t1},
    t1 = First[list][[1]];
    {t1}];


(**********************************************************)
(* MinCoordinates                                         *)
(**********************************************************)

MinCoordinates[d_DataTable] :=
  Module[{list = ToList[d], t2},
    t2 = Last[list][[1]];
    {t2}];


(****************************************************************)
(* NDerivative                                                  *)
(****************************************************************)

(* TODO: Extend NDerivative to work with arbitrary order derivatives and arbitrary order of accuracy, like in DataRegion.  Don't omit the endpoints *)

NDerivative[d_DataTable] :=
 Module[{diff, table1, table2, deriv},
  diff[{t1_, f1_}, {t2_, f2_}] :=
   {t1, (f2 - f1)/(t2 - t1)};
  table1 = Drop[ToList[d], 1];
  table2 = Drop[ToList[d], -1];
  deriv = MapThread[diff, {table2, table1}];
  Return[MakeDataTable[deriv]];
];


(**********************************************************)
(* Plotting functions                                     *)
(**********************************************************)

(* TODO: Provide undocumented workaround functions for the SaveDefinitions/Protected issue? *)

$1DPlotFunctions = {ListPlot, ListLinePlot, ListLogPlot, ListLogLogPlot};

DataTable /: f_Symbol[d_DataTable, args___] :=
 f[Evaluate[ToList[d]], args] /; MemberQ[$1DPlotFunctions, f];

(* We cannot use upvalues in the following as the DataTable appears too deep *)
(* TODO: Make this use plotWrapper for consistency *)
Unprotect /@ $1DPlotFunctions;
Scan[(#[ds:List[DataTable[___]..], opts___] := #[ToList /@ ds, opts])&, $1DPlotFunctions];
Protect /@ $1DPlotFunctions;


(**********************************************************)
(* Resampled                                              *)
(**********************************************************)
(* TODO: Implement Resampled[d:DataTable[__], template:DataTable[__], p_Integer:8] *)

Resampled[d_DataTable, dt_?NumberQ, p_Integer:8] :=
 Module[{t1, t2},
  {t1, t2} = DataTableRange[d];
  Resampled[d, {t1, t2, dt}, p]
];

Resampled[d_DataTable, {t1_?NumberQ, t2_?NumberQ, dt_?NumberQ}, p_Integer:8] :=
 Resampled[d, {{t1, t2, dt}}, p];

DataTable /: Resampled[d_DataTable, {t1_, t2_, dt_}, p_:8] :=
 Module[{f, dt1, dt2},
  {dt1,dt2} = Endpoints[d];
  If[t1 < dt1 || t2 > dt2 || t1 > t2 || dt < 0,
    Error["ResampleDataTable: bad range spec " <> ToString[{t1,t2,dt}] <>
          " for DataTable with range " <> ToString[{dt1,dt2}]]];
  f = Interpolation[d, InterpolationOrder -> p];
  (* TODO: possibly remove/replace the Attributes functions here *)
  AddAttributes[ToDataTable[Table[{t, f[t]}, {t, t1, t2, dt}]], ListAttributes[d]]
];

Resampled[ds:{DataTable[__]...}, p_:8] :=
  Module[{dts, dt, ranges, t1s, t2s, t1, t2},
    If[Length[ds] === 0, Return[{}]];
    dts = Map[First@CoordinateSpacings, ds];
    dt = Apply[Min, dts];
    ranges = Map[Endpoints, ds];
    t1s = Map[First, ranges];
    t2s = Map[Last, ranges];
    t1 = Apply[Max, t1s];
    t2 = Apply[Min, t2s];
    Map[Resampled[#, {t1, t2, dt}, p] &, ds];
];


(****************************************************************)
(* ToDataTable *)
(****************************************************************)

ToDataTable[l_List] :=
  DataTable[Developer`ToPackedArray[l]];

ToDataTable[l_List, attrRules:{(_ -> _) ...}] :=
  (* The attrRules are currently unsupported *)
  DataTable[Developer`ToPackedArray[l], Apply[Sequence,attrRules]];

ToDataTable[d_DataRegion`DataRegion] :=
 Module[{ndims, xmin, xmax, spacing, data},
  ndims = ArrayDepth[d];
  If[ ndims != 1,
	Error["ToDataTable: Number of dimensions " <> ToString[ndims] <> " in DataRegion '" 
          <> DataRegion`VariableName[d] <> "' is greater than 1."]
  ];

  {{xmin, xmax}} = CoordinateRanges[d];
  {spacing} = CoordinateSpacings[d];
  data = ToListOfData[d];
  ToDataTable[Transpose[{Range[xmin, xmax, spacing],data}]]
];


(****************************************************************)
(* ToList *)
(****************************************************************)

ToList[DataTable[l_, ___]] := l;


(****************************************************************)
(* ToListOfData *)
(****************************************************************)

ToListOfData[DataTable[l_, ___]] :=
  Map[#[[2]]&, l];


(****************************************************************)
(* ToListOfCoordinates *)
(****************************************************************)

ToListOfCoordinates[DataTable[l_, ___]] :=
  l[[All,1]];



(****************************************************************)
(****************************************************************)
(* DataTable specific                                           *)
(****************************************************************)
(****************************************************************)

(****************************************************************)
(* FourierDCT                                                   *)
(****************************************************************)

DataTable /: FourierDCT[d_DataTable,args___] :=
 Module[{df, fs},
  (* I think these frequencies are right *)
  df = 2 Pi/-Subtract @@ DataTableRange[d];
  fs = Table[i df, {i, 0, Length[d] - 1}];
  MakeDataTable@MapThread[List, {fs, Abs@FourierDCT[DepVar@d,args]}]/
   N[Sqrt[Length[d]]]
];

(****************************************************************)
(* Fourier                                                      *)
(****************************************************************)

DataTable /: Fourier[d_DataTable,args___] :=
 Module[{amp, l, n, T, freq},
  amp = Fourier[DepVar[d],args];
  l = Length[d];
  n = Floor[l/2];
  amp = RotateRight[amp, n];
  T = Spacing[d]*l;
  freq = Range[-n, l - 1 - n]/T;
  Return[MakeDataTable[Transpose[{freq, amp}]]];
];

(****************************************************************)
(* InverseFourier                                               *)
(****************************************************************)

DataTable /: InverseFourier[d_DataTable, t0_:0.0, opts:OptionsPattern[]] :=
 Module[{amp, time, df, l, n},
  l = Length[d];
  n = Floor[l/2];
  amp = InverseFourier[RotateLeft[DepVar[d], n],opts];
  df = Spacing[d];
  time = t0 + Range[0, l - 1]/(df*l);
  Return[MakeDataTable[Transpose[{time, amp}]]];
];

(****************************************************************)
(* Frequency                                                    *)
(****************************************************************)

SyntaxInformation[Frequency] =
 {"ArgumentsPattern" -> {_}};

(* TODO: Since this uses NDerivative, we should be able to specify the method and order of accuracy *)
Frequency[d:DataTable[__]] :=
  NDerivative[Phase[d]];


(****************************************************************)
(* AntiDerivative                                               *)
(****************************************************************)

SyntaxInformation[AntiDerivative] =
 {"ArgumentsPattern" -> {_, {_, _}, OptionsPattern[]}};

Options[AntiDerivative] = {InterpolationOrder->3};

AntiDerivative[d_DataTable, {tbc_, fbc_}, opts:OptionsPattern[]] :=
 Module[{tMin, tMax, dFn, gFn, g, t, dt, gTb},
  {tMin, tMax} = Endpoints[d];
  If[tbc < tMin || tbc > tMax,
   Error["AntiDerivative: boundary condition is not within range of DataTable"]];
  dt = First[CoordinateSpacings[d]];
	dFn = Interpolation[d, InterpolationOrder -> OptionValue[InterpolationOrder]];
  gFn = g /.
    NDSolve[{D[g[t], t] == dFn[t], g[tbc] == fbc}, {g}, {t, tMin, tMax}, MaxSteps -> 1000000][[
     1]];
  gTb = ToDataTable[Table[{t, gFn[t]}, {t, tMin, tMax, dt}], ListAttributes[d]]];


(****************************************************************)
(* CoordinateAtInterpolatedMax                                  *)
(****************************************************************)

SyntaxInformation[CoordinateAtInterpolatedMax] =
 {"ArgumentsPattern" -> {_}};

CoordinateAtInterpolatedMax[d_DataTable] :=
 Module[{tMax, fMax, l, maxFind, fn, t1, t2, t, tMax2},
  l = ToList[d];
  {t1, t2} = DataTableRange[d];
  fMax = -Infinity;
  maxFind[{time_, f_}] :=
   If[f > fMax, fMax = f; tMax = time];
  Scan[maxFind, l];
  fn = Interpolation[d];
  tMax2 = 
   t /. FindMaximum[{fn[t], {t > tMax - 50, t < tMax + 50}}, {t, 
       tMax}][[2]];
  Return[tMax2]];


(****************************************************************)
(* InterpolatedMax                                              *)
(****************************************************************)

SyntaxInformation[InterpolatedMax] =
 {"ArgumentsPattern" -> {_}};

InterpolatedMax[d_DataTable] :=
  Interpolation[d][LocateMaximum[d]];


(****************************************************************)
(* InterpolatedWhere                                            *)
(****************************************************************)

SyntaxInformation[InterpolatedWhere] =
 {"ArgumentsPattern" -> {_}};

InterpolatedWhere[d_DataTable, f_] :=
 Module[{dInterpolater},
  dInterpolater=Interpolation@MakeDataTable@DeleteCases[ToList[d],_?f];
  MakeDataTable[ToList[d]/. {t_,x_}:>{t,dInterpolater[t]}/;f[{t,x}]]
];


(**********************************************************)
(* MonotonicQ                                             *)
(**********************************************************)

SyntaxInformation[MonotonicQ] =
 {"ArgumentsPattern" -> {_}};

MonotonicQ[d_DataTable, tol_:0.] :=
  Module[{positive = (# > tol &)},
  Apply[And, positive /@ Drop[Drop[RotateLeft[IndVar[d]] - IndVar[d],1],-1]]];


(****************************************************************)
(* Phase                                                        *)
(****************************************************************)

SyntaxInformation[Phase] =
 {"ArgumentsPattern" -> {_}};

(* TODO: check the continuity algorithm and see if it can be improved *)

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


(**********************************************************)
(* UniformSpacingQ                                        *)
(**********************************************************)

SyntaxInformation[UniformSpacingQ] =
 {"ArgumentsPattern" -> {_}};

UniformSpacingQ[d_DataTable] :=
  Module[
    {ts, dts, dt1, tol = 10.^-5},
    ts = IndVar[d];
    dts = Drop[ts,1] - Drop[RotateRight[ts],1];
    dt1 = dts[[1]];
    Max[Abs[dts - dt1]] < tol];








(****************************************************************)
(****************************************************************)
(* Experimental                                                 *)
(****************************************************************)
(****************************************************************)

(****************************************************************)
(* FilterDCT                                                    *)
(****************************************************************)

zeroAfter[l_, n_] :=
 Module[{len},
  len = Length[l];
  Join[Take[l, n], Table[0, {i, n + 1, len}]]];

tableRange[t_List, tStart_?NumberQ, tEnd_?NumberQ] :=
  Select[t,
   (#[[1]] >= tStart && #[[1]] < tEnd) &];

tableRange[t_List, range_List] :=
  tableRange[t,range[[1]],range[[2]]];

FilterDCT[f_List, nModes_Integer] :=
 Module[{times, data, dataDCT, dataFilDCT, dataFil, fFil},
  times = Map[First, f];
  data = Map[Last, f];
  dataDCT = FourierDCT[data, 2];
  dataFilDCT = zeroAfter[dataDCT, nModes];
  dataFil = FourierDCT[dataFilDCT, 3];
  fFil = MapThread[List, {times, dataFil}];
  Return[fFil];
  ];

FilterDCT[f_DataTable,  nModes_Integer,
   range1 : {tMin1_?NumberQ, tMax1_?NumberQ},
   range2 : {tMin2_?NumberQ, tMax2_?NumberQ}] :=
  Module[{},
    MakeDataTable[FilterDCT[ToList[f], nModes, range1, range2]]
  ];

FilterDCT[f_List, nModes_Integer,
   range1 : {tMin1_?NumberQ, tMax1_?NumberQ},
   range2 : {tMin2_?NumberQ, tMax2_?NumberQ}] :=
  Module[{filtered, t1, t2, t3},
   filtered =
    tableRange[FilterDCT[tableRange[f, range1], nModes], range2];
   {t1, t2, t3} = PartitionTable[f, range2];
   Return[Join[t1, filtered, t3]]];


(****************************************************************)
(* PartitionTable                                               *)
(****************************************************************)

PartitionTable[t_List, {tMin_?NumberQ, tMax_?NumberQ}] :=
 Module[{before, middle, after},
  before = tableRange[t, First[t][[1]], tMin];
  middle = tableRange[t, tMin, tMax];
  after = tableRange[t, tMax, Last[t][[1]] + 1];
  Return[{before, middle, after}]
  ];


(****************************************************************)
(* AddAttribute                                                 *)
(****************************************************************)

AddAttribute[d:DataTable[x__], name_ -> val_] :=
  DataTable[x, name -> val];


(****************************************************************)
(* AddAttributes                                                *)
(****************************************************************)

AddAttributes[d:DataTable[x__], attrRules_List] :=
  DataTable[x, Apply[Sequence, attrRules]];


(****************************************************************)
(* ReadAttribute                                                *)
(****************************************************************)

ReadAttribute[d:DataTable[l_, attrs___], name_] :=
  Module[{val},
    val = name /. {attrs};
    If[val === name,
      Error["Attribute "<>ToString[name]<>" not found in "<>ToString[d]]];
    Return[val]];


(****************************************************************)
(* ListAttributes                                               *)
(****************************************************************)

ListAttributes[d:DataTable[l_, attrs___]] :=
  {attrs};





(****************************************************************)
(****************************************************************)
(* Functions which don't belong here or need to be updated      *)
(****************************************************************)
(****************************************************************)

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

(****************************************************************)
(* Map *)
(****************************************************************)

DataTable /: Map[f_, DataTable[l_, attrs___]] :=
  DataTable[Transpose[{l[[All,1]],Map[f,l[[All,2]]]}], attrs];

(****************************************************************)
(* MapList *)
(****************************************************************)

MapList[f_, DataTable[l_, attrs___]] :=
  DataTable[Map[f,l], attrs];

(****************************************************************)
(* MapThread *)
(****************************************************************)

(* The DataTable appears too deep here to associate this definition as
   an upvalue of DataTable *)
Unprotect[MapThread];
MapThread[f_, ds:List[DataTable[__]...]] :=
  Module[{lists, vals, xs, fOfVals, lengths, tb, attrs},
    lists = Map[ToList, ds];
    lengths = Map[Length, lists];

    If[!Apply[Equal,lengths],
      Error["MapThreadData: DataTables are not all of the same length"]];

    (* TODO: Check that the DataTables are "compatible"; i.e. their coordinates are the same to within a tolerance *)

    vals = Map[DepVar, ds];
    xs = IndVar[First[ds]];
    fOfVals = MapThread[f, vals];
    tb = MapThread[List, {xs,fOfVals}];
    attrs = Apply[Intersection, Map[ListAttributes, ds]];
    MakeDataTable[tb,attrs]];
Protect[MapThread];

RedefineAsDataTable[First[DataTable[l_,___]],
  l[[1]]];

RedefineAsDataTable[Last[DataTable[l_,___]],
  l[[-1]]];

RedefineAsDataTable[Dot[d1:DataTable[__], d2:DataTable[__]],
  MapThreadData[Dot, {d1, d2}]];

RedefineAsDataTable[Norm[d:DataTable[__]],
  MapData[Norm, d]];

RedefineAsDataTable[Length[DataTable[d_,___]],
  Length[d]];

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

RedefineAsDataTable[Length[d_DataTable],
	Length[ToList[d]]
];

(****************************************************************)
(* Downsampled *)
(****************************************************************)

Downsampled[d_DataTable, n_Integer] :=
  ApplyToList[downsample[#, n] &, d];


(*    MakeDataTable[Phase[ToList[d]]];*)

downsample[l_List, n_Integer] :=
  Take[l, {1, Length[l], n}];


Redefine[Interpolation[d:DataTable[___], args___],
   Interpolation[ToList[d], args]];

ShiftDataTable[dt_?NumberQ, d : DataTable[__]] :=
 AddAttributes[MakeDataTable[Map[{#[[1]] + dt, #[[2]]} &, ToList[d]]], ListAttributes[d]];


DataTableInterval[x___] := Error["DataTableInterval: Invalid arguments: "<>ToString[{x}]];

Options[DataTableInterval] = {Interval -> {Closed, Open}};
DataTableInterval[d_DataTable, {t1_, t2_}, opts:OptionsPattern[]] :=
  Module[
    {range, tMin, tMax, lower, upper, eps=10.^-6},
    range = DataTableRange[d];
    tMin = If[SameQ[t1, All], range[[1]], t1];
    tMax = If[SameQ[t2, All], range[[2]], t2];
    lower = Switch[OptionValue[Interval][[1]],
                   Closed, GreaterEqual,
                   Open, Greater,
                   _, Error["Unrecognised option Interval -> "
                            <>ToString[OptionValue[Interval], InputForm]]];
    upper = Switch[OptionValue[Interval][[2]],
                   Closed, LessEqual,
                   Open, Less,
                   _, Error["Unrecognised option Interval -> "
                            <>ToString[OptionValue[Interval], InputForm]]];

    d /. DataTable[l_, x___] :>
    DataTable[Select[l,lower[#[[1]],tMin-eps] && upper[#[[1]], tMax+eps] &], x]];

DataTableDepVarInterval[d_DataTable, {y1_, y2_}] :=
  d /. DataTable[data_, attrs___] :> DataTable[Select[data,#[[2]] >= y1 && #[[2]] < y2 &], attrs];

IntersectDataTables[d1_DataTable, d2_DataTable] :=
  Module[{d1Min, d1Max, d2Min, d2Max, dMin, dMax},
    {d1Min, d1Max} = DataTableRange[d1];
    {d2Min, d2Max} = DataTableRange[d2];

    dMin = Max[d1Min, d2Min];
    dMax = Min[d1Max, d2Max];

    Return[{DataTableInterval[d1,{dMin, dMax}, Interval -> {Closed,Closed}], 
            DataTableInterval[d2,{dMin, dMax}, Interval -> {Closed,Closed}]}]];

IntersectDataTables[{d1_DataTable, d2_DataTable}] :=
  Module[{d1Min, d1Max, d2Min, d2Max, dMin, dMax},
    {d1Min, d1Max} = DataTableRange[d1];
    {d2Min, d2Max} = DataTableRange[d2];

    dMin = Max[d1Min, d2Min];
    dMax = Min[d1Max, d2Max];

    Return[{DataTableInterval[d1,{dMin, dMax}, Interval -> {Closed,Closed}], 
            DataTableInterval[d2,{dMin, dMax}, Interval -> {Closed,Closed}]}]];

IntersectDataTables[ds:{(_DataTable)...}] :=
  Module[{ranges, mins, maxs, min, max, ds2},
    ranges = Map[DataTableRange, ds];
    mins = Map[First, ranges];
    maxs = Map[Last, ranges];

    (* TODO: add a check that the grids are compatible; and an option to disable the check? *)

    min = Max[mins];
    max = Min[maxs];

    ds2 = Map[DataTableInterval[#,{min, max}, Interval -> {Closed,Closed}] &, ds];
    ds2];


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

InvertDataTable[d_DataTable] :=
  MakeDataTable[MapThread[List,{DepVar[d],IndVar[d]}]];

LocateMaximumPoint[d_DataTable] :=
 Module[{tMax, fMax, l, maxFind, t1, t2},
  l = ToList[d];
  {t1, t2} = DataTableRange[d];
  fMax = -Infinity;
  maxFind[{t_, f_}] :=
   If[f > fMax, fMax = f; tMax = t];
  Scan[maxFind, l];
  Return[tMax]];



FunctionOfPhase[d_DataTable, p_DataTable, {t1_, t2_}, dp_: 0.01] :=
 Module[{phiOft, tOfphi, tOfphiFn, phiMin, phiMax, dOftFn, dOfphiTb},
  phiOft = ToList[DataTableInterval[p,{t1,t2}]];
  tOfphi = Map[Reverse, phiOft];
  tOfphiFn = Interpolation[tOfphi];
  {phiMin,phiMax} = Sort[{First[tOfphi][[1]], Last[tOfphi][[1]]}];
  dOftFn = Interpolation[d];
  dOfphiTb = 
   Table[{phi, dOftFn[tOfphiFn[phi]]}, {phi, phiMin, phiMax, dp}];
  AddAttributes[MakeDataTable[dOfphiTb], ListAttributes[d]]];



(****************************************************************)
(****************************************************************)
(* Deprecated                                                   *)
(****************************************************************)
(****************************************************************)

MakeDataTable[l_List] :=
  DataTable[Developer`ToPackedArray[l]];

MakeDataTable[l_List, attrRules:{(_ -> _) ...}] :=
  DataTable[Developer`ToPackedArray[l], Apply[Sequence,attrRules]];

(* This is never used, and should be removed *)
MakeDataTable[f_InterpolatingFunction, dt_] :=
  Module[{tMin,tMax},
    tMin = f[[1]][[1]][[1]];
    tMax = f[[1]][[1]][[2]];
    MakeDataTable[Table[{t,f[t]},{t,tMin,tMax,dt}]]];                  

(* This should be deprecated *)
MakeDataTable[xs_List, ys_List] :=
  MakeDataTable[MapThread[List, {xs,ys}]];

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

MapThreadData[f_, ds:List[DataTable[__]..]] :=
  Module[{lists, vals, xs, fOfVals, lengths, tb, attrs},
    lists = Map[ToList, ds];
    lengths = Map[Length, lists];

    If[!Apply[Equal,lengths],
      Error["MapThreadData: DataTables are not all of the same length"]];

    vals = Map[DepVar, ds];
    xs = IndVar[First[ds]];
    fOfVals = MapThread[f, vals];
    tb = MapThread[List, {xs,fOfVals}];
    attrs = Apply[Intersection, Map[ListAttributes, ds]];
    MakeDataTable[tb,attrs]];

Downsample[d_DataTable, n_Integer] :=
  ApplyToList[downsample[#, n] &, d];

Spacing[d:DataTable[__]] := First[CoordinateSpacings[d]];

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
      Error["ResampleDataTable: bad range spec " <> ToString[{t1,t2,dt}] <>
            " for DataTable with range " <> ToString[{dt1,dt2}]]];
    f = Interpolation[d, InterpolationOrder -> p];
    AddAttributes[MakeDataTable[Table[{t, f[t]}, {t, t1, t2, dt}]], ListAttributes[d]]];

ResampleDataTable[d:DataTable[__], template:DataTable[__], p_Integer:8] :=
  Module[
    {d2, template2},
    {d2, template2} = IntersectDataTables[d,template];
    f = Interpolation[d, InterpolationOrder -> p];
    AddAttributes[MakeDataTable[Table[{t, f[t]}, {t, IndVar[template2]}]],
                  ListAttributes[d]]];

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

MakeInterpolatingDataTable[d:DataTable[__], dt_] :=
  Module[{l, t1, t2, f, l2},
    l = ToList[d];
    t1 = First[l][[1]];
    t2 = Last[l][[1]];
    f = Interpolation[l];
    l2 = Table[{t, f[t]}, {t, t1, t2, dt}];
    d /. DataTable[_, x___] -> DataTable[l2, x]];

ShiftPhase[d_DataTable, dph_] :=
  MapData[Exp[I dph] # &, d];

AbsOfPhase[d_DataTable, {t1_, t2_}] :=
  FunctionOfPhase[Abs[d], Phase[d], {t1,t2}];

DataTableListLinePlot[d:DataTable[___], args___] :=
  ListLinePlot[ToList[d], args];

PhaseOfFrequency[psi4_] :=
 Module[{phase1, freq1, phaseOfFreq1},
  phase1 = -Phase[psi4];
  freq1 = NDerivative[phase1];
  phaseOfFreq1 = 
   MakeDataTable@
    MapThread[List, 
     Map[DepVar, IntersectDataTables[{freq1, phase1}]]]
  ];

IntegrateDataTableZeroStart[d_DataTable] := 
  IntegrateDataTable[d, {DataTableRange[d][[1]], 0}];

IntegrateDataTableZeroEnd[d_DataTable] := 
  IntegrateDataTable[d, {DataTableRange[d][[2]], 0}];

MakeUniform[d_DataTable] :=
  ResampleDataTable[d, Spacing[d], 8];

UniformGridQ = UniformSpacingQ;
InterpolateWhereFunction = InterpolatedWhere;
LocateMaximum = CoordinateAtInterpolatedMax;
MaximumValue = InterpolatedMax;
DataTableRange = Endpoints;
DataTableNormL2 = GridNorm;
IntegrateDataTable = AntiDerivative;

End[];

EndPackage[];
