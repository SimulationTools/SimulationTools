(* Copyright 2010-2012 Ian Hinder, Barry Wardell and Aaryn Tonita

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

BeginPackage["SimulationTools`DataTable`",
 {
  "SimulationTools`DataRepresentations`",
  "SimulationTools`Error`",
  "SimulationTools`Profile`",
  "SimulationTools`"
 }];

DataTable::usage = "DataTable[{{x1,f1},{x2,f2},...,{xn,fn}}] is a one-dimensional table of data (fi) with an associated coordinate (xi).  DataTable objects print as DataTable[...] to avoid printing the potentially large data content.  The independent variables, xi, should be monotonically increasing real numbers and may have a variable increment.  The dependent variables, fi, can be of any type for which the basic mathematical operations (+, -, *, /) make sense.";
ToDataTable::usage = "ToDataTable[{{x1,f1},{x2,f2},...,{xn,fn}}] constructs a DataTable object out of the list passed. The independent variables, xi, should be monotonically increasing real numbers and may have a variable increment.  The dependent variables, fi, can be of any type for which the basic mathematical operations (+, -, *, /) make sense.
ToDataTable[dr] converts a 1-dimensional DataRegion into a DataTable.";

MinCoordinateSpacing::usage = "MinCoordinateSpacing[d] gives the smallest spacing of the coordinates in d.";
MaxCoordinateSpacing::usage = "MaxCoordinateSpacing[d] gives the largest spacing of the coordinates in d.";
UniformSpacingQ::usage = "UniformSpacingQ[d] returns True if the DataTable has a uniform grid spacing and False if the grid spacing is variable.  The grid spacings are considered uniform if they are equal up to a tolerance of 1e-5.";

CoordinateAtInterpolatedMax::usage = "CoordinateAtInterpolatedMax[d] finds the time at which a maximum occurs in the range of the DataTable d. This time is interpolated and may not coincide with a data point in the DataTable.";
InterpolatedMax::usage = "InterpolatedMax[d] returns the maximum value of the interpolant of a DataTable d";

InterpolatedWhere::usage = "InterpolatedWhere[d, f] returns a new DataTable where the elements of d where the function returns true have been replaced by interpolated values."
AntiDerivative::usage = "AntiDerivative[d, {x, f}] returns the first integral, I, of the DataTable d, with the integration constant chosen such that I[x] = f.";

RestrictedToCommonInterval::usage = "RestrictedToCommonInterval[{d1, d2, ...}] returns copies of the supplied set of DataTables but restricted to having their independent variables within the same range, which is the intersection of the ranges of the inputs.";

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

(* TODO: Move to another package. This only works for uniform DataTables.  Should we have a separate Filtering package? Should it be Filtered? *)
FilterDCT;

(* TODO: Decide if non-monotonic DataTables are allowed/checked *)
(* TODO: Implement a Monotonic[d] to make a non-monotonic DataTable monotonic?  Or maybe this happens as an option to ToDataTable? *)
MonotonicQ;
FunctionInverse;
Composition;

RestrictedToInterval;

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
ShiftDataTable;
IntersectDataTables;
DataTableInterval;
DataTableDepVarInterval;
InvertDataTable;
FunctionOfPhase;
LocateMaximumPoint;

(* Exceptions *)

CoordinateSpacingNonUniform;
DataTableSingle;

Begin["`Private`"];

(****************************************************************)
(* DataTable *)
(****************************************************************)

DocumentationBuilder`SymbolDescription["DataTable"] = "representation of 1D data";

check[d_DataTable, where_:None] :=
  If[!validQ[d],
    Error[If[where===None,"",where<>": "]<>"Invalid DataTable"]];

validQ[d_DataTable] :=
  MatchQ[d,DataTable[{{_?NumericQ, _?NumericQ|_List}...},___]];

(* TODO: Remove this hack *)
DataTable /: Dimensions[d_DataTable] := {Length[ToListOfData[d]]};

SetAttributes[DataTable, {ReadProtected}];

(* DataTables are formatted like SparseArrays *)
Format[d:DataTable[l_ /; (Head[l]=!=SequenceForm), ___]] :=
  If[!validQ[d], 
   DataTable[SequenceForm@@{"<", "invalid", ">"}],
   DataTable[SequenceForm@@{"<", Length[l], ">"}, {{l[[1, 1]], l[[-1, 1]]}}]];

DataRepresentationQ[DataTable[l_, attrs___]] = True;


(**********************************************************)
(* CoordinateRanges                                       *)
(**********************************************************)

CoordinateRanges[d_DataTable] :=
  Module[{list = ToList[d], t1, t2},
    If[Length[list] === 0, Error["Cannot compute the range of an empty DataTable"]];
    t1 = First[list][[1]];
    t2 = Last[list][[1]];
    {{t1,t2}}];


(**********************************************************)
(* CoordinateSpacing                                     *)
(**********************************************************)

CoordinateSpacing[d_DataTable] := First[CoordinateSpacings[d]];


(**********************************************************)
(* CoordinateSpacings                                     *)
(**********************************************************)

CoordinateSpacings[d_DataTable] :=
 Module[{ts},
  If[!UniformSpacingQ[d],
  	Error[CoordinateSpacingNonUniform,
              "CoordinateSpacing undefined for non-uniform DataTables."];
  ];

  ts = ToListOfCoordinates[d];

  {Min[Differences[ts]]}
];


(**********************************************************)
(* MaxCoordinateSpacing                                   *)
(**********************************************************)

SyntaxInformation[MaxCoordinateSpacing] =
 {"ArgumentsPattern" -> {_}};

MaxCoordinateSpacing[d_DataTable] :=
 Module[{ts},
  ts = ToListOfCoordinates[d];

  Max[Differences[ts]]
];


(**********************************************************)
(* MinCoordinateSpacing                                   *)
(**********************************************************)

SyntaxInformation[MinCoordinateSpacing] =
 {"ArgumentsPattern" -> {_}};

MinCoordinateSpacing[d_DataTable] :=
 Module[{ts},
  ts = ToListOfCoordinates[d];

  Min[Differences[ts]]
];


(**********************************************************)
(* Downsampled                                            *)
(**********************************************************)

downsample[l_List, n_Integer] :=
  Take[l, {1, Length[l], n}];

Downsampled[d_DataTable, n_Integer] :=
  ApplyToList[downsample[#, n] &, d];


(****************************************************************)
(* Drop                                                         *)
(****************************************************************)

DataTable /: Drop[d:DataTable[l_, x___], args__] :=
 Module[{data},
  data = Drop[l, args];

  If[Length[data] === 1,
  	Error[DataTableSingle,
              "Operations which would return a DataTable with a single element are not currently supported."];
  ];
  DataTable[data, x]
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

DataTable /: f_Symbol[x___, d_DataTable, y___] /;
 MemberQ[$DataTableFunctions, f] ||
 (MemberQ[Attributes[f], NumericFunction] && MemberQ[Attributes[f], Listable]) :=
 Module[{args, ds, rds, attrs},
  ds = Cases[{x, d, y}, _DataTable, Infinity];
  Assert[Apply[And,validQ /@ ds]];
  If[Length[ds] > 1 && !(SameGridQ@@ds),
    rds = Resampled[ds];
    If[rds === $Failed,
      Error[ToString[f]<>" cannot operate on DataTables with different coordinates. Too allow this, set the $ResamplingMethod variable."];
    ];
    args = ({x, d, y} /. Thread[ds -> rds]) /. dt_DataTable :> ToListOfData[dt];
  ,
    rds = ds;
    args = {x, d, y} /. dt_DataTable :> ToListOfData[dt];
  ];
  attrs = Apply[Intersection, Map[ListAttributes, ds]];
  ToDataTable[Transpose[{ToListOfCoordinates[First[rds]], f@@args}], ListAttributes[d]]
];

DataTable /: f_Symbol[x___, d_DataTable, y___] /;
 MemberQ[$NonDataTableFunctions, f] || MemberQ[Attributes[f], NumericFunction] :=
 Module[{},
  f[x, ToListOfData[d], y]
];


(**********************************************************)
(* Interpolation                                          *)
(**********************************************************)

DataTable /: Interpolation[d_DataTable, args___] :=
   Interpolation[ToList[d], args];

DataTable /: Interpolation[d_DataTable, args___] /; !MonotonicQ[d]:=
   Error["Can not interpolate a non-monotonic DataTable."];

(****************************************************************)
(* Map *)
(****************************************************************)

DataTable /: Map[f_, DataTable[l_, attrs___]] :=
  DataTable[Transpose[{l[[All,1]],Map[f,l[[All,2]]]}], attrs];


(****************************************************************)
(* MapList *)
(****************************************************************)

MapList[f_, dt_DataTable] :=
  Module[{fdt, t},
    fdt = Map[f, ToList[dt]];
    t = ToListOfCoordinates[dt];
    If[!MatchQ[fdt[[1]], _?NumericQ],
      Error["MapList: Result is not numeric: "<>ToString[fdt[[1]]]]];
    ToDataTable[t, fdt]
];

(****************************************************************)
(* MapThread *)
(****************************************************************)

(* The DataTable appears too deep here to associate this definition as
   an upvalue of DataTable *)
Unprotect[MapThread];

MapThread[f_, ds:List[DataTable[__]...]] :=
 Module[{vals, xs, fOfVals, tb, attrs},
  If[Length[ds] > 1 && !(SameGridQ@@ds),
    Error["MapThread cannot operate on DataTables with different coordinates."];
  ];

  vals = Map[DepVar, ds];
  xs = IndVar[First[ds]];
  fOfVals = MapThread[f, vals];
  tb = MapThread[List, {xs,fOfVals}];
  attrs = Apply[Intersection, Map[ListAttributes, ds]];
  MakeDataTable[tb,attrs]
];

Protect[MapThread];


(**********************************************************)
(* MaxCoordinates                                         *)
(**********************************************************)

MaxCoordinates[d_DataTable] :=
  Module[{list = ToList[d], t2},
    t2 = Last[list][[1]];
    {t2}];


(**********************************************************)
(* MinCoordinates                                         *)
(**********************************************************)

MinCoordinates[d_DataTable] :=
  Module[{list = ToList[d], t1},
    t1 = First[list][[1]];
    {t1}];


(****************************************************************)
(* NDerivative                                                  *)
(****************************************************************)

NDerivative[derivs__][d_DataTable, opts___] :=
 Module[{grid, data, deriv},

  (* Get the grid in the form {{x1, ..., xn}, {y1, ..., yn}, ...} *)
  grid = ToListOfCoordinates[d];
  data = ToListOfData[d];

  deriv = NDSolve`FiniteDifferenceDerivative[Derivative[derivs], grid, data, opts];

  ToDataTable[grid, deriv]
];

(* TODO: This form is deprecated *)
NDerivative[d_DataTable] :=
 Module[{diff, table1, table2, deriv},
  diff[{t1_, f1_}, {t2_, f2_}] :=
   {t1, (f2 - f1)/(t2 - t1)};
  table1 = Drop[ToList[d], 1];
  table2 = Drop[ToList[d], -1];
  deriv = MapThread[diff, {table2, table1}];
  Return[MakeDataTable[deriv]];
];


(****************************************************************)
(* Part                                                         *)
(****************************************************************)

DataTable /: Part[d:DataTable[l_, x___], args__] :=
 Module[{data, result},
  data = Part[l, args];

  Which[
   ArrayDepth[data] === 1,
     result = data[[2]];,
   Length[data] === 1,
     Error[DataTableSingle,
           "Operations which would return a DataTable with a single element are not currently supported."];,
   True,
     result = DataTable[data, x];
  ];

  result
];

(**********************************************************)
(* Phase                                                  *)
(**********************************************************)

(* TODO: clean up Phase implementation *)
Phase[d_DataTable] :=
  ToDataTable[SimulationTools`DataRepresentations`Private`phase[ToList[d]]];


(**********************************************************)
(* Pick                                                   *)
(**********************************************************)

DataTable/: Pick[dr_DataTable, elements_] := ToDataTable[Pick[ToList[dr], elements]];


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
(* SameGridQ                                              *)
(**********************************************************)

DataTable /: SameGridQ[dts:DataTable[__]..] :=
 Module[{coords},
  If[Length[{dts}] < 2,
    Error["SameGridQ expects at least two arguments."];
  ];

  coords = ToListOfCoordinates /@ {dts};
  SameQ @@ coords
];


(****************************************************************)
(* Shifted                                                      *)
(****************************************************************)

Shifted[d_DataTable, dt_?NumericQ] :=
  Module[{list = ToList[d]},
    AddAttributes[MakeDataTable[Thread[{list[[All,1]] + dt, list[[All,2]]}]],
                  ListAttributes[d]]];

Shifted[d_DataTable, {dt_?NumericQ}] := Shifted[d, dt];


(****************************************************************)
(* Take                                                         *)
(****************************************************************)

DataTable /: Take[d:DataTable[l_, x___], args__] :=
 Module[{data},
  data = Take[l, args];

  If[Length[data] === 1,
  	Error[DataTableSingle,
              "Operations which would return a DataTable with a single element are not currently supported."];
  ];
  DataTable[data, x]
];


(****************************************************************)
(* ToDataTable *)
(****************************************************************)

ToDataTable[l_List] :=
  DataTable[Developer`ToPackedArray[l]];
ToDataTable[t_List, f_List] :=
 Module[{},
  If[Length[t] =!= Length[f],
    Error["ToDataTable: Dependent and independent variables must be Lists of the same length."];
  ];
  DataTable[Developer`ToPackedArray[Transpose[{t, f}]]]
];

ToDataTable[l_List, attrRules:{(_ -> _) ...}] :=
  (* The attrRules are currently unsupported *)
  DataTable[Developer`ToPackedArray[l], Apply[Sequence,attrRules]];

ToDataTable[d_SimulationTools`DataRegion`DataRegion] :=
 Module[{ndims, xmin, xmax, spacing, data},
  ndims = ArrayDepth[d];
  If[ ndims != 1,
	Error["ToDataTable: Number of dimensions " <> ToString[ndims] <> " in DataRegion '" 
          <> SimulationTools`DataRegion`VariableName[d] <> "' is greater than 1."]
  ];

  {{xmin, xmax}} = CoordinateRanges[d];
  {spacing} = CoordinateSpacings[d];
  data = ToListOfData[d];
  ToDataTable[Range[xmin, xmax, spacing], data]
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
(* AntiDerivative                                               *)
(****************************************************************)

SyntaxInformation[AntiDerivative] =
 {"ArgumentsPattern" -> {_, {_, _}, OptionsPattern[]}};

Options[AntiDerivative] = {InterpolationOrder->3};

DocumentationBuilder`OptionDescriptions["AntiDerivative"] = {
  InterpolationOrder -> "The order of interpolation to use. This may be take value "<>
    "supported by the Interpolation function."
};

AntiDerivative[d_DataTable, {tbc_, fbc_}, opts:OptionsPattern[]] :=
 Module[{tMin, tMax, dFn, gFn, g, t, dt, gTb},
  {tMin, tMax} = CoordinateRange[d];
  If[tbc < tMin || tbc > tMax,
   Error["AntiDerivative: boundary condition is not within range of DataTable"]];
  dt = First[CoordinateSpacings[d]];
	dFn = Interpolation[d, InterpolationOrder -> OptionValue[InterpolationOrder]];
  gFn = g /.
    NDSolve[{D[g[t], t] == dFn[t], g[tbc] == fbc}, {g}, {t, tMin, tMax}, MaxSteps -> 1000000][[
     1]];
  gTb = ToDataTable[Table[{t, gFn[t]}, {t, tMin, tMax, dt}], ListAttributes[d]]];


(**********************************************************)
(* Coordinate                                             *)
(**********************************************************)

Coordinate[d_DataTable, dir_:Automatic] :=
 Module[{coord},
  If[(dir =!= Automatic) && (dir =!= 1),
    Error["Dimension "<>ToString[dir]<>" is not valid for DataTables"];
  ];

  coord = ToListOfCoordinates[d];
  ToDataTable[Transpose[{coord, coord}]]
];


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
 {"ArgumentsPattern" -> {_, _}};

InterpolatedWhere[d_DataTable, f_] :=
 Module[{dInterpolater},
  dInterpolater=Interpolation @ ToDataTable @ DeleteCases[ToList[d],_?f];
  ToDataTable[ToList[d] /. {t_,x_} :> {t,dInterpolater[t]} /; f[{t,x}]]
];


(**********************************************************)
(* Length                                                 *)
(**********************************************************)

DataTable /: Length[DataTable[d_,___]] :=
  Length[d];


(****************************************************************)
(* PadLeft                                                      *)
(****************************************************************)

DataTable /: PadLeft[d_DataTable, n_, x___] :=
 Module[{spacing, coords},
  If[Length[{x}] > 1,
    Error["Arguments "<>ToString[{x}]<>" not supported by PadLeft"];
  ];
  spacing = CoordinateSpacing[d];
  coords  = Range[n] spacing + First[MinCoordinates[d]] - spacing (n-Length[d]+1);
  ToDataTable[coords, PadLeft[ToListOfData[d], n, x]]
];

(****************************************************************)
(* PadRight                                                      *)
(****************************************************************)

DataTable /: PadRight[d_DataTable, n_, x___] :=
 Module[{spacing, coords},
  If[Length[{x}] > 1,
    Error["Arguments "<>ToString[{x}]<>" not supported by PadRight"];
  ];
  spacing = CoordinateSpacing[d];
  coords  = (Range[n]-1) spacing + First[MinCoordinates[d]];
  ToDataTable[coords, PadRight[ToListOfData[d], n, x]]
];

(**********************************************************)
(* RestrictedToCommonInterval                             *)
(**********************************************************)

SyntaxInformation[RestrictedToCommonInterval] =
 {"ArgumentsPattern" -> {__}};

RestrictedToCommonInterval[d1_DataTable, d2_DataTable] :=
  Module[{d1Min, d1Max, d2Min, d2Max, dMin, dMax},
    {d1Min, d1Max} = DataTableRange[d1];
    {d2Min, d2Max} = DataTableRange[d2];

    dMin = Max[d1Min, d2Min];
    dMax = Min[d1Max, d2Max];

    Return[{RestrictedToInterval[d1,{dMin, dMax}, Interval -> {Closed,Closed}],
            RestrictedToInterval[d2,{dMin, dMax}, Interval -> {Closed,Closed}]}]];

RestrictedToCommonInterval[{d1_DataTable, d2_DataTable}] :=
  Module[{d1Min, d1Max, d2Min, d2Max, dMin, dMax},
    {d1Min, d1Max} = DataTableRange[d1];
    {d2Min, d2Max} = DataTableRange[d2];

    dMin = Max[d1Min, d2Min];
    dMax = Min[d1Max, d2Max];

    Return[{RestrictedToInterval[d1,{dMin, dMax}, Interval -> {Closed,Closed}],
            RestrictedToInterval[d2,{dMin, dMax}, Interval -> {Closed,Closed}]}]];

RestrictedToCommonInterval[ds:{(_DataTable)...}] :=
  Module[{ranges, mins, maxs, min, max, ds2},
    ranges = Map[DataTableRange, ds];
    mins = Map[First, ranges];
    maxs = Map[Last, ranges];

    min = Max[mins];
    max = Min[maxs];

    ds2 = Map[RestrictedToInterval[#,{min, max}, Interval -> {Closed,Closed}] &, ds];
    ds2];


(**********************************************************)
(* CommonInterval                                         *)
(**********************************************************)

CommonInterval[ds:{(_DataTable)...}] :=
  Module[{ranges, mins, maxs, min, max, ds2},
    ranges = Map[CoordinateRange, ds];
    mins = Map[First, ranges];
    maxs = Map[Last, ranges];

    min = Max[mins];
    max = Min[maxs];
    {{min, max}}];

(**********************************************************)
(* RestrictedToInterval                                   *)
(**********************************************************)

SyntaxInformation[RestrictedToInterval] =
 {"ArgumentsPattern" -> {_, {_, _}, OptionsPattern[]}};

Options[RestrictedToInterval] = {Interval -> {Closed, Closed}};

DocumentationBuilder`OptionDescriptions["RestrictedToInterval"] = {
  Interval -> "Controls whether the specified interval endpoints should be "<>
    "interpreted as Open or Closed."
};

RestrictedToInterval[d_DataTable, {t1_, t2_}, opts:OptionsPattern[]] :=
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

partitionTable[t_List, {tMin_?NumberQ, tMax_?NumberQ}] :=
 Module[{before, middle, after},
  before = tableRange[t, First[t][[1]], tMin];
  middle = tableRange[t, tMin, tMax];
  after = tableRange[t, tMax, Last[t][[1]] + 1];
  Return[{before, middle, after}]
  ];

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
   {t1, t2, t3} = partitionTable[f, range2];
   Return[Join[t1, filtered, t3]]];


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
(* FunctionInverse                                              *)
(****************************************************************)

FunctionInverse[d_DataTable] :=
  MakeDataTable[MapThread[List,{DepVar[d],IndVar[d]}]];


(****************************************************************)
(* Composition                                                  *)
(****************************************************************)

DataTable /: Composition[d_DataTable, p_DataTable] :=
 Module[{dInterp, data},
  dInterp = Interpolation[d];
  coords = ToListOfCoordinates[p];
  data = dInterp[ToListOfData[p]];
  AddAttributes[ToDataTable[coords, data], ListAttributes[d]]
];


(**********************************************************)
(* MonotonicQ                                             *)
(**********************************************************)

SyntaxInformation[MonotonicQ] =
 {"ArgumentsPattern" -> {_}};

MonotonicQ[d_DataTable, tol_] :=
 Module[{positive},
  If[tol == 0,
    MonotonicQ[d]
  ];
  positive = (# > tol &);
  Apply[And, positive /@ Drop[Drop[RotateLeft[IndVar[d]] - IndVar[d],1],-1]]
];

MonotonicQ[d_DataTable] := Abs[Plus @@ Sign[Differences[ToListOfCoordinates[d]]]] == (Length[d]-1);

(****************************************************************)
(* Dot                                                          *)
(****************************************************************)

DataTable /: Dot[d1:DataTable[__], d2:DataTable[__]] :=
  MapThread[Dot, {d1, d2}];


(****************************************************************)
(* Export                                                       *)
(****************************************************************)

DataTable /: Export[file_String, dt_DataTable, type___] :=
  Export[file, Flatten/@ToList[dt], type];


(****************************************************************)
(* Join                                                         *)
(****************************************************************)

singleToList[d_DataTable] := MapData[If[SameQ[Head[#], List], #, {#}]&, d]

DataTable /: Join[ds:DataTable[__]...] := Module[{resampled, joineddata},
  resampled = ResampleDataTables[{ds}];
  joineddata=MapThread[Join, DepVar/@singleToList/@resampled];
  MakeDataTable[Thread[{IndVar[resampled[[1]]] ,joineddata}]]
];


(****************************************************************)
(* Norm                                                         *)
(****************************************************************)

DataTable /: Norm[d:DataTable[__]] :=
  Map[Norm, d];







(****************************************************************)
(****************************************************************)
(* Deprecated                                                   *)
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

Spacing[d:DataTable[__]] := MinCoordinateSpacing[d];

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

Options[ResampleDataTable] = {"Intersect" -> True};
ResampleDataTable[d:DataTable[__], template:DataTable[__], p_Integer:8, OptionsPattern[]] :=
  Module[
    {d2, template2},
    (* This might be leading to unexplained differences *)
    (* If[DepVar[d] === DepVar[template], *)
    (*    Return[d]]; *)
    {d2, template2} = If[OptionValue[Intersect],
                         IntersectDataTables[d,template],
                         {d,template}];
    f = Interpolation[d, InterpolationOrder -> p];
    AddAttributes[MakeDataTable[Table[{t, f[t]}, {t, IndVar[template2]}]],
                  ListAttributes[d]]];

ResampleDataTables[ds:{DataTable[__]...}, p : _Integer : 8] :=
  Module[{dts, dt, ranges, t1s, t2s, t1, t2},
    Assert[Apply[And,validQ/@ds]];
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

ShiftDataTable[dt_?NumberQ, d : DataTable[__]] :=
 Shifted[d, dt];

DataTableDepVarInterval[d_DataTable, {y1_?NumberQ, y2_?NumberQ}] :=
  d /. DataTable[data_, attrs___] :> DataTable[Select[data,#[[2]] >= y1 && #[[2]] < y2 &], attrs];

(* TODO: Could implement this in terms of Composition[d, FunctionInverse[p]], but it would not be an identical algorithm *)
FunctionOfPhase[d_DataTable, p_DataTable, {t1_, t2_}, dp_: 0.01] :=
 Module[{phiOft, tOfphi, tOfphiFn, phiMin, phiMax, dOftFn, dOfphiTb},
  phiOft = ToList[RestrictedToInterval[p,{t1,t2}]];
  tOfphi = Map[Reverse, phiOft];
  tOfphiFn = Interpolation[tOfphi];
  {phiMin,phiMax} = Sort[{First[tOfphi][[1]], Last[tOfphi][[1]]}];
  dOftFn = Interpolation[d];
  dOfphiTb = 
   Table[{phi, dOftFn[tOfphiFn[phi]]}, {phi, phiMin, phiMax, dp}];
  AddAttributes[MakeDataTable[dOfphiTb], ListAttributes[d]]];

rangepatt = _?NumberQ | All;

Options[DataTableInterval] = {Interval -> {Closed, Open}};
DataTableInterval[d_DataTable, {t1:rangepatt, t2:rangepatt}, opts:OptionsPattern[]] :=
 RestrictedToInterval[d, {t1, t2}, Interval -> OptionValue[Interval]];

LocateMaximumPoint[d_DataTable] :=
 Module[{tMax, fMax, l, maxFind, t1, t2},
  l = ToList[d];
  {t1, t2} = DataTableRange[d];
  fMax = -Infinity;
  maxFind[{t_, f_}] :=
   If[f > fMax, fMax = f; tMax = t];
  Scan[maxFind, l];
  Return[tMax]];

resampled[ds:{DataTable[__]...}, p_:8] :=
  Module[{dts, dt, ranges, t1s, t2s, t1, t2},
    If[Length[ds] === 0, Return[{}]];
    dts = Map[MinCoordinateSpacing, ds];
    dt = Apply[Min, dts];
    ranges = Map[CoordinateRange, ds];
    t1s = Map[First, ranges];
    t2s = Map[Last, ranges];
    t1 = Apply[Max, t1s];
    t2 = Apply[Min, t2s];
    Map[Resampled[#, {{t1, t2, dt}}, InterpolationOrder -> p] &, ds]
];

UniformGridQ = UniformSpacingQ;
InterpolateWhereFunction = InterpolatedWhere;
LocateMaximum = CoordinateAtInterpolatedMax;
MaximumValue = InterpolatedMax;
DataTableRange = CoordinateRange;
DataTableNormL2 = GridNorm;
IntegrateDataTable = AntiDerivative;
IntersectDataTables = RestrictedToCommonInterval;
InvertDataTable = FunctionInverse;

End[];

EndPackage[];
