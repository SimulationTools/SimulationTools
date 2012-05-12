(* ::Package:: *)

(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["DataRepresentations`", {"DataTable`", "DataRegion"}];

ToListOfData::usage = ToListOfData::usage<>"
ToListOfData[d] returns the N-dimensional array of data in DataRegion d.";

Begin["`Private`"];

(*******************************************************************************************)
(* Redefine various built-in Mathematica functions to work on our new data types           *)
(*******************************************************************************************)

$DataTypes = {DataRegion, DataTable};
$MappedFunctions =
  {Abs, Sqrt, Re, Im, Conjugate,
   Log, Log2, Log10, Exp,
   Sin, Cos, Tan, Csc, Sec, Cot,
   ArcSin, ArcCos, ArcTan, ArcCsc, ArcSec, ArcCot,
   Sinh, Cosh, Tanh, Csch, Sech, Coth,
   ArcSin, ArcCosh, ArcTanh, ArcCsch, ArcSech, ArcCoth,
   Sinc, Haversine, InverseHaversine, Gudermannian, InverseGudermannian};
   
overload[func_, type_] := (type /: func[x : Blank[type]] := Map[func, x]);

Table[overload[f, t], {f, $MappedFunctions}, {t, 
DataRegion /: Log[b_, d_DataRegion] := MapDataRegion[Log[b,#] &, d];
DataRegion /: ArcTan[x_DataRegion, y_DataRegion] := MapThreadDataRegion[ArcTan, {x, y}];

DataRegion /: Total[DataRegion[_,data_]]:=Total[data];
DataRegion /: Max[DataRegion[_,data_]]:=Max[data];
DataRegion /: Min[DataRegion[_,data_]]:=Min[data];
DataRegion /: Mean[DataRegion[_,data_]]:=Mean[data];

DataRegion /: Times[a_, DataRegion[h_,data_]] := DataRegion[h, a data];
DataRegion /: Times[DataRegion[h1_,data1_], DataRegion[h2_,data2_]]:=DataRegion[h1, data1*data2]
DataRegion /: Power[DataRegion[h_,data_], a_] := DataRegion[h, data^a];
DataRegion /: Power[a_, DataRegion[h_,data_]] := DataRegion[h, a^data];
DataRegion /: Power[DataRegion[h1_,data1_], DataRegion[h2_,data2_]] := DataRegion[h1, data1^data2];
DataRegion /: Plus[DataRegion[h1_,data1_], DataRegion[h2_,data2_]]:= DataRegion[h1, data1+data2]
DataRegion /: Plus[DataRegion[h1_,data1_], a_] := DataRegion[h1, data1+a]
DataRegion /: Mod[d_DataRegion, n_] := MapDataRegion[Mod[#, n]&, d];


DataRegion/:Position[DataRegion[h1_,data1_], pattern_, opts___] := Position[data1, pattern, opts];

DataRegion/:Extract[DataRegion[h1_,data1_], positions_List] := Extract[data1, positions];

DataRegion /: Interpolation[v_DataRegion, opts___] :=
  Module[{data = GetData[v], ndims = GetNumDimensions[v]},
    ListInterpolation[Transpose[data,Reverse[Range[ndims]]], GetDataRange[v], opts]
];

NormL2[d_DataRegion] :=
 Sqrt[Times@@GetSpacing[d] * Plus @@ Flatten[GetData[d^2]]];

NDerivative[d:DataRegion[h_,_], dir_Integer] :=
 Module[{ndims},
   ndims   = GetNumDimensions[d];
   (NDerivative@@UnitVector[ndims, dir])[d]
];

NDerivative[d_DataRegion] :=
 Module[{ndims, result},
  ndims = GetNumDimensions[d];

  If[ndims == 1,
   result = NDerivative[d, 1];,
   Error["Must specify a direction when applying NDerivative to a DataRegion of dimension greater than 1."];
   result = $Failed;
  ];

  result
];

NDerivative[derivs__][d:DataRegion[h_,_], opts___] :=
 Module[{origin, spacing, dimensions, grid, data, deriv, newh},
  origin  = GetOrigin[d];
  spacing = GetSpacing[d];
  dimensions = GetDimensions[d];

  (* Get the grid in the form {{x1, ..., xn}, {y1, ..., yn}, ...} *)
  grid = origin + spacing (Range /@ dimensions-1);
  data = GetData[d];

  deriv = NDSolve`FiniteDifferenceDerivative[Reverse[Derivative[derivs]], grid, data, opts];

  newh = replaceRules[h, {Name -> "dx"<>ToString[derivs]<>"_"<>GetVariableName[d]}];

  DataRegion[newh, deriv]
];

UFDWeights[m_, n_, s_, h_] :=
 CoefficientList[Normal[Series[x^s Log[x]^m, {x, 1, n}]/h^m], x]

TimeDerivative[dr:{__DataRegion}, centering_:Automatic] :=
 Module[{nd, dims, spacing, origin, variable, sorted, times, dt, stencil, offset, deriv, attr, newTime},
  nd = GetNumDimensions/@dr;
  If[Not[Equal@@nd],
   Error["Error, can't compute time derivative from DataRegions with a different number of dimensions."];
   Return[$Failed];
  ];

  dims = GetDimensions/@dr;
  If[Not[Equal@@dims],
   Error["Error, can't compute time derivative from DataRegions with different dimensions."];
   Return[$Failed];
  ];

  spacing = GetSpacing/@dr;
  If[Not[Equal@@spacing],
    Error["Error, can't compute time derivative from DataRegions with different spacings."];
    Return[$Failed];
  ];

  origin = GetSpacing/@dr;
  If[Not[Equal@@origin],
    Error["Error, can't compute time derivative from DataRegions with different origins."];
    Return[$Failed];
  ];

  variable = GetVariableName/@dr;
  If[Not[Equal@@variable],
    Error["Error, can't compute time derivative from DataRegions with different variables."];
    Return[$Failed];
  ];

  (* Sort DataRegions by time *)
  sorted = SortBy[dr, GetTime];

  (* Check spacing is uniform *)
  times = GetTime/@sorted;
  If[Apply[Or, Map[# > 10^-10 &, Abs[1 - Differences[times]/Differences[times][[1]]]]],
    Error["Error, can't compute time derivative from DataRegions with non-uniform spacing in time."];
    Return[$Failed];
  ];

  (* Get finite differencing stencil *)
  If[centering === Automatic, offset = (Length[dr]-1)/2, offset = centering];
  dt = times[[2]] - times[[1]];
  stencil = UFDWeights[1, Length[dr]-1, offset, dt];
  
  (* Compute derivative using finite differences *)
  If[Length[stencil]!=Length[sorted],
    Error["Error, can't compute time derivative from inconsistent stencil and DataRegions."];
    Return[$Failed];
  ];
  deriv = stencil.sorted;

  (* Correct time and variable name attributes *)
  newTime = times[[1]]+offset*dt;
  attr=replaceRules[GetAttributes[deriv], {Time-> newTime, Name -> "dt_"<>variable[[1]]}];

  DataRegion[ attr, GetData[deriv]]
];

(* We cannot use upvalues here, as the DataRegion appears too deep in
the expression *)

Unprotect[ListLinePlot];

ListLinePlot[ds:List[DataRegion[___]..], opts___] :=
  ListLinePlot[ToList /@ ToDataTable /@ ds, opts];

ListLinePlot[d:DataRegion[___], opts___] :=
   ListLinePlot[{d}, opts];

Protect[ListLinePlot];

Unprotect[ListPlot];

ListPlot[ds:List[DataRegion[___]..], opts___] :=
  ListPlot[ToList /@ ToDataTable /@ ds, opts];

ListPlot[d:DataRegion[___], opts___] :=
   ListPlot[{d}, opts];

Protect[ListPlot];



(* Take a list of elements of the form {x,y,..., f} and convert it to
   a DataRegion. It is assumed (and not checked) that the data grid is
   regular.  *)

TableToDataRegion[t_List] :=
 Module[{d, sorted, split, spacing, origin, subregions, s, subdims,
   suborigin, subspacing},
  d = Length[t[[1]]] - 1; (* Number of dimensions in the data *)

  sorted = Sort[t, #1[[d]] < #2[[d]] &];
  split = SplitBy[sorted, #[[d]] &];
  spacing = split[[2, 1, d]] - split[[1, 1, d]];
  origin = split[[1, 1, d]];
  If[d == 1,
   ToDataRegion[Map[Last, sorted],
    {origin}, {spacing}],
   (* else *)
   subregions =
     Map[TableToDataRegion, Map[Drop[#, {d}] &, split, {2}]];
   s = First[subregions];
   subdims = GetDimensions[s];
   suborigin = GetOrigin[s];
   subspacing = GetSpacing[s];
   MakeDataRegion[Map[GetData, subregions], "table",
    Append[subdims, Length[subregions]], Append[suborigin, origin],
    Append[subspacing, spacing], 0]]];

(* TODO: generalise this to arbitrary number of dimensions *)
(* SetAttributes[EvaluateOnDataRegion, HoldFirst]; *)
EvaluateOnDataRegion[exprp_, {t_, x_, y_, z_}, dp_DataRegion] :=
  Module[
    {xd, yd, zd, td, result, expr, d},
    expr = ReleaseHold[exprp];
    d = ReleaseHold[dp];
    {xd, yd, zd} = Table[GetCoordinate[d, i], {i, 3}];
    td = GetTime[d];
    result = (0.*x + expr) /. {x :> xd, y :> yd, z :> zd, t :> td};
    If[!NumberQ@Max@Flatten[GetData[result]],
       Error["Expression " <> ToString[expr,InputForm]<> " did not evaluate to a numeric value in coordinates "<>ToString[{t,x,y,z}]<>" ("<>ToString[Max@Flatten[GetData[result]],InputForm]]];
    result];

EvaluateOnDataRegion[exprp_, {t_, x_, y_}, dp_DataRegion] :=
  Module[
    {xd, yd, td, result, expr, d},
    expr = ReleaseHold[exprp];
    d = ReleaseHold[dp];
    {xd, yd} = Table[GetCoordinate[d, i], {i, 2}];
    td = GetTime[d];
    result = (0.*x + expr) /. {x :> xd, y :> yd, t :> td};
    If[!NumberQ@Max@Flatten[GetData[result]],
       Error["Expression " <> ToString[expr,InputForm]<> " did not evaluate to a numeric value in coordinates "<>ToString[{t,x,y}]<>" ("<>ToString[Max@Flatten[GetData[result]],InputForm]]];
    result];

(**********************************************************************************)
(* Deprecated functions *)
(**********************************************************************************)

MakeDataRegion[data_List, name_String, dims_List, origin_List, spacing_List, time_] :=
  DataRegion[{Name -> name, Origin -> origin, Spacing -> spacing, Time -> time},
             Developer`ToPackedArray[data]];

SliceData[v:DataRegion[h_, data_], dim_Integer, coord_:0] :=
 Module[{index, newOrigin, newSpacing, h2, origin, spacing, dims, range, slice, ndims},
  origin = GetOrigin[v];
  spacing = GetSpacing[v];
  dims = GetDimensions[v];
  ndims = GetNumDimensions[v];

  If[dim > ndims,
    Error["Slicing direction "<>ToString[dim]<>" is greater than dimension "<>
      ToString[ndims]<>" of the DataRegion."]
  ];

  range = GetDataRange[v][[dim]];

  If[(coord < range[[1]]) || (coord >range[[2]]),
    Error["Slice coordinate "<>ToString[coord]<>" is outside the range "<>
      ToString[range, StandardForm]<>" of the DataRegion."]
  ];

  index = Round[(coord - origin[[dim]])/spacing[[dim]]+1];
  newOrigin = Drop[origin, {dim}];
  newSpacing = Drop[spacing, {dim}];
  h2 = replaceRules[h, {Origin -> newOrigin, Spacing-> newSpacing}];
  slice = Sequence @@ Reverse[Join[ConstantArray[All,dim-1],{index},ConstantArray[All,ndims-dim]]];
  Return[DataRegion[h2, data[[slice]] ]]
];

SliceData[v_DataRegion, dims_List, coords_:0] := 
  Fold[SliceData[#, Sequence@@#2]&, v, Reverse[SortBy[Thread[{dims, coords}], First]]];

GetData[DataRegion[h_, data_]] :=
  data;

End[];

EndPackage[];
