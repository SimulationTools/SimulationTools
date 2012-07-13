(* Mathematica Test File *)

data = Table[{i, 10 + i}, {i, 10}];
dt   = ToDataTable[data];

(****************************************************************)
(* Built-in functions *)
(****************************************************************)
test[f_] := 
 Test[
   {ToListOfData[f[dt]], CoordinateRanges[f[dt]]}
   ,
   {f[data[[All,2]]], CoordinateRanges[dt]}
   ,
   TestID -> "Built-in function: "<>ToString[InputForm[f]]
 ]

Scan[test, {Abs, Sqrt, Re, Im, Conjugate, Log, Log[3, #] &, Log2, Log10, 
     Exp, Sin, Cos, Tan, Csc, Sec, Cot, ArcSin, ArcCos, ArcTan, ArcCsc, 
     ArcSec, ArcCot, ArcTan[#, #] &, Sinh, Cosh, Tanh, Csch, Sech, Coth,
     ArcSinh, ArcCosh, ArcTanh, ArcCsch, ArcSech, ArcCoth, Sinc, 
     Haversine, InverseHaversine, Gudermannian, InverseGudermannian, Times[3, #] &, 
     Times[#, #] &, Times[#, 3] &, Power[3, #] &, Power[#, #] &}]

test2[f_] :=
 Test[
   f[dt]
   ,
   f[data[[All,2]]]
   ,
   TestID -> "Built-in function: "<>ToString[f]
 ];

Scan[test2, {Total, Max, Min, Mean}]
