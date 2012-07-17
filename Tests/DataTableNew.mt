(* Mathematica Test File *)

data = Table[{i, 10 + i}, {i, 10}];
dt   = ToDataTable[data];

data2 = Table[{i^2, 10 + i}, {i, 10}];
dt2   = ToDataTable[data2];

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
     Times[#, #] &, Times[#, 3] &, Power[3, #] &, Power[#, 3] &, Power[#, #] &,
     Plus[3, #] &, Plus[#, #] &, Plus[#, 3] &, Mod[3, #] &, Mod[#, #] &, Mod[#, 3] &}]

test2[f_] :=
 Test[
   f[dt]
   ,
   f[data[[All,2]]]
   ,
   TestID -> "Built-in function: "<>ToString[f]
 ];

Scan[test2, {Total, Max, Min, Mean}]

test3[f_] :=
 Test[
   f[dt]
   ,
   f[data]
   ,
   TestID -> "Built-in function: "<>ToString[f]
 ];

Scan[test3, DataTable`Private`$1DPlotFunctions]


(****************************************************************)
(* AntiDerivative                                               *)
(****************************************************************)

Test[
    AntiDerivative[dt, {1, 10}]
    ,
    DataTable[{{1, 10.}, {2, 21.500000010432387}, {3, 34.00000001043239},
      {4, 47.50000001043239}, {5, 62.00000001043239}, {6, 77.50000001043239},
      {7, 94.00000001043239}, {8, 111.5000000104324}, {9, 130.00000001043242},
      {10, 149.50000001043242}}]
    ,
    TestID->"AntiDerivative"
]


(****************************************************************)
(* CoordinateSpacing                                            *)
(****************************************************************)

Test[
    CoordinateSpacing[dt]
    ,
    1
    ,
    TestID->"CoordinateSpacing"
]

Test[
    Catch[CoordinateSpacing[dt2],
      ErrorString["CoordinateSpacing undefined for non-uniform DataTables."], f]
    ,
    f[{{}, {}}, ErrorString["CoordinateSpacing undefined for non-uniform DataTables."]]
    ,
    TestID->"CoordinateSpacing"
]


(****************************************************************)
(* CoordinateSpacings                                           *)
(****************************************************************)

Test[
    CoordinateSpacings[dt]
    ,
    {1}
    ,
    TestID->"CoordinateSpacings"
]


(****************************************************************)
(* Fourier                                                      *)
(****************************************************************)

Test[
    Fourier[dt]
    ,
    DataTable[{{-1/2, -1.5811388300841898 + 0.*I},
      {-2/5, -1.5811388300841875 + 0.5137431483730079*I},
      {-3/10, -1.5811388300841898 + 1.1487646027368057*I},
      {-1/5, -1.5811388300841875 + 2.1762508994828216*I},
      {-1/10, -1.5811388300841898 + 4.866244947338651*I}, {0, 49.01530373260988 + 0.*I},
      {1/10, -1.5811388300841898 - 4.866244947338651*I},
      {1/5, -1.5811388300841875 - 2.1762508994828216*I},
      {3/10, -1.5811388300841898 - 1.1487646027368057*I},
      {2/5, -1.5811388300841875 - 0.5137431483730079*I}}]
    ,
    TestID->"Fourier"
]


(****************************************************************)
(* FourierDCT                                                   *)
(****************************************************************)

Test[
    FourierDCT[dt]
    ,
    DataTable[{{0, 15.5}, {(2*Pi)/9, 2.0180180604866425},
      {(4*Pi)/9, 7.602370863196823*^-16}, {(2*Pi)/3, 0.21615105455192296},
      {(8*Pi)/9, 5.748411484595887*^-16}, {(10*Pi)/9, 0.07071067811865477},
      {(4*Pi)/3, 4.1764654120333466*^-16}, {(14*Pi)/9, 0.028592689035501988},
      {(16*Pi)/9, 2.4701600315198096*^-16}, {2*Pi, 0.008017936114802292}}]
    ,
    TestID->"FourierDCT"
]


(****************************************************************)
(* Interpolation                                                *)
(****************************************************************)

Test[
    Interpolation[dt][3.5]
    ,
    13.5
    ,
    TestID->"Interpolation"
]


(****************************************************************)
(* InverseFourier                                               *)
(****************************************************************)

Test[
    InverseFourier[dt]
    ,
    DataTable[{{0., 49.01530373260988 + 0.*I},
      {0.1, 1.5811388300841898 - 4.866244947338651*I},
      {0.2, -1.5811388300841875 + 2.1762508994828216*I},
      {0.3, 1.5811388300841898 - 1.1487646027368057*I},
      {0.4, -1.5811388300841875 + 0.5137431483730079*I}, {0.5, 1.5811388300841898 + 0.*I},
      {0.6, -1.5811388300841875 - 0.5137431483730079*I},
      {0.7, 1.5811388300841898 + 1.1487646027368057*I},
      {0.8, -1.5811388300841875 - 2.1762508994828216*I},
      {0.9, 1.5811388300841898 + 4.866244947338651*I}}]
    ,
    TestID->"InverseFourier"
]


(****************************************************************)
(* MaxCoordinates                                               *)
(****************************************************************)

Test[
    MaxCoordinates[dt]
    ,
    {10}
    ,
    TestID->"MaxCoordinates"
]


(****************************************************************)
(* MinCoordinates                                               *)
(****************************************************************)

Test[
    MinCoordinates[dt]
    ,
    {1}
    ,
    TestID->"MinCoordinates"
]


(****************************************************************)
(* MinCoordinateSpacing                                         *)
(****************************************************************)

Test[
    MinCoordinateSpacing[dt2]
    ,
    3
    ,
    TestID->"MinCoordinateSpacing"
]


