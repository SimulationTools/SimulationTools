(* Mathematica Test File *)

data = Table[{i, 10 + i}, {i, 10}];
dt   = ToDataTable[data];

data2 = Table[{i^2, 10 + i}, {i, 10}];
dt2   = ToDataTable[data2];

dt3   = ToDataTable[Module[{om = 2}, N@Table[{t, t^2 Exp[I om t]}, {t, 0, 2 Pi, 2 Pi/10}]]];

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
(* Add                                                          *)
(****************************************************************)

Test[
    Add[dt, dt2]
    ,
    DataTable[{{1, 22}, {2, 1052/45}, {3, 5189/210}, {4, 26}, {5, 1717/63}, {6, 598/21}, {7, 2077/70}, {8, 1388/45}, {9, 32}, {10, 232/7}}]
    ,
    TestID->"Add"
]


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
(* ArrayDepth                                                   *)
(****************************************************************)
Test[
    ArrayDepth[dt]
    ,
    1
    ,
    TestID->"ArrayDepth"
]


(****************************************************************)
(* CoordinateOutline                                            *)
(****************************************************************)
Test[
    CoordinateOutline[dt]
    ,
    Line[{{0, 1}, {0, 10}}]
    ,
    TestID->"CoordinateOutline"
]


(****************************************************************)
(* CoordinateRanges                                             *)
(****************************************************************)

Test[
    CoordinateRanges[dt]
    ,
    {{1,10}}
    ,
    TestID->"CoordinateRanges"
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
(* DataRepresentationQ                                          *)
(****************************************************************)

Test[
    DataRepresentationQ[dt]
    ,
    True
    ,
    TestID->"DataRepresentationQ"
]


(****************************************************************)
(* Dimensions                                                   *)
(****************************************************************)
Test[
    Dimensions[dt]
    ,
    {10}
    ,
    TestID->"Dimensions"
]


(****************************************************************)
(* Div                                                          *)
(****************************************************************)

Test[
    Div[dt, dt2]
    ,
    DataTable[{{1, 1}, {2, 135/128}, {3, 2730/2459}, {4, 7/6}, {5, 945/772}, {6, 168/131}, {7, 1190/887}, {8, 405/289}, {9, 19/13}, {10, 35/23}}]
    ,
    TestID->"Div"
]


(****************************************************************)
(* Drop                                                         *)
(****************************************************************)
Test[
    ToList[Drop[dt, 2]]
    ,
    Drop[data, 2]
    ,
    TestID->"Drop1"
]

Test[
    ToList[Drop[dt, -2]]
    ,
    Drop[data, -2]
    ,
    TestID->"Drop2"
]


(****************************************************************)
(* Endpoints                                                    *)
(****************************************************************)
Test[
    Endpoints[dt]
    ,
    {1, 10}
    ,
    TestID->"Endpoints"
]


(****************************************************************)
(* Extract                                                      *)
(****************************************************************)
Test[
    Extract[dt, {5}]
    ,
    Extract[data[[All, 2]], {5}]
    ,
    TestID->"Extract"
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
(* Frequency                                                    *)
(****************************************************************)

Test[
    Frequency[dt3]
    ,
    DataTable[{{0., 1.9999999999999991}, {0.6283185307179586, 1.9999999999999996},
      {1.2566370614359172, 1.9999999999999996}, {1.8849555921538759, 1.9999999999999998},
      {2.5132741228718345, 2.}, {3.141592653589793, 1.9999999999999996},
      {3.7699111843077517, 1.9999999999999998}, {4.39822971502571, 2.},
      {5.026548245743669, 1.9999999999999996}, {5.654866776461628, 2.}, {6.283185307179586, 2.}}]
	,
	TestID->"Frequency"
]

Test[
    Block[{$NRMMACompatibilityVersion = 0.0}, Frequency[dt3]]
    ,
    DataTable[{{0., 2.}, {0.6283185307179586, 2.}, {1.2566370614359172, 2.},
      {1.8849555921538759, 2.}, {2.5132741228718345, 2.}, {3.141592653589793, 2.},
      {3.7699111843077517, 2.}, {4.39822971502571, 2.}, {5.026548245743669, 2.},
      {5.654866776461628, 2.}}]
	,
	TestID->"Frequency"
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
(* GridNorm                                                     *)
(****************************************************************)

Test[
    GridNorm[dt]
    ,
    Sqrt[2485]
    ,
    TestID->"GridNorm"
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
(* Mean                                                         *)
(****************************************************************)
Test[
    Mean[dt]
    ,
    31/2
    ,
    TestID->"Mean"
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


(****************************************************************)
(* Mul                                                          *)
(****************************************************************)

Test[
    Mul[dt, dt2]
    ,
    DataTable[{{1, 121}, {2, 2048/15}, {3, 31967/210}, {4, 168}, {5, 3860/21}, {6, 4192/21}, {7, 15079/70}, {8, 1156/5}, {9, 247}, {10, 1840/7}}]
    ,
    TestID->"Mul"
]


(****************************************************************)
(* NDerivative                                                  *)
(****************************************************************)

Test[
    NDerivative[1][dt2]
    ,
    DataTable[{{1, 1039/2520}, {4, 1343/5040}, {9, 3541/22680}, {16, 3407/27720}, {25, 35807/360360}, {36, 134737/1621620}, {49, 97089/1361360}, {64, 1321499/21162960}, {81, 392503/7054320}, {100, 1050893/21162960}}]
    ,
    TestID->"NDerivative"
]


(****************************************************************)
(* Part                                                         *)
(****************************************************************)
parts = {
  (*2,*)
  All,
  1 ;; 2,
  (*{2},*)
  1 ;;,
  ;; ,
  ;; 2,
  1 ;; -1 ;; 2
  (*-2,*)
  (*{-1}*)
};

partTest[p_] :=
 Test[
    ToList[dt[[p]]]
    ,
    data[[p]]
    ,
    TestID->"Part "<>ToString[p]
 ];

Scan[partTest, parts];


(****************************************************************)
(* Position                                                     *)
(****************************************************************)
Test[
    Position[dt, 15]
    ,
    Position[data[[All,2]], 15]
    ,
    TestID->"Position"
]


(****************************************************************)
(* Resampled                                                    *)
(****************************************************************)
Test[
    Resampled[dt2, {{1, 100, 10}}]
    ,
    DataTable[{{1, 11}, {11, 731959247/54991872}, {21, 8413225/576576}, {31, 172438869/11088896}, {41, 475922563/28945917}, {51, 842006687/49116672}, {61, 5223521/293216}, {71, 28849999835/1567268352}, {81, 19}, {91, 22821873/1157632}}]
    ,
    TestID->"Resampled 1"
]

Test[
    Resampled[dt2, {1, 100, 10}]
    ,
    Resampled[dt2, {{1, 100, 10}}]
    ,
    TestID->"Resampled 2"
]

Test[
    Resampled[dt2, 10]
    ,
    Resampled[dt2, {{1, 100, 10}}]
    ,
    TestID->"Resampled 3"
]

Test[
    Resampled[{dt, dt2}]
    ,
    {DataTable[{{1, 11}, {2, 12}, {3, 13}, {4, 14}, {5, 15}, {6, 16}, {7, 17}, {8, 18}, {9, 19}, {10, 20}}],
     DataTable[{{1, 11}, {2, 50665445257/4447872000}, {3, 11246292941/959616000}, {4, 12}, {5, 1472831341/120294720}, {6, 1519734919/121977856}, {7, 27562022381/2178176000}, {8, 891846223/69498000}, {9, 13}, {10, 123465387/9382912}}]}
    ,
    TestID->"Resampled 4"
]


(****************************************************************)
(* SameGridQ                                                    *)
(****************************************************************)

Test[
    SameGridQ[dt, dt2]
    ,
    False
    ,
    TestID->"SameGridQ False"
]

Test[
    SameGridQ[dt, dt]
    ,
    True
    ,
    TestID->"SameGridQ True"
]


(****************************************************************)
(* Shifted                                                      *)
(****************************************************************)
Test[
    ToListOfCoordinates[Shifted[dt2, 10]]
    ,
    {11, 14, 19, 26, 35, 46, 59, 74, 91, 110}
    ,
    TestID->"Shifted"
]


(****************************************************************)
(* Sub                                                          *)
(****************************************************************)

Test[
    Sub[dt, dt2]
    ,
    DataTable[{{1, 0}, {2, 28/45}, {3, 271/210}, {4, 2}, {5, 173/63}, {6, 74/21}, {7, 303/70}, {8, 232/45}, {9, 6}, {10, 48/7}}]
    ,
    TestID->"Sub"
]


(****************************************************************)
(* Take                                                         *)
(****************************************************************)
Test[
    ToList[Take[dt, 2]]
    ,
    Take[data, 2]
    ,
    TestID->"Take"
]

Test[
    ToList[Take[dt, 3]]
    ,
    Take[data, 3]
    ,
    TestID->"Take2"
]

Test[
    Catch[ToList[Take[dt, {3}]], ErrorString["Operations which would return a DataTable with a single element are not currently supported."]]
    ,
    {{}, {}}
    ,
    TestID->"Take3"
]

Test[
    ToList[Take[dt, {2, 3}]]
    ,
    Take[data, {2, 3}]
    ,
    TestID->"Take4"
]

Test[
    ToList[Take[dt, {1, 3, 2}]]
    ,
    Take[data, {1, 3, 2}]
    ,
    TestID->"Take5"
]


(****************************************************************)
(* Total                                                        *)
(****************************************************************)
Test[
    Total[dt]
    ,
    155
    ,
    TestID->"Total"
]


