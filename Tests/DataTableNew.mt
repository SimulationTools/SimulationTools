(* Mathematica Test File *)

data = Table[{i, 10 + i}, {i, 10}];
dt   = ToDataTable[data];

data2 = Table[{i^2, 10 + i}, {i, 10}];
dt2   = ToDataTable[data2];

dt3   = ToDataTable[Module[{om = 2}, N@Table[{t, t^2 Exp[I om t]}, {t, 0, 2 Pi, 2 Pi/10}]]];

data4 = Table[{i, -(i-11/2)^2}, {i, 10}];
dt4   = ToDataTable[data4];

data5 = Table[{i, -(i - 11/2)^2}, {i, 10}];
data5[[5, 2]] = 1000;
dt5 = ToDataTable[data5];

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
    DataTable[{{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, {22, 1052/45, 5189/210, 26, 1717/63, 598/21, 2077/70, 1388/45, 32, 232/7}}]
    ,
    TestID->"Add"
]


(****************************************************************)
(* AntiDerivative                                               *)
(****************************************************************)

Test[
    AntiDerivative[dt, {1, 10}]
    ,
    DataTable[{{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, {10., 21.500000010432387, 34.00000001043239, 47.50000001043239, 62.00000001043239, 77.50000001043239, 94.00000001043239, 111.5000000104324, 130.00000001043242, 149.50000001043242}}]
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
(* Coordinate                                                   *)
(****************************************************************)
Test[
    Coordinate[dt2]
    ,
    DataTable[{{1, 4, 9, 16, 25, 36, 49, 64, 81, 100}, {1, 4, 9, 16, 25, 36, 49, 64, 81, 100}}]
    ,
    TestID->"Coordinate"
]


(****************************************************************)
(* CoordinateAtInterpolatedMax                                  *)
(****************************************************************)

Test[
    CoordinateAtInterpolatedMax[dt4]
    ,
    5.499999999909634
    ,
    TestID->"CoordinateAtInterpolatedMax"
]


(****************************************************************)
(* CoordinateAtMax                                              *)
(****************************************************************)

Test[
    CoordinateAtMax[dt]
    ,
    10
    ,
    TestID->"CoordinateAtMax"
]


(****************************************************************)
(* CoordinatesAtMax                                              *)
(****************************************************************)

Test[
    CoordinatesAtMax[dt4]
    ,
    {5, 6}
    ,
    TestID->"CoordinatesAtMax"
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
(* CoordinateRange                                              *)
(****************************************************************)
Test[
    CoordinateRange[dt]
    ,
    {1, 10}
    ,
    TestID->"CoordinateRange"
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
    TestID->"CoordinateSpacing-uniform"
]

Test[
    WithExceptions[CoordinateSpacing[dt2], CoordinateSpacingNonUniform -> e]
    ,
    e
    ,
    TestID->"CoordinateSpacing-nonuniform"
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
    DataTable[{{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, {1, 135/128, 2730/2459, 7/6, 945/772, 168/131, 1190/887, 405/289, 19/13, 35/23}}]
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
(* First                                                        *)
(****************************************************************)

Test[
    First[dt]
    ,
    11
    ,
    TestID -> "First"
]


(****************************************************************)
(* Fourier                                                      *)
(****************************************************************)

Test[
    Fourier[dt]
    ,
    DataTable[{{-1/2, -2/5, -3/10, -1/5, -1/10, 0, 1/10, 1/5, 3/10, 2/5}, {-1.5811388300841898 + 0.*I, -1.5811388300841875 + 0.5137431483730079*I, -1.5811388300841898 + 1.1487646027368057*I, -1.5811388300841875 + 2.1762508994828216*I, -1.5811388300841898 + 4.866244947338651*I, 49.01530373260988 + 0.*I, -1.5811388300841898 - 4.866244947338651*I, -1.5811388300841875 - 2.1762508994828216*I, -1.5811388300841898 - 1.1487646027368057*I, -1.5811388300841875 - 0.5137431483730079*I}}]
    ,
    TestID->"Fourier"
]


(****************************************************************)
(* Frequency                                                    *)
(****************************************************************)

Test[
    Frequency[dt3]
    ,
    DataTable[{{0., 0.6283185307179586, 1.2566370614359172, 1.8849555921538759, 2.5132741228718345, 3.141592653589793, 3.7699111843077517, 4.39822971502571, 5.026548245743669, 5.654866776461628, 6.283185307179586}, {1.9999999999999991, 1.9999999999999996, 1.9999999999999996, 1.9999999999999998, 2., 1.9999999999999996, 1.9999999999999998, 2., 1.9999999999999996, 2., 2.}}]
	,
	TestID->"Frequency"
]

Test[
    Block[{$SimulationToolsCompatibilityVersion = 0.0}, Frequency[dt3]]
    ,
    DataTable[{{0., 0.6283185307179586, 1.2566370614359172, 1.8849555921538759, 2.5132741228718345, 3.141592653589793, 3.7699111843077517, 4.39822971502571, 5.026548245743669, 5.654866776461628}, {2., 2., 2., 2., 2., 2., 2., 2., 2., 2.}}]
	,
	TestID->"Frequency (old version)"
]


(****************************************************************)
(* FourierDCT                                                   *)
(****************************************************************)

Test[
    FourierDCT[dt]
    ,
    DataTable[{{0, (2*Pi)/9, (4*Pi)/9, (2*Pi)/3, (8*Pi)/9, (10*Pi)/9, (4*Pi)/3, (14*Pi)/9, (16*Pi)/9, 2*Pi}, {15.5, 2.0180180604866425, 7.602370863196823*^-16, 0.21615105455192296, 5.748411484595887*^-16, 0.07071067811865477, 4.1764654120333466*^-16, 0.028592689035501988, 2.4701600315198096*^-16, 0.008017936114802292}}]
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
(* InterpolatedMax                                              *)
(****************************************************************)

Test[
    InterpolatedMax[dt4]
    ,
    0.
    ,
    TestID->"InterpolatedMax"
]


(****************************************************************)
(* InterpolatedWhere                                            *)
(****************************************************************)
Test[
    InterpolatedWhere[dt5, #[[2]] > 10 &]
    ,
    DataTable[{{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, {-81/4, -49/4, -25/4, -9/4, -1/4, -1/4, -9/4, -25/4, -49/4, -81/4}}]
    ,
    TestID->"InterpolatedWhere"
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
    DataTable[{{0., 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9}, 
 {49.01530373260988 + 0.*I, 1.5811388300841898 - 4.866244947338651*I, 
  -1.5811388300841875 + 2.1762508994828216*I, 1.5811388300841898 - 
   1.1487646027368057*I, -1.5811388300841875 + 0.5137431483730079*I, 
  1.5811388300841898 + 0.*I, -1.5811388300841875 - 0.5137431483730079*I, 
  1.5811388300841898 + 1.1487646027368057*I, -1.5811388300841875 - 
   2.1762508994828216*I, 1.5811388300841898 + 4.866244947338651*I}}]
    ,
    TestID->"InverseFourier"
]


(****************************************************************)
(* Last                                                         *)
(****************************************************************)

Test[
    Last[dt]
    ,
    20
    ,
    TestID -> "Last"
]


(****************************************************************)
(* MaxCoordinate                                                *)
(****************************************************************)

Test[
    MaxCoordinate[dt]
    ,
    10
    ,
    TestID->"MaxCoordinate"
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
(* MinCoordinate                                                *)
(****************************************************************)

Test[
    MinCoordinate[dt]
    ,
    1
    ,
    TestID->"MinCoordinate"
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
    DataTable[{{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, {121, 2048/15, 31967/210, 168, 3860/21, 4192/21, 15079/70, 1156/5, 247, 1840/7}}]
    ,
    TestID->"Mul"
]


(****************************************************************)
(* NDerivative                                                  *)
(****************************************************************)

Test[
    NDerivative[1][dt2]
    ,
    DataTable[{{1, 4, 9, 16, 25, 36, 49, 64, 81, 100}, {1039/2520, 1343/5040, 3541/22680, 3407/27720, 35807/360360, 134737/1621620, 97089/1361360, 1321499/21162960, 392503/7054320, 1050893/21162960}}]
    ,
    TestID->"NDerivative"
]


(****************************************************************)
(* PadLeft                                                      *)
(****************************************************************)
Test[
    PadLeft[dt, 13]
    ,
    DataTable[{{-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, {0, 0, 0, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20}}]
    ,
    TestID->"PadLeft[dt, 13]"
]

Test[
    PadLeft[dt, 13, 5]
    ,
    DataTable[{{-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, {5, 5, 5, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20}}]
    ,
    TestID->"PadLeft[dt, 13, 5]"
]


(****************************************************************)
(* PadRight                                                     *)
(****************************************************************)
Test[
    PadRight[dt, 13]
    ,
    DataTable[{{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13}, {11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 0, 0, 0}}]
    ,
    TestID->"PadRight[dt, 13]"
]

Test[
    PadRight[dt, 13, 5]
    ,
    DataTable[{{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13}, {11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 5, 5, 5}}]
    ,
    TestID->"PadRight[dt, 13, 5]"
]


(****************************************************************)
(* Part                                                         *)
(****************************************************************)
parts = {
  All,
  1 ;; 2,
  (*{2},*)
  1 ;;,
  ;; ,
  ;; 2,
  1 ;; -1 ;; 2
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

Test[
    dt[[2]]
    ,
    12
    ,
    TestID->"Part 2"
];

Test[
    dt[[-2]]
    ,
    19
    ,
    TestID->"Part -2"
];


(****************************************************************)
(* Phase *)
(****************************************************************)
Test[
    DepVar[Phase[dt3]] == 2 IndVar[Phase[dt3]]
    ,
    True
	,
	TestID->"Phase"
]


(****************************************************************)
(* Pick *)
(****************************************************************)
Test[
    Pick[dt, {True, False, True, True, False, True, True, True, True, True}]
    ,
    DataTable[{{1, 3, 4, 6, 7, 8, 9, 10}, {11, 13, 14, 16, 17, 18, 19, 20}}]
	,
	TestID->"Pick"
]


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
    DataTable[{{1, 11, 21, 31, 41, 51, 61, 71, 81, 91}, {11, 731959247/54991872, 8413225/576576, 172438869/11088896, 475922563/28945917, 842006687/49116672, 5223521/293216, 28849999835/1567268352, 19, 22821873/1157632}}]
    ,
    TestID->"Resampled 1"
]

Test[
    Resampled[dt2, {10}]
    ,
    Resampled[dt2, {{1, 100, 10}}]
    ,
    TestID->"Resampled 2"
]

Test[
    SimulationTools`DataTable`Private`resampled[{dt, dt2}]
    ,
    {DataTable[{{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, {11, 12, 13, 14, 15, 16, 17, 18, 19, 20}}],
     DataTable[{{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, {11, 50665445257/4447872000, 11246292941/959616000, 12, 1472831341/120294720, 1519734919/121977856, 27562022381/2178176000, 891846223/69498000, 13, 123465387/9382912}}]}
    ,
    TestID->"Resampled 4"
]


(****************************************************************)
(* RestrictedToCommonInterval                                   *)
(****************************************************************)
Test[
    RestrictedToCommonInterval[{dt, dt2}]
    ,
    {DataTable[{{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, {11, 12, 13, 14, 15, 16, 17, 18, 19, 20}}],
     DataTable[{{1, 4, 9}, {11, 12, 13}}]}
    ,
    TestID->"RestrictedToCommonInterval"
]


(****************************************************************)
(* RestrictedToInterval                                         *)
(****************************************************************)
Test[
    RestrictedToInterval[dt2, {3, 50}]
    ,
    DataTable[{{4, 9, 16, 25, 36, 49}, {12, 13, 14, 15, 16, 17}}]
    ,
    TestID->"RestrictedToInterval"
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

Test[
    SameGridQ[dt, dt, dt2]
    ,
    False
    ,
    TestID->"SameGridQ multiple False"
]

Test[
    SameGridQ[dt, dt, dt]
    ,
    True
    ,
    TestID->"SameGridQ multiple True"
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
(* Slab                                                         *)
(****************************************************************)
parts = {
  2,
  All,
  1 ;; 3,
  ;; ,
  ;; 3,
  2 ;;
};

slabs = {
  2,
  All,
  1 ;; 3,
  ;; ,
  ;; 3,
  2 ;;
};

slabTest[p_, s_] :=
 Test[
    Slab[dt, s]
    ,
    dt[[p]]
    ,
    TestID->"Slab "<>ToString[s]
 ];

MapThread[slabTest, {parts, slabs}];

Test[
    Slab[dt2, 9 ;; 13]
    ,
    DataTable[{{9, 16}, {13, 14}}]
    ,
    TestID->"Slab non-uniform"
];


(****************************************************************)
(* Sub                                                          *)
(****************************************************************)

Test[
    Sub[dt, dt2]
    ,
    DataTable[{{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, {0, 28/45, 271/210, 2, 173/63, 74/21, 303/70, 232/45, 6, 48/7}}]
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
    WithExceptions[Take[dt, {3}], DataTableSingle -> e]
    ,
    e
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


(****************************************************************)
(* UniformSpacingQ                                              *)
(****************************************************************)
Test[
    UniformSpacingQ[dt]
    ,
    True
    ,
    TestID->"UniformSpacingQ True"
]

Test[
    UniformSpacingQ[dt2]
    ,
    False
    ,
    TestID->"UniformSpacingQ False"
]


(****************************************************************)
(* WithResampling                                               *)
(****************************************************************)

Test[
    WithResampling["First", Resampled[{dt, dt2}]]
    ,
    Resampled[{dt, dt2}, dt]
    ,
    TestID->"WithResampling First"
]

Test[
    WithResampling["Last", Resampled[{dt2, dt}]]
    ,
    Resampled[{dt2, dt}, dt]
    ,
    TestID->"WithResampling Last"
]

