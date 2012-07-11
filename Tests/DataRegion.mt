(* Mathematica Test File *)

data  = Table[100 i + 10 j + k, {i, 2}, {j, 3}, {k, 4}];
data2 = 2 data;

dr = ToDataRegion[data, {10, 20, 30}, {0.1, 0.2, 0.3},
   VariableName -> "test region", Time -> 42];
dr2 = ToDataRegion[data2, {10, 20, 30}, {0.1, 0.2, 0.3},
   VariableName -> "test region", Time -> 47];
dr3 = ToDataRegion[data2, {9.9, 20, 30}, {0.1, 0.2, 0.3},
   VariableName -> "test region", Time -> 47];
dr4 = ToDataRegion[data, {10, 20, 30.3}, {0.1, 0.2, 0.3},
   VariableName -> "test region", Time -> 47];

(****************************************************************)
(* Downsampled *)
(****************************************************************)
Test[
    Downsampled[dr, {2, 2, 2}]
    ,
    DataRegion[{VariableName -> "test region",
      DataRegion`Private`Origin -> {10., 20., 30.},
      Spacing -> {0.2, 0.4, 0.6},
      DataRegion`Private`Time -> Undefined}, {{{111, 113}, {131, 133}}}]
    ,
    TestID->"Downsampled"
]


(****************************************************************)
(* Resampled *)
(****************************************************************)
grid = {{10.`, 10.05`, 0.05}, {20.`, 20.3`, 0.1}, {30.`, 30.9`, 0.4}};

Test[
    Resampled[dr, grid]
    ,
    DataRegion[{
      VariableName -> "test region",
      DataRegion`Private`Origin -> {10., 20., 30.},
      Spacing -> {0.05, 0.1, 0.4},
      DataRegion`Private`Time -> Undefined},
      {{{111., 112.33333333333333, 113.66666666666667}, {116.00000000000009, 117.33333333333341, 118.66666666666676},
      	{121., 122.33333333333333, 123.66666666666667}, {126.00000000000009, 127.33333333333343, 128.66666666666677}},
      	{{161.00000000000088, 162.33333333333422, 163.66666666666757}, {166.00000000000097, 167.3333333333343, 168.66666666666765}, 
        {171.00000000000088, 172.33333333333422, 173.66666666666757}, {176.00000000000097, 177.3333333333343, 178.66666666666765}}}
      ]
    ,
    TestID->"Resampled"
]

Test[
    Resampled[{dr, dr4}]
    ,
    {
     DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {10., 20., 30.3}, Spacing -> {0.1, 0.2, 0.3}, DataRegion`Private`Time -> Undefined}, {{{112., 113.00000000000001, 114.00000000000001}, {122., 123.00000000000001, 124.00000000000001}, {132., 133., 134.}}, {{212., 213., 214.}, {222., 223., 224.}, {232., 233., 234.}}}], 
     DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {10., 20., 30.3}, Spacing -> {0.1, 0.2, 0.3}, DataRegion`Private`Time -> Undefined}, {{{111., 112., 113.00000000000001}, {121., 122., 123.00000000000001}, {131., 132., 133.}}, {{211., 212., 213.}, {221., 222., 223.}, {231., 232., 233.}}}]
    }
    ,
    TestID->"Resampled"
]

(****************************************************************)
(* Interpolation *)
(****************************************************************)
Test[
    Quiet[Interpolation[dr][10.05, 20.3, 30.7], ListInterpolation::inhr]
    ,
    178.3333333333343`
    ,
    TestID->"Interpolation"
]


(****************************************************************)
(* SameGridQ *)
(****************************************************************)
Test[
    SameGridQ[dr, dr3]
    ,
    False
    ,
    TestID->"SameGridQFalse"
]

Test[
    SameGridQ[dr, dr2]
    ,
    True
    ,
    TestID->"SameGridQTrue"
]


(****************************************************************)
(* Map *)
(****************************************************************)
Test[
    Map[Sqrt[#/2] &, data]
    ,
    ToListOfData[Map[Sqrt[#/2] &, dr], Flatten -> False]
    ,
    TestID->"Map"
]


(****************************************************************)
(* MapThread *)
(****************************************************************)
Test[
    MapThread[f, {dr,dr2}]
    ,
    DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {10., 20., 30.},
    	Spacing -> {0.1, 0.2, 0.3},
    	DataRegion`Private`Time -> 42},
    	{{{f[111, 222], f[112, 224], f[113, 226], f[114, 228]}, {f[121, 242], f[122, 244],
    		 f[123, 246], f[124, 248]}, {f[131, 262], f[132, 264], f[133, 266], f[134, 268]}},
    		  {{f[211, 422], f[212, 424], f[213, 426], f[214, 428]}, {f[221, 442], f[222, 444], 
    		  	f[223, 446], f[224, 448]}, {f[231, 462], f[232, 464], f[233, 466], f[234, 468]}}}] 
    ,
    TestID->"MapThread"
]


(****************************************************************)
(* Part *)
(****************************************************************)
parts = {
  2,
  All,
  1 ;; 2,
  Sequence[1 ;; 2, All, {2}],
  Sequence[1 ;; 2, 1 ;;, 2],
  Sequence[1 ;; 2, ;; , 2],
  Sequence[1 ;; 2, ;; 2, 2](*,
  1 ;; -1 ;; 2 ,
  -1,
  {-1}*)
};

partTest[p_] :=
 Test[
    ToListOfData[dr[[p]], Flatten -> False]
    ,
    data[[p]]
    ,
    TestID->"Part "<>ToString[p]
 ];

Scan[partTest, parts];

slabs = {
  10.1,
  All,
  10.0 ;; 10.1,
  Sequence[10.0 ;; 10.1, All, {30.3}],
  Sequence[10.0 ;; 10.1, 20.0 ;;, 30.3](*,
  Sequence[10.0 ;; 10.1, ;; , 30.3],
  Sequence[10.0 ;; 10.1, ;; 20.2, 30.3]*)
};
 
slatTest[p_, s] :=
 Test[
    ToListOfData[dr[[p]], Flatten -> False]
    ,
    data[[p]]
    ,
    TestID->"Part "<>ToString[p]
 ];

Scan[slabTest, Transpose[parts[[1;;5]], slabs]];

(****************************************************************)
(* Take *)
(****************************************************************)
Test[
    ToListOfData[Take[dr, 2], Flatten -> False]
    ,
    Take[data, 2]
    ,
    TestID->"Take"
]

Test[
    ToListOfData[Take[dr, All, 3], Flatten -> False]
    ,
    Take[data, All, 3]
    ,
    TestID->"Take2"
]

Test[
    ToListOfData[Take[dr, All, {3}], Flatten -> False]
    ,
    Take[data, All, {3}]
    ,
    TestID->"Take3"
]

Test[
    ToListOfData[Take[dr, All, {2, 3}], Flatten -> False]
    ,
    Take[data, All, {2, 3}]
    ,
    TestID->"Take4"
]

Test[
    ToListOfData[Take[dr, All, {1, 3, 2}], Flatten -> False]
    ,
    Take[data, All, {1, 3, 2}]
    ,
    TestID->"Take5"
]


(****************************************************************)
(* Drop *)
(****************************************************************)
Test[
    ToListOfData[Drop[dr, None, 2], Flatten -> False]
    ,
    Drop[data, None, 2]
    ,
    TestID->"Drop1"
]

Test[
    ToListOfData[Drop[dr, None, 2], Flatten -> False]
    ,
    Drop[data, None, 2]
    ,
    TestID->"Drop2"
]

Test[
    Catch[Drop[dr, None, -2], ErrorString["Negative part specifications are not currently supported by DataRegion."]]
    ,
    {{},{}}
    ,
    TestID->"Drop3"
]


(****************************************************************)
(* Coordinate *)
(****************************************************************)
Test[
    ToListOfData[Coordinate[dr, 1], Flatten -> False][[Sequence @@ #]] & /@ {{1, 1, 1}, {2, 1, 1}, {1, 2, 1}, {1, 1, 2}, {2, 2, 1}, {2, 1, 2}, {1, 2, 2}, {2, 2, 2}}
    ,
    {10.`, 10.1`, 10.`, 10.`, 10.1`, 10.1`, 10.`, 10.1`}
    ,
    TestID->"Coordinate1"
]

Test[
    ToListOfData[Coordinate[dr, 2], Flatten -> False][[Sequence @@ #]] & /@ {{1, 1, 1}, {2, 1, 1}, {1, 2, 1}, {1, 1, 2}, {2, 2, 1}, {2, 1, 2}, {1, 2, 2}, {2, 2, 2}}
    ,
    {20.`, 20.`, 20.2`, 20.`, 20.2`, 20.`, 20.2`, 20.2`}
    ,
    TestID->"Coordinate2"
]

Test[
    ToListOfData[Coordinate[dr, 3], Flatten -> False][[Sequence @@ #]] & /@ {{1, 1, 1}, {2, 1, 1}, {1, 2, 1}, {1, 1, 2}, {2, 2, 1}, {2, 1, 2}, {1, 2, 2}, {2, 2, 2}}
    ,
    {30.`, 30.`, 30.`, 30.3`, 30.`, 30.3`, 30.3`, 30.3`}
    ,
    TestID->"Coordinate3"
]


(****************************************************************)
(* NDerivative *)
(****************************************************************)
Test[
    Quiet[DeleteDuplicates[ToListOfData[NDerivative[dr, 3]]], NDSolve`FiniteDifferenceDerivative::ordred]
    ,
    {3.3333333333333854, 3.333333333333364, 3.3333333333333286, 3.3333333333333712, 3.333333333333428, 3.3333333333333997, 3.3333333333332575, 3.3333333333333144, 3.333333333333485, 3.333333333333286, 3.33333333333303}
    ,
    TestID->"NDerivative1"
]

Test[
    Quiet[DeleteDuplicates[ToListOfData[NDerivative[dr, 2]]], NDSolve`FiniteDifferenceDerivative::ordred]
    ,
    {50.00000000000023, 50.00000000000017, 50.00000000000006, 50.000000000000114, 50., 50.000000000000455, 49.99999999999977}
    ,
    TestID->"NDerivative2"
]

Test[
    Quiet[DeleteDuplicates[ToListOfData[NDerivative[dr, 1]]], NDSolve`FiniteDifferenceDerivative::ordred]
    ,
    {1000.0000000000034, 1000.0000000000036, 1000.0000000000039}
    ,
    TestID->"NDerivative3"
]


(****************************************************************)
(* Position *)
(****************************************************************)
Test[
    Position[dr, 123]
    ,
    Position[data, 123]
    ,
    TestID->"Position"
]


(****************************************************************)
(* Extract *)
(****************************************************************)
Test[
    Extract[dr, {{1, 2, 3}}]
    ,
    Extract[data, {{1, 2, 3}}]
    ,
    TestID->"Extract"
]


(****************************************************************)
(* ToListOfData *)
(****************************************************************)
Test[
    ToListOfData[dr, Flatten -> False][[1, 2, 3]]
    ,
    123
    ,
    TestID->"ToListOfDataNoFlatten"
]

Test[
    ToListOfData[dr, Flatten -> True][[7]]
    ,
    123
    ,
    TestID->"ToListOfDataFlatten"
]


(****************************************************************)
(* ToListOfCoordinates *)
(****************************************************************)
Test[
    ToListOfCoordinates[dr, Flatten -> False][[1, 2, 3]]
    ,
    {10.`, 20.2`, 30.6`}
    ,
    TestID->"ToListOfCoordinatesNoFlatten"
]

Test[
    ToListOfCoordinates[dr, Flatten -> True][[7]]
    ,
    {10.`, 20.2`, 30.6`}
    ,
    TestID->"ToListOfCoordinatesFlatten"
]


(****************************************************************)
(* ToList *)
(****************************************************************)
Test[
    ToList[dr, Flatten -> False][[1, 2, 3]]
    ,
    {10.`, 20.2`, 30.6`, 123}
    ,
    TestID->"ToListNoFlatten"
]

Test[
    ToList[dr, Flatten -> True][[7]]
    ,
    {10.`, 20.2`, 30.6`, 123}
    ,
    TestID->"ToListFlatten"
]


(****************************************************************)
(* CoordinateRanges *)
(****************************************************************)
Test[
    CoordinateRanges[dr]
    ,
    {{10.`, 10.1`}, {20.`, 20.4`}, {30.`, 30.9`}}
    ,
    TestID->"CoordinateRanges"
]


(****************************************************************)
(* CoordinateSpacings *)
(****************************************************************)
Test[
    CoordinateSpacings[dr]
    ,
    {0.1`, 0.2`, 0.3`}
    ,
    TestID->"CoordinateSpacings"
]


(****************************************************************)
(* MinCoordinates *)
(****************************************************************)
Test[
    MinCoordinates[dr]
    ,
    {10.`, 20.`, 30.`}
    ,
    TestID->"MinCoordinates"
]


(****************************************************************)
(* MaxCoordinates *)
(****************************************************************)
Test[
    MaxCoordinates[dr]
    ,
    {10.1`, 20.4`, 30.9`}
    ,
    TestID->"MaxCoordinates"
]


(****************************************************************)
(* VariableName *)
(****************************************************************)
Test[
    VariableName[dr]
    ,
    "test region"
    ,
    TestID->"VariableName"
]


(****************************************************************)
(* Dimensions *)
(****************************************************************)
Test[
    Dimensions[dr]
    ,
    {2,3,4}
    ,
    TestID->"Dimensions"
]


(****************************************************************)
(* ArrayDepth *)
(****************************************************************)
Test[
    ArrayDepth[dr]
    ,
    3
    ,
    TestID->"ArrayDepth"
]


(****************************************************************)
(* Built-in functions *)
(****************************************************************)
test[f_] := 
 Test[
   {f[data], CoordinateRanges[f[dr]]}
   ,
   {ToListOfData[f[dr], Flatten -> False], CoordinateRanges[dr]}
   ,
   TestID -> "Built-in function: "<>ToString[f]
 ]

Scan[test, {Abs, Sqrt, Re, Im, Conjugate, Log, Log[3, #] &, Log2, Log10, 
     Exp, Sin, Cos, Tan, Csc, Sec, Cot, ArcSin, ArcCos, ArcTan, ArcCsc, 
     ArcSec, ArcCot, ArcTan[#, #] &, Sinh, Cosh, Tanh, Csch, Sech, Coth,
     ArcSinh, ArcCosh, ArcTanh, ArcCsch, ArcSech, ArcCoth, Sinc, 
     Haversine, Gudermannian, InverseGudermannian, Times[3, #] &, 
     Times[#, #] &, Times[#, 3] &, Power[3, #] &, Power[#, #] &}]

test2[f_] :=
 Test[
   f[data]
   ,
   f[dr]
   ,
   TestID -> "Built-in function: "<>ToString[f]
 ];

Scan[test2, {Total, Max, Min, Mean}]

test3[f_] :=
 Test[
   f[data[[1, 2]], DataRange -> CoordinateRanges[dr[[1, 2]]][[1]]]
   ,
   f[dr[[1, 2]]]
   ,
   TestID -> "Built-in function: "<>ToString[f]
 ];

Scan[test3, DataRegion`Private`$1DPlotFunctions]


test4[f_] :=
 Test[
   f[data[[1]], DataRange -> CoordinateRanges[dr[[1]]]]
   ,
   f[dr[[1]]]
   ,
   TestID -> "Built-in function: "<>ToString[f]
 ];

Scan[test4, DataRegion`Private`$2DPlotFunctions]


(****************************************************************)
(* GridNorm *)
(****************************************************************)
Test[
    GridNorm[dr]
    ,
    68.22521527998282`
    ,
    TestID->"GridNorm"
]


(****************************************************************)
(* CoordinateOutline *)
(****************************************************************)
Test[
    CoordinateOutline[dr[[1,2]]]
    ,
    Line[{{0, 30.`}, {0, 30.9`}}]
    ,
    TestID->"CoordinateOutline1D"
]

Test[
    CoordinateOutline[dr[[1]]]
    ,
    Rectangle[{20.`, 30.`}, {20.4`, 30.9`}]
    ,
    TestID->"CoordinateOutline2D"
]

Test[
    CoordinateOutline[dr]
    ,
    Cuboid[{10.`, 20.`, 30.`}, {10.1`, 20.4`, 30.9`}]
    ,
    TestID->"CoordinateOutline3D"
]


(****************************************************************)
(* ToDataRegion *)
(****************************************************************)
Test[
    ToDataRegion[data, {10, 20, 30}, {0.1, 0.2, 0.3},
    	VariableName -> "test region", Time -> 42]
    ,
    DataRegion[{VariableName -> "test region",
        DataRegion`Private`Origin -> {10., 20., 30.}, Spacing -> {0.1, 0.2, 0.3},
        DataRegion`Private`Time -> 42},
        data]
    ,
    TestID->"ToDataRegion"
]


(****************************************************************)
(* ToListOfData *)
(****************************************************************)
Test[
    ToListOfData[dr, Flatten -> False]
    ,
    data
    ,
    TestID->"ToListOfData"
]





(****************************************************************)
(************************* To be moved **************************)
(****************************************************************)

(****************************************************************)
(* TimeDerivative *)
(****************************************************************)
Test[
    TimeDerivative[{dr, dr2}]
    ,
    DataRegion[{VariableName -> "dt_test region",
      DataRegion`Private`Origin -> {10., 20., 30.},
      Spacing -> {0.1, 0.2, 0.3}, 
      DataRegion`Private`Time -> 89/2},
      {{{111/5, 112/5, 113/5, 114/5}, {121/5, 122/5, 123/5, 124/5}, {131/5, 132/5, 133/5, 134/5}}, 
      {{211/5, 212/5, 213/5, 214/5}, {221/5, 222/5, 223/5, 224/5}, {231/5, 232/5, 233/5, 234/5}}}]
    ,
    TestID->"TimeDerivative"
]


