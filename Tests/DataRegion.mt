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
(* Built-in functions                                           *)
(****************************************************************)
test[f_] :=
 Test[
   {f[data], CoordinateRanges[f[dr]]}
   ,
   {ToListOfData[f[dr]], CoordinateRanges[dr]}
   ,
   TestID -> "Built-in function: "<>ToString[f]
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
   f[dr[[1]]]
   ,
   If[f===ArrayPlot,
   	f[Reverse[Transpose[data[[1]]]], DataRange -> CoordinateRanges[dr[[1]]]]
   ,
   	f[Transpose[data[[1]]], DataRange -> CoordinateRanges[dr[[1]]]]
   ]
   ,
   TestID -> "Built-in function: "<>ToString[f]
 ];

Scan[test4, DataRegion`Private`$2DPlotFunctions]


(****************************************************************)
(* Add                                                          *)
(****************************************************************)

Test[
    Add[dr2, dr4]
    ,
    DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {10., 20., 30.3}, Spacing -> {0.1, 0.2, 0.3}, DataRegion`Private`Time -> Undefined}, {{{335., 338., 341.00000000000006}, {365., 368., 371.00000000000006}, {395., 398., 401.}}, {{635., 638., 641.}, {665., 668., 671.}, {695., 698., 701.}}}]
    ,
    TestID->"Add"
]


(****************************************************************)
(* ArrayDepth                                                   *)
(****************************************************************)
Test[
    ArrayDepth[dr]
    ,
    3
    ,
    TestID->"ArrayDepth"
]


(****************************************************************)
(* Coordinate                                                   *)
(****************************************************************)
Test[
    ToListOfData[Coordinate[dr, 1]][[Sequence @@ #]] & /@ {{1, 1, 1}, {2, 1, 1}, {1, 2, 1}, {1, 1, 2}, {2, 2, 1}, {2, 1, 2}, {1, 2, 2}, {2, 2, 2}}
    ,
    {10.`, 10.1`, 10.`, 10.`, 10.1`, 10.1`, 10.`, 10.1`}
    ,
    TestID->"Coordinate1"
]

Test[
    ToListOfData[Coordinate[dr, 2]][[Sequence @@ #]] & /@ {{1, 1, 1}, {2, 1, 1}, {1, 2, 1}, {1, 1, 2}, {2, 2, 1}, {2, 1, 2}, {1, 2, 2}, {2, 2, 2}}
    ,
    {20.`, 20.`, 20.2`, 20.`, 20.2`, 20.`, 20.2`, 20.2`}
    ,
    TestID->"Coordinate2"
]

Test[
    ToListOfData[Coordinate[dr, 3]][[Sequence @@ #]] & /@ {{1, 1, 1}, {2, 1, 1}, {1, 2, 1}, {1, 1, 2}, {2, 2, 1}, {2, 1, 2}, {1, 2, 2}, {2, 2, 2}}
    ,
    {30.`, 30.`, 30.`, 30.3`, 30.`, 30.3`, 30.3`, 30.3`}
    ,
    TestID->"Coordinate3"
]


(****************************************************************)
(* CoordinateOutline                                            *)
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
(* CoordinateRanges                                             *)
(****************************************************************)
Test[
    CoordinateRanges[dr]
    ,
    {{10.`, 10.1`}, {20.`, 20.4`}, {30.`, 30.9`}}
    ,
    TestID->"CoordinateRanges"
]


(****************************************************************)
(* CoordinateSpacing                                            *)
(****************************************************************)
Test[
    Catch[CoordinateSpacing[dr],
      ErrorString["CoordinateSpacing can only be used with 1-dimensional DataRegions."], f]
    ,
    f[{{}, {}}, ErrorString["CoordinateSpacing can only be used with 1-dimensional DataRegions."]]
    ,
    TestID->"CoordinateSpacing n-dimensional"
]

Test[
    CoordinateSpacing[dr[[1,1]]]
    ,
    0.3
    ,
    TestID->"CoordinateSpacing 1-dimensional"
]


(****************************************************************)
(* CoordinateSpacings                                           *)
(****************************************************************)
Test[
    CoordinateSpacings[dr]
    ,
    {0.1`, 0.2`, 0.3`}
    ,
    TestID->"CoordinateSpacings"
]


(****************************************************************)
(* DataRepresentationQ                                          *)
(****************************************************************)

Test[
    DataRepresentationQ[dr]
    ,
    True
    ,
    TestID->"DataRepresentationQ"
]


(****************************************************************)
(* Dimensions                                                   *)
(****************************************************************)
Test[
    Dimensions[dr]
    ,
    {2,3,4}
    ,
    TestID->"Dimensions"
]


(****************************************************************)
(* Div                                                          *)
(****************************************************************)

Test[
    Div[dr2, dr4]
    ,
    DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {10., 20., 30.3}, Spacing -> {0.1, 0.2, 0.3}, DataRegion`Private`Time -> Undefined}, {{{2.018018018018018, 2.0178571428571432, 2.017699115044248}, {2.0165289256198347, 2.016393442622951, 2.016260162601626}, {2.015267175572519, 2.015151515151515, 2.0150375939849625}}, {{2.009478672985782, 2.009433962264151, 2.0093896713615025}, {2.009049773755656, 2.009009009009009, 2.008968609865471}, {2.0086580086580086, 2.0086206896551726, 2.0085836909871246}}}]
    ,
    TestID->"Div"
]


(****************************************************************)
(* Downsampled                                                  *)
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
(* Drop                                                         *)
(****************************************************************)
Test[
    ToListOfData[Drop[dr, None, 2]]
    ,
    Drop[data, None, 2]
    ,
    TestID->"Drop1"
]

Test[
    Catch[Drop[dr, None, -2], ErrorString["Negative part specifications are not currently supported by DataRegion."]]
    ,
    {{},{}}
    ,
    TestID->"Drop2"
]


(****************************************************************)
(* Endpoints                                                    *)
(****************************************************************)
Test[
    Catch[Endpoints[dr], ErrorString["Endpoints should only be used with 1 dimensional data representations."], f]
    ,
    f[{{}, {}}, ErrorString["Endpoints should only be used with 1 dimensional data representations."]]
    ,
    TestID->"Endpoints n-dimensional"
]

Test[
    Endpoints[dr[[1,1]]]
    ,
    {30., 30.9}
    ,
    TestID->"Endpoints 1-dimensional"
]


(****************************************************************)
(* Extract                                                      *)
(****************************************************************)
Test[
    Extract[dr, {{1, 2, 3}}]
    ,
    Extract[data, {{1, 2, 3}}]
    ,
    TestID->"Extract"
]


(****************************************************************)
(* First                                                        *)
(****************************************************************)

Test[
    First[dr[[1,1]]]
    ,
    111
    ,
    TestID -> "First"
]


(****************************************************************)
(* GridNorm                                                     *)
(****************************************************************)
Test[
    GridNorm[dr]
    ,
    68.22521527998282`
    ,
    TestID->"GridNorm"
]


(****************************************************************)
(* Interpolation                                                *)
(****************************************************************)
Test[
    Quiet[Interpolation[dr][10.05, 20.3, 30.7], ListInterpolation::inhr]
    ,
    178.3333333333343`
    ,
    TestID->"Interpolation"
]


(****************************************************************)
(* Length                                                       *)
(****************************************************************)
Test[
    Length[dr[[1,1]]]
    ,
    4
    ,
    TestID->"Length"
]


(****************************************************************)
(* Map                                                          *)
(****************************************************************)
Test[
    Map[f, dr]
    ,
    DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {10., 20., 30.},
    	Spacing -> {0.1, 0.2, 0.3},
    	DataRegion`Private`Time -> 42},
    	{{{f[111], f[112], f[113], f[114]}, {f[121], f[122], f[123], f[124]}, {f[131], f[132], f[133], f[134]}},
         {{f[211], f[212], f[213], f[214]}, {f[221], f[222], f[223], f[224]}, {f[231], f[232], f[233], f[234]}}}]
    ,
    TestID->"Map"
]


(****************************************************************)
(* MapList                                                      *)
(****************************************************************)
Test[
    MapList[f, dr]
    ,
    DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {10., 20., 30.}, Spacing -> {0.1, 0.2, 0.3}, 
      DataRegion`Private`Time -> 42}, {{{f[{10., 20., 30., 111}], f[{10., 20., 30.3, 112}], f[{10., 20., 30.6, 113}], 
        f[{10., 20., 30.9, 114}]}, {f[{10., 20.2, 30., 121}], f[{10., 20.2, 30.3, 122}], f[{10., 20.2, 30.6, 123}], 
        f[{10., 20.2, 30.9, 124}]}, {f[{10., 20.4, 30., 131}], f[{10., 20.4, 30.3, 132}], f[{10., 20.4, 30.6, 133}], 
        f[{10., 20.4, 30.9, 134}]}}, {{f[{10.1, 20., 30., 211}], f[{10.1, 20., 30.3, 212}], f[{10.1, 20., 30.6, 213}], 
        f[{10.1, 20., 30.9, 214}]}, {f[{10.1, 20.2, 30., 221}], f[{10.1, 20.2, 30.3, 222}], f[{10.1, 20.2, 30.6, 223}], 
        f[{10.1, 20.2, 30.9, 224}]}, {f[{10.1, 20.4, 30., 231}], f[{10.1, 20.4, 30.3, 232}], f[{10.1, 20.4, 30.6, 233}], 
        f[{10.1, 20.4, 30.9, 234}]}}}]
    ,
    TestID->"MapList"
]


(****************************************************************)
(* MapThread                                                    *)
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
(* MaxCoordinate                                                *)
(****************************************************************)
Test[
    MaxCoordinate[dr[[1,1]]]
    ,
    30.9`
    ,
    TestID->"MaxCoordinate"
]


(****************************************************************)
(* MaxCoordinates                                               *)
(****************************************************************)
Test[
    MaxCoordinates[dr]
    ,
    {10.1`, 20.4`, 30.9`}
    ,
    TestID->"MaxCoordinates"
]


(****************************************************************)
(* MinCoordinate                                                *)
(****************************************************************)
Test[
    MinCoordinate[dr[[1,1]]]
    ,
    30.`
    ,
    TestID->"MinCoordinate"
]


(****************************************************************)
(* MinCoordinates                                               *)
(****************************************************************)
Test[
    MinCoordinates[dr]
    ,
    {10.`, 20.`, 30.`}
    ,
    TestID->"MinCoordinates"
]


(****************************************************************)
(* Mul                                                          *)
(****************************************************************)

Test[
    Mul[dr2, dr4]
    ,
    DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {10., 20., 30.3}, Spacing -> {0.1, 0.2, 0.3}, DataRegion`Private`Time -> Undefined}, {{{24864., 25312.000000000004, 25764.000000000007}, {29524., 30012.000000000004, 30504.000000000007}, {34584., 35112., 35644.}}, {{89464., 90312., 91164.}, {98124., 99012., 99904.}, {107184., 108112., 109044.}}}]
    ,
    TestID->"Mul"
]


(****************************************************************)
(* NDerivative                                                  *)
(****************************************************************)
Test[
    Quiet[DeleteDuplicates[Flatten[ToListOfData[NDerivative[dr, 3]]]], NDSolve`FiniteDifferenceDerivative::ordred]
    ,
    {3.3333333333333854, 3.333333333333364, 3.3333333333333286, 3.3333333333333712, 3.333333333333428, 3.3333333333333997, 3.3333333333332575, 3.3333333333333144, 3.333333333333485, 3.333333333333286, 3.33333333333303}
    ,
    TestID->"NDerivative1"
]

Test[
    Quiet[DeleteDuplicates[Flatten[ToListOfData[NDerivative[dr, 2]]]], NDSolve`FiniteDifferenceDerivative::ordred]
    ,
    {50.00000000000023, 50.00000000000017, 50.00000000000006, 50.000000000000114, 50., 50.000000000000455, 49.99999999999977}
    ,
    TestID->"NDerivative2"
]

Test[
    Quiet[DeleteDuplicates[Flatten[ToListOfData[NDerivative[dr, 1]]]], NDSolve`FiniteDifferenceDerivative::ordred]
    ,
    {1000.0000000000034, 1000.0000000000036, 1000.0000000000039}
    ,
    TestID->"NDerivative3"
]


(****************************************************************)
(* Part                                                         *)
(****************************************************************)
parts = {
  2,
  All,
  1 ;; 2,
  {1 ;; 2, All, {2}},
  {1 ;; 2, 1 ;;, 2},
  {1 ;; 2, ;; , 2},
  {1 ;; 2, ;; 2, 2}(*,
  1 ;; -1 ;; 2 ,
  -1,
  {-1}*)
};

partTest[p_] :=
 Test[
    ToListOfData[dr[[p /. List -> Sequence]]]
    ,
    data[[p /. List -> Sequence]]
    ,
    TestID->"Part "<>ToString[p]
 ];

Scan[partTest, parts];

(****************************************************************)
(* Position                                                     *)
(****************************************************************)
Test[
    Position[dr, 123]
    ,
    Position[data, 123]
    ,
    TestID->"Position"
]


(****************************************************************)
(* Resampled                                                    *)
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
    TestID->"Resampled list of DataRegions"
]


(****************************************************************)
(* SameGridQ                                                    *)
(****************************************************************)
Test[
    SameGridQ[dr, dr3]
    ,
    False
    ,
    TestID->"SameGridQ False"
]

Test[
    SameGridQ[dr, dr2]
    ,
    True
    ,
    TestID->"SameGridQ True"
]


Test[
    SameGridQ[dr, dr2, dr3]
    ,
    False
    ,
    TestID->"SameGridQ multiple False"
]

Test[
    SameGridQ[dr, dr2, dr2]
    ,
    True
    ,
    TestID->"SameGridQ multiple True"
]


(****************************************************************)
(* Shifted                                                      *)
(****************************************************************)
Test[
    MinCoordinates[Shifted[dr, {1,2,3}]]
    ,
    {11., 22., 33.}
    ,
    TestID->"Shifted"
]


(****************************************************************)
(* Slab                                                         *)
(****************************************************************)
slabs = {
  10.1,
  All,
  10.0 ;; 10.1,
  {10.0 ;; 10.1, All, {30.3}},
  {10.0 ;; 10.1, 20.0 ;;, 30.3}(*,
  Sequence[10.0 ;; 10.1, ;; , 30.3],
  Sequence[10.0 ;; 10.1, ;; 20.2, 30.3]*)
};

slabTest[p_, s] :=
 Test[
    Slab[dr, s /. List -> Sequence]
    ,
    dr[[p /. List -> Sequence]]
    ,
    TestID->"Slab "<>ToString[s]
 ];

Scan[slabTest, Transpose[{parts[[1;;5]], slabs}]];


(****************************************************************)
(* Sub                                                          *)
(****************************************************************)

Test[
    Sub[dr2, dr4]
    ,
    DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {10., 20., 30.3}, Spacing -> {0.1, 0.2, 0.3}, DataRegion`Private`Time -> Undefined}, {{{113., 114.00000000000003, 115.00000000000001}, {123., 124.00000000000003, 125.00000000000001}, {133., 134., 135.}}, {{213., 214., 215.}, {223., 224., 225.}, {233., 234., 235.}}}]
    ,
    TestID->"Sub"
]


(****************************************************************)
(* Take                                                         *)
(****************************************************************)
Test[
    ToListOfData[Take[dr, 2]]
    ,
    Take[data, 2]
    ,
    TestID->"Take"
]

Test[
    ToListOfData[Take[dr, All, 3]]
    ,
    Take[data, All, 3]
    ,
    TestID->"Take2"
]

Test[
    ToListOfData[Take[dr, All, {3}]]
    ,
    Take[data, All, {3}]
    ,
    TestID->"Take3"
]

Test[
    ToListOfData[Take[dr, All, {2, 3}]]
    ,
    Take[data, All, {2, 3}]
    ,
    TestID->"Take4"
]

Test[
    ToListOfData[Take[dr, All, {1, 3, 2}]]
    ,
    Take[data, All, {1, 3, 2}]
    ,
    TestID->"Take5"
]


(****************************************************************)
(* ToDataRegion                                                 *)
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

Test[
    ToDataRegion[{dr, dr3}]
    ,
    DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {9.9, 20., 30.}, Spacing -> {0.1, 0.2, 0.3}, DataRegion`Private`Time -> Undefined},
     {{{222, 224, 226, 228}, {242, 244, 246, 248}, {262, 264, 266, 268}},
      {{422, 424, 426, 428}, {442, 444, 446, 448}, {462, 464, 466, 468}},
      {{211, 212, 213, 214}, {221, 222, 223, 224}, {231, 232, 233, 234}}}]
    ,
    TestID->"ToDataRegion from List of DataRegions"
]


(****************************************************************)
(* ToList                                                       *)
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
(* ToListOfCoordinates                                          *)
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
(* ToListOfData                                                 *)
(****************************************************************)
Test[
    ToListOfData[dr]
    ,
    data
    ,
    TestID->"ToListOfData"
]


(****************************************************************)
(* VariableName                                                 *)
(****************************************************************)
Test[
    VariableName[dr]
    ,
    "test region"
    ,
    TestID->"VariableName"
]





(****************************************************************)
(************************ Experimental **************************)
(****************************************************************)

(****************************************************************)
(* TimeDerivative                                               *)
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


