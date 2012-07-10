(* Mathematica Test File *)

data = {{{111, 112, 113, 114}, {121, 122, 123, 124}, {131, 132, 133, 134}},
	    {{211, 212, 213, 214}, {221, 222, 223, 224}, {231, 232, 233, 234}}};

data2 = 2 * {{{111, 112, 113, 114}, {121, 122, 123, 124}, {131, 132, 133, 134}},
	     {{211, 212, 213, 214}, {221, 222, 223, 224}, {231, 232, 233, 234}}};

dataregion = ToDataRegion[data, {10, 20, 30}, {0.1, 0.2, 0.3},
        VariableName -> "test region", Time -> 42];

dataregion2 = ToDataRegion[data2, {10, 20, 30}, {0.1, 0.2, 0.3},
        VariableName -> "test region", Time -> 47];

dataregion3 = ToDataRegion[data2, {9.9, 20, 30}, {0.1, 0.2, 0.3}, 
        VariableName -> "test region", Time -> 47];

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
    ToListOfData[dataregion, Flatten -> False]
    ,
    data
    ,
    TestID->"ToListOfData"
]


(****************************************************************)
(************************** DEPRECATED **************************)
(****************************************************************)


(****************************************************************)
(* DataRegionPart *)
(****************************************************************)
Test[
    DataRegionPart[dataregion, {10 ;; 10.2, All, 30 ;; 30.9}]
    ,
    DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {10., 20., 30.}, Spacing -> {0.1, 0.2, 0.3}, DataRegion`Private`Time -> 42},
      {{{111, 112, 113}, {121, 122, 123}, {131, 132, 133}}, {{211, 212, 213}, {221, 222, 223}, {231, 232, 233}}}]
    ,
    TestID->"DataRegionPart"
]


(****************************************************************)
(* EvaluateOnDataRegion *)
(****************************************************************)
Test[
    EvaluateOnDataRegion[x + y + z + t, {t, x, y, z}, dataregion]
    ,
    DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {10., 20., 30.}, Spacing -> {0.1, 0.2, 0.3}, DataRegion`Private`Time -> 42}, 
      {{{102., 102.1, 102.2, 102.3}, {102.2, 102.3, 102.4, 102.5}, {102.4, 102.5, 102.6, 102.7}}, {{102.3, 102.4, 102.5, 102.6}, {102.5, 102.6, 102.7, 102.8}, {102.7, 102.8, 102.9, 103.}}}]
    ,
    TestID->"EvaluateOnDataRegion"
]


(****************************************************************)
(* FilterNaNs *)
(****************************************************************)
Test[
    FilterNaNs[ToDataRegion[{-2147483648}, {10}, {0.1}, VariableName -> "test region", Time -> 42]]
    ,
    DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {10.}, Spacing -> {0.1}, DataRegion`Private`Time -> 42}, {Missing[]}]
    ,
    TestID->"FilterNaNs"
]


(****************************************************************)
(* GetAttributes *)
(****************************************************************)
Test[
    GetAttributes[dataregion]
    ,
    {VariableName -> "test region", DataRegion`Private`Origin -> {10., 20., 30.}, Spacing -> {0.1, 0.2, 0.3}, DataRegion`Private`Time -> 42}
    ,
    TestID->"GetAttributes"
]


(****************************************************************)
(* GetCoordinate *)
(****************************************************************)
Test[
    GetCoordinate[dataregion, 3]
    ,
    DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {10., 20., 30.}, Spacing -> {0.1, 0.2, 0.3}, DataRegion`Private`Time -> 42}, 
      {{{30., 30., 30., 30.}, {30., 30., 30., 30.}, {30., 30., 30., 30.}}, {{30.3, 30.3, 30.3, 30.3}, {30.3, 30.3, 30.3, 30.3}, {30.3, 30.3, 30.3, 30.3}}}]
    ,
    TestID->"GetCoordinate"
]


(****************************************************************)
(* GetData *)
(****************************************************************)
Test[
    GetData[dataregion]
    ,
    data
    ,
    TestID->"GetData"
]


(****************************************************************)
(* GetDataRange *)
(****************************************************************)
Test[
    GetDataRange[dataregion]
    ,
    {{10., 10.3}, {20., 20.4}, {30., 30.3}}
    ,
    TestID->"GetDataRange"
]


(****************************************************************)
(* GetDimensions *)
(****************************************************************)
Test[
    GetDimensions[dataregion]
    ,
    {4, 3, 2}
    ,
    TestID->"GetDimensions"
]


(****************************************************************)
(* GetNumDimensions *)
(****************************************************************)
Test[
    GetNumDimensions[dataregion]
    ,
    3
    ,
    TestID->"GetNumDimensions"
]


(****************************************************************)
(* GetOrigin *)
(****************************************************************)
Test[
    GetOrigin[dataregion]
    ,
    {10., 20., 30.}
    ,
    TestID->"GetOrigin"
]


(****************************************************************)
(* GetSpacing *)
(****************************************************************)
Test[
    GetSpacing[dataregion]
    ,
    {0.1, 0.2, 0.3}
    ,
    TestID->"GetSpacing"
]


(****************************************************************)
(* GetTime *)
(****************************************************************)
Test[
    GetTime[dataregion]
    ,
    42
    ,
    TestID->"GetTime"
]


(****************************************************************)
(* GetVariableName *)
(****************************************************************)
Test[
    GetVariableName[dataregion]
    ,
    "test region"
    ,
    TestID->"GetVariableName"
]


(****************************************************************)
(* MakeDataRegion *)
(****************************************************************)
Test[
    MakeDataRegion[data, "test region", {4, 3, 2}, {10, 20, 30}, {0.1, 0.2, 0.3}, 42]
    ,
    DataRegion[{VariableName -> "test region",
    	DataRegion`Private`Origin -> {10, 20, 30}, Spacing -> {0.1, 0.2, 0.3}, 
        DataRegion`Private`Time -> 42},
        data]
    ,
    TestID->"MakeDataRegion"
]


(****************************************************************)
(* MapDataRegion *)
(****************************************************************)
Test[
    MapDataRegion[2 # &, dataregion]
    ,
    DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {10., 20., 30.}, Spacing -> {0.1, 0.2, 0.3}, DataRegion`Private`Time -> 42}, 
      {{{222, 224, 226, 228}, {242, 244, 246, 248}, {262, 264, 266, 268}}, {{422, 424, 426, 428}, {442, 444, 446, 448}, {462, 464, 466, 468}}}]
    ,
    TestID->"MapDataRegion"
]


(****************************************************************)
(* MapThreadDataRegion *)
(****************************************************************)
Test[
    MapThreadDataRegion[2 #1 - #2 &, {dataregion, dataregion2}]
    ,
    DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {10., 20., 30.}, Spacing -> {0.1, 0.2, 0.3}, DataRegion`Private`Time -> 42}, 
      {{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}}]
    ,
    TestID->"MapThreadDataRegion"
]


(****************************************************************)
(* MergeDataRegions *)
(****************************************************************)
Test[
    MergeDataRegions[{dataregion, dataregion3}]
    ,
    DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {9.9, 20, 30}, Spacing -> {0.1, 0.2, 0.3}, DataRegion`Private`Time -> 42}, 
      {{{222, 224, 226, 228, 114}, {242, 244, 246, 248, 124}, {262, 264, 266, 268, 134}}, {{422, 424, 426, 428, 214}, {442, 444, 446, 448, 224}, {462, 464, 466, 468, 234}}}]
    ,
    TestID->"MergeDataRegions"
]


(****************************************************************)
(* NaNQ *)
(****************************************************************)
Test[
    NaNQ[-2147483648]
    ,
    True
    ,
    TestID->"NaNQ"
]


(****************************************************************)
(* NormL2 *)
(****************************************************************)
Test[
    NormL2[dataregion]
    ,
    68.22521527998282
    ,
    TestID->"NormL2"
]


(****************************************************************)
(* Outline *)
(****************************************************************)
Test[
    Outline[dataregion]
    ,
    Cuboid[{10., 20., 30.}, {10.3, 20.4, 30.3}]
    ,
    TestID->"Outline"
]


(****************************************************************)
(* ResampleDataRegion *)
(****************************************************************)
Test[
    Quiet[ResampleDataRegion[SliceData[dataregion, 2, 20], {{10.1, 30}, {10.3, 30.2}, {0.05, 0.15}}, 2], ListInterpolation::inhr]
    ,
    DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {10.1, 30}, Spacing -> {0.05, 0.15}, DataRegion`Private`Time -> 42}, 
      {{112.00000000000001, 161.9999999999994}, {112.50000000000001, 162.4999999999994}, {113., 162.99999999999937}, {113.5, 163.4999999999994}, {114., 163.99999999999937}}]
    ,
    TestID->"ResampleDataRegion"
]


(****************************************************************)
(* ResampleDataRegions *)
(****************************************************************)
Test[
    Quiet[ResampleDataRegions[{SliceData[dataregion, 2, 20], SliceData[dataregion3, 2, 20]}], ListInterpolation::inhr]
    ,
    {DataRegion[{VariableName -> "test region",
       DataRegion`Private`Origin -> {10, 30},
       Spacing -> {0.1, 0.3},
       DataRegion`Private`Time -> 42}, 
       {{111.00000000000001, 211.}, {112.00000000000001, 212.}, {113., 212.99999999999997}}], 
     DataRegion[{VariableName -> "test region",
       DataRegion`Private`Origin -> {10, 30},
       Spacing -> {0.1, 0.3},
       DataRegion`Private`Time -> 47}, 
       {{224.00000000000003, 424.}, {226., 425.99999999999994}, {228., 427.99999999999994}}]}
    ,
    TestID->"ResampleDataRegions"
]


(****************************************************************)
(* SliceData *)
(****************************************************************)
Test[
    SliceData[dataregion, 1, 10.1]
    ,
    DataRegion[{VariableName -> "test region",
    	DataRegion`Private`Origin -> {20., 30.}, Spacing -> {0.2, 0.3},
    	 DataRegion`Private`Time -> 42},
    	 {{112, 122, 132}, {212, 222, 232}}]
    ,
    TestID->"SliceData"
]


(****************************************************************)
(* Strip *)
(****************************************************************)
Test[
    Strip[dataregion, {1, 0, 0}]
    ,
    DataRegion[{VariableName -> "test region",
      DataRegion`Private`Origin -> {10.1, 20., 30.},
      Spacing -> {0.1, 0.2, 0.3}, 
      DataRegion`Private`Time -> 42},
    {{{112, 113}, {122, 123}, {132, 133}}, {{212, 213}, {222, 223}, {232, 233}}}]
    ,
    TestID->"Strip"
]


(****************************************************************)
(* TimeDerivative *)
(****************************************************************)
Test[
    TimeDerivative[{dataregion, dataregion2}]
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


(****************************************************************)
(* ToDataRegion *)
(****************************************************************)
Test[
    TableToDataRegion[{{10, 20, 1}, {12, 20, 2}, {10, 23, 3}, {12, 23, 4}}]
    ,
    DataRegion[{
      VariableName -> "table",
      DataRegion`Private`Origin -> {10, 20},
      Spacing -> {2, 3},
      DataRegion`Private`Time -> 0},
      {{1, 2}, {3, 4}}]
    ,
    TestID->"TableToDataRegion"
]
