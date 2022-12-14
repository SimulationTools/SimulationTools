Get[FileNameJoin[{$SimulationToolsInstallationDirectory,"Tests","Common.m"}]];

(* Mathematica Test File *)

(****************************************************************)
(************************** DEPRECATED **************************)
(****************************************************************)
data = {{{111, 112, 113, 114}, {121, 122, 123, 124}, {131, 132, 133, 134}},
	    {{211, 212, 213, 214}, {221, 222, 223, 224}, {231, 232, 233, 234}}};

data2 = 2 * {{{111, 112, 113, 114}, {121, 122, 123, 124}, {131, 132, 133, 134}},
	     {{211, 212, 213, 214}, {221, 222, 223, 224}, {231, 232, 233, 234}}};

dataregion  = MakeDataRegion[data, "test region", Reverse[Dimensions[data]], {10, 20, 30}, {0.1, 0.2, 0.3}, 42];
dataregion2 = MakeDataRegion[data2, "test region", Reverse[Dimensions[data]], {10, 20, 30}, {0.1, 0.2, 0.3}, 47];
dataregion3 = MakeDataRegion[data2, "test region", Reverse[Dimensions[data]], {9.9, 20, 30}, {0.1, 0.2, 0.3}, 47];

(****************************************************************)
(* DataRegionPart *)
(****************************************************************)
VerificationTest[
    DataRegionPart[dataregion, {10 ;; 10.2, All, 30 ;; 30.9}]
    ,
    DataRegion[{"VariableName" -> "test region", "Origin" -> {10., 20., 30.}, "Spacing" -> {0.1, 0.2, 0.3}, "Time" -> 42},
      {{{111, 211}, {121, 221}, {131, 231}}, {{112, 212}, {122, 222}, {132, 232}}, {{113, 213}, {123, 223}, {133, 233}}}]
    ,
    TestID->"DataRegionPart"
]


(****************************************************************)
(* EvaluateOnDataRegion *)
(****************************************************************)
VerificationTest[
    EvaluateOnDataRegion[x + y + z + t, {t, x, y, z}, dataregion]
    ,
    DataRegion[{"VariableName" -> "test region", "Origin" -> {10, 20, 30}, "Spacing" -> {0.1, 0.2, 0.3}, "Time" -> 42}, 
      {{{102., 102.3}, {102.2, 102.5}, {102.4, 102.7}}, {{102.1, 102.4}, {102.3, 102.6}, {102.5, 102.8}},
       {{102.2, 102.5}, {102.4, 102.7}, {102.6, 102.9}}, {{102.3, 102.6}, {102.5, 102.8}, {102.7, 103.}}}]
    ,
    TestID->"EvaluateOnDataRegion"
]


(****************************************************************)
(* GetAttributes *)
(****************************************************************)
VerificationTest[
    GetAttributes[dataregion]
    ,
    {"VariableName" -> "test region", "Origin" -> {10, 20, 30}, "Spacing" -> {0.1, 0.2, 0.3}, "Time" -> 42}
    ,
    TestID->"GetAttributes"
]


(****************************************************************)
(* GetCoordinate *)
(****************************************************************)
VerificationTest[
    GetCoordinate[dataregion, 3]
    ,
    DataRegion[{"VariableName" -> "test region", "Origin" -> {10, 20, 30}, "Spacing" -> {0.1, 0.2, 0.3}, "Time" -> 42}, 
      {{{30., 30.3}, {30., 30.3}, {30., 30.3}}, {{30., 30.3}, {30., 30.3}, {30., 30.3}}, {{30., 30.3}, {30., 30.3}, {30., 30.3}}, {{30., 30.3}, {30., 30.3}, {30., 30.3}}}]
    ,
    TestID->"GetCoordinate"
]


(****************************************************************)
(* GetData *)
(****************************************************************)
VerificationTest[
    GetData[dataregion]
    ,
    data
    ,
    TestID->"GetData"
]


(****************************************************************)
(* GetDataRange *)
(****************************************************************)
VerificationTest[
    GetDataRange[dataregion]
    ,
    {{10, 10.3}, {20, 20.4}, {30, 30.3}}
    ,
    TestID->"GetDataRange"
]


(****************************************************************)
(* GetDimensions *)
(****************************************************************)
VerificationTest[
    GetDimensions[dataregion]
    ,
    {4, 3, 2}
    ,
    TestID->"GetDimensions"
]


(****************************************************************)
(* GetNumDimensions *)
(****************************************************************)
VerificationTest[
    GetNumDimensions[dataregion]
    ,
    3
    ,
    TestID->"GetNumDimensions"
]


(****************************************************************)
(* GetOrigin *)
(****************************************************************)
VerificationTest[
    GetOrigin[dataregion]
    ,
    {10, 20, 30}
    ,
    TestID->"GetOrigin"
]


(****************************************************************)
(* GetSpacing *)
(****************************************************************)
VerificationTest[
    GetSpacing[dataregion]
    ,
    {0.1, 0.2, 0.3}
    ,
    TestID->"GetSpacing"
]


(****************************************************************)
(* GetTime *)
(****************************************************************)
VerificationTest[
    GetTime[dataregion]
    ,
    42
    ,
    TestID->"GetTime"
]


(****************************************************************)
(* GetVariableName *)
(****************************************************************)
VerificationTest[
    GetVariableName[dataregion]
    ,
    "test region"
    ,
    TestID->"GetVariableName"
]


(****************************************************************)
(* MakeDataRegion *)
(****************************************************************)
VerificationTest[
    MakeDataRegion[data, "test region", {4, 3, 2}, {10, 20, 30}, {0.1, 0.2, 0.3}, 42]
    ,
    DataRegion[{"VariableName" -> "test region",
    	"Origin" -> {10, 20, 30}, "Spacing" -> {0.1, 0.2, 0.3}, 
        "Time" -> 42},
        Transpose[data, Reverse[Range[ArrayDepth[data]]]]]
    ,
    TestID->"MakeDataRegion"
]


(****************************************************************)
(* MapDataRegion *)
(****************************************************************)
VerificationTest[
    MapDataRegion[2 # &, dataregion]
    ,
    DataRegion[{"VariableName" -> "test region", "Origin" -> {10, 20, 30}, "Spacing" -> {0.1, 0.2, 0.3}, "Time" -> 42}, 
      {{{222, 422}, {242, 442}, {262, 462}}, {{224, 424}, {244, 444}, {264, 464}}, {{226, 426}, {246, 446}, {266, 466}}, {{228, 428}, {248, 448}, {268, 468}}}]
    ,
    TestID->"MapDataRegion"
]


(****************************************************************)
(* MapThreadDataRegion *)
(****************************************************************)
VerificationTest[
    MapThreadDataRegion[2 #1 - #2 &, {dataregion, dataregion2}]
    ,
    DataRegion[{"VariableName" -> "test region", "Origin" -> {10, 20, 30}, "Spacing" -> {0.1, 0.2, 0.3}, "Time" -> 42}, 
      {{{0, 0}, {0, 0}, {0, 0}}, {{0, 0}, {0, 0}, {0, 0}}, {{0, 0}, {0, 0}, {0, 0}}, {{0, 0}, {0, 0}, {0, 0}}}]
    ,
    TestID->"MapThreadDataRegion"
]


(****************************************************************)
(* MergeDataRegions *)
(****************************************************************)
VerificationTest[
    MergeDataRegions[{dataregion, dataregion3}]
    ,
    DataRegion[{"VariableName" -> "test region", "Origin" -> {9.9, 20, 30}, "Spacing" -> {0.1, 0.2, 0.3}, "Time" -> 42}, 
      {{{222, 422}, {242, 442}, {262, 462}}, {{224, 424}, {244, 444}, {264, 464}}, {{226, 426}, {246, 446}, {266, 466}}, {{228, 428}, {248, 448}, {268, 468}}, {{114, 214}, {124, 224}, {134, 234}}}]
    ,
    TestID->"MergeDataRegions"
]


(****************************************************************)
(* NormL2 *)
(****************************************************************)
VerificationTest[
    NormL2[dataregion]
    ,
    68.22521527998282
    ,
    TestID->"NormL2"
]


(****************************************************************)
(* Outline *)
(****************************************************************)
VerificationTest[
    Outline[dataregion]
    ,
    Cuboid[{10, 20, 30}, {10.3, 20.4, 30.3}]
    ,
    TestID->"Outline"
]


(****************************************************************)
(* ResampleDataRegion *)
(****************************************************************)
VerificationTest[
    Quiet[ResampleDataRegion[SliceData[dataregion, 2, 20], {{10.1, 30}, {10.3, 30.2}, {0.05, 0.15}}, 2], ListInterpolation::inhr]
    ,
    DataRegion[{"VariableName" -> "test region", "Origin" -> {10.1, 30}, "Spacing" -> {0.05, 0.15}, "Time" -> 42}, 
      {{112.00000000000001, 112.50000000000001, 113., 113.5, 114.},
       {161.9999999999994, 162.4999999999994, 162.99999999999937, 163.4999999999994, 163.99999999999937}}]
    ,
    TestID->"ResampleDataRegion"
]


(****************************************************************)
(* ResampleDataRegions *)
(****************************************************************)
VerificationTest[
    Quiet[ResampleDataRegions[{SliceData[dataregion, 2, 20], SliceData[dataregion3, 2, 20]}], ListInterpolation::inhr]
    ,
    {DataRegion[{"VariableName" -> "test region",
       "Origin" -> {10, 30},
       "Spacing" -> {0.1, 0.3},
       "Time" -> 42}, 
       {{111., 112., 113.}, {211., 212., 213.}}],
     DataRegion[{"VariableName" -> "test region",
       "Origin" -> {10, 30},
       "Spacing" -> {0.1, 0.3},
       "Time" -> 47}, 
       {{224.00000000000003, 226., 228.}, {424., 425.99999999999994, 427.99999999999994}}]}
    ,
    TestID->"ResampleDataRegions"
]


(****************************************************************)
(* SliceData *)
(****************************************************************)
VerificationTest[
    SliceData[dataregion, 1, 10.1]
    ,
    DataRegion[{"VariableName" -> "test region",
    	"Origin" -> {20, 30}, "Spacing" -> {0.2, 0.3},
    	 "Time" -> 42},
    	 {{112, 212}, {122, 222}, {132, 232}}]
    ,
    TestID->"SliceData"
]


(****************************************************************)
(* Strip *)
(****************************************************************)
VerificationTest[
    Strip[dataregion, {1, 0, 0}]
    ,
    DataRegion[{"VariableName" -> "test region",
      "Origin" -> {10.1, 20., 30.},
      "Spacing" -> {0.1, 0.2, 0.3}, 
      "Time" -> 42},
    {{{112, 212}, {122, 222}, {132, 232}}, {{113, 213}, {123, 223}, {133, 233}}}]
    ,
    TestID->"Strip"
]


(****************************************************************)
(* TableToDataRegion *)
(****************************************************************)
VerificationTest[
    TableToDataRegion[{{10, 20, 1}, {12, 20, 2}, {10, 23, 3}, {12, 23, 4}}]
    ,
    DataRegion[{
      "VariableName" -> "table",
      "Origin" -> {10, 20},
      "Spacing" -> {2, 3},
      "Time" -> 0},
      {{1, 3}, {2, 4}}]
    ,
    TestID->"TableToDataRegion"
]