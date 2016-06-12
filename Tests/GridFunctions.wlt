Get[FileNameJoin[{$SimulationToolsInstallationDirectory,"Tests","Common.m"}]];

(* Mathematica Test File *)

$SimulationPath = {$SimulationToolsTestSimulationDirectory};
TestReferenceDirectory = FileNameJoin[{$SimulationToolsInstallationDirectory,"Data/TestReference"}];

$SimulationToolsMPTestSimulation = "test7patch";

var = "phi";

dims1D = {"x"};
dims2D = {"x","y"};
dims3D = {"x","y","z"};

(* ReadGridFunction (pre 1.0) *)

Module[{data},
    VerificationTest[
        data = GetData[Block[{$SimulationToolsCompatibilityVersion = 0}, ReadGridFunction[$SimulationToolsTestSimulation, var, {"x"},Iteration -> 256]]];
        Part[data, {36, 59, 35, 35, 53, 13, 22, 57, 14, 58}]
    ,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadGridFunction-1D.m"}]]
    ,
        TestID->"ReadGridFunction-1D"
    ]
]

Module[{data},
    VerificationTest[
        data = GetData[Block[{$SimulationToolsCompatibilityVersion = 0}, ReadGridFunction[$SimulationToolsTestSimulation, var, {"x","y"}, Iteration -> 1024]]];
        Map[Part[data, Sequence @@ #] &,
            {{16, 39}, {4, 17}, {28, 58}, {96, 12}, {32, 38},
             {96, 52}, {27, 47}, {114, 5}, {105, 34}, {14, 5}}
            ]
    ,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadGridFunction-2D.m"}]]
    ,
        TestID->"ReadGridFunction-2D"
    ]
]

Module[{data},
	VerificationTest[
        data = GetData[Block[{$SimulationToolsCompatibilityVersion = 0}, ReadGridFunction[$SimulationToolsTestSimulation, var, {"x","y","z"}]]];
        Map[Part[data, Sequence @@ #] &,
            {{49, 1, 4}, {1, 59, 41}, {15, 36, 52}, {14, 20, 38}, {41, 71, 2},
             {18, 1, 35}, {8, 38, 53}, {25, 21, 37}, {51, 80, 44}, {44, 113, 37}}
            ]
    ,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadGridFunction-3D.m"}]]
    ,
        TestID->"ReadGridFunction-3D"
	]
]


(* ReadIterations *)

VerificationTest[
    ReadIterations[$SimulationToolsTestSimulation, var, {"x"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-1D.m"}]]
,
    TestID->"ReadIterations-1D"
]

VerificationTest[
    ReadIterations[$SimulationToolsTestSimulation, var, {"x","y"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-2D.m"}]]
,
    TestID->"ReadIterations-2D"
]

VerificationTest[
    ReadIterations[$SimulationToolsTestSimulation, var, {"x","y","z"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-3D.m"}]]
,
    TestID->"ReadIterations-3D"
]

(* Dimensions specifications *)

VerificationTest[
    ReadIterations[$SimulationToolsTestSimulation, var, {1}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-1D.m"}]]
,
    TestID->"ReadIterations-dims-1D"
]

VerificationTest[
    ReadIterations[$SimulationToolsTestSimulation, var, {1,2}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-2D.m"}]]
,
    TestID->"ReadIterations-dims-2D"
]

VerificationTest[
    ReadIterations[$SimulationToolsTestSimulation, var, {1,2,3}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-3D.m"}]]
,
    TestID->"ReadIterations-dims-3D"
]

VerificationTest[
    ReadIterations[$SimulationToolsTestSimulation, var, "x"]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-1D.m"}]]
,
    TestID->"ReadIterations-dimsstrings-1D"
]

VerificationTest[
    ReadIterations[$SimulationToolsTestSimulation, var, "xy"]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-2D.m"}]]
,
    TestID->"ReadIterations-dimsstrings-2D"
]

VerificationTest[
    ReadIterations[$SimulationToolsTestSimulation, var, "xyz"]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-3D.m"}]]
,
    TestID->"ReadIterations-dimsstrings-3D"
]


(* ReadMaps *)

VerificationTest[
    ReadMaps[$SimulationToolsTestSimulation, var, {"x"}]
,
    {None}
,
    TestID->"ReadMaps-1D"
]

VerificationTest[
    ReadMaps[$SimulationToolsTestSimulation, var, {"x","y"}]
,
    {None}
,
    TestID->"ReadMaps-2D"
]

VerificationTest[
    ReadMaps[$SimulationToolsTestSimulation, var, {"x","y","z"}]
,
    {None}
,
    TestID->"ReadMaps-3D"
]

(* ReadRefinementLevels *)

VerificationTest[
    ReadRefinementLevels[$SimulationToolsTestSimulation, var, {"x"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadRefinementLevels-1D.m"}]]
,
    TestID->"ReadRefinementLevels-1D"
]

VerificationTest[
    ReadRefinementLevels[$SimulationToolsTestSimulation, var, {"x","y"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadRefinementLevels-2D.m"}]]
,
    TestID->"ReadRefinementLevels-2D"
]

VerificationTest[
    ReadRefinementLevels[$SimulationToolsTestSimulation, var, {"x","y","z"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadRefinementLevels-3D.m"}]]
,
    TestID->"ReadRefinementLevels-3D"
]


(* ReadTime *)

VerificationTest[
    ReadTime[$SimulationToolsTestSimulation, var, {"x"}, Iteration -> 256, RefinementLevel -> 5]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTime-1D.m"}]]
,
    TestID->"ReadTime-1D"
]

VerificationTest[
    ReadTime[$SimulationToolsTestSimulation, var, {"x","y"}, Iteration -> 1024, RefinementLevel -> 2]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTime-2D.m"}]]
,
    TestID->"ReadTime-2D"
]

VerificationTest[
    ReadTime[$SimulationToolsTestSimulation, var, {"x","y","z"}, Iteration -> 8192, RefinementLevel -> 0]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTime-3D.m"}]]
,
    TestID->"ReadTime-3D"
]

(* ReadTimeLevels *)

VerificationTest[
    ReadTimeLevels[$SimulationToolsTestSimulation, var, {"x"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTimeLevels-1D.m"}]]
,
    TestID->"ReadTimeLevels-1D"
]

VerificationTest[
    ReadTimeLevels[$SimulationToolsTestSimulation, var, {"x","y"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTimeLevels-2D.m"}]]
,
    TestID->"ReadTimeLevels-2D"
]

VerificationTest[
    ReadTimeLevels[$SimulationToolsTestSimulation, var, {"x","y","z"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTimeLevels-3D.m"}]]
,
    TestID->"ReadTimeLevels-3D"
]

VerificationTest[
    FindGridFunctions[$SimulationToolsTestSimulation]
,
    {"phi"}
,
    TestID->"FindGridFunctions"
]

VerificationTest[
    ReadGridFunctionDimensions[$SimulationToolsTestSimulation, "phi"]
,
    {"x", "xy", "xyz"}
,
    TestID->"ReadGridFunctionDimensions"
]

(****************************************************************)
(* Multipatch                                                   *)
(****************************************************************)

Module[{data},
    VerificationTest[
        data = ToListOfData[ReadGridFunction[$SimulationToolsMPTestSimulation, "x", {"z"},Iteration -> 0, Map -> 0]]
    ,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadGridFunction-1D-mp-map0.m"}]]
    ,
        TestID->"ReadGridFunction-1D-mp-map0"
    ]
]

Module[{data},
    VerificationTest[
        data = ToListOfData[ReadGridFunction[$SimulationToolsMPTestSimulation, "x", {"z"},Iteration -> 0, Map -> 1]]
    ,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadGridFunction-1D-mp-map1.m"}]]
    ,
        TestID->"ReadGridFunction-1D-mp-map1"
    ]
]

VerificationTest[
    ReadMaps[$SimulationToolsMPTestSimulation, "x", {"z"}]
,
    {0, 1, 2, 3, 4, 5, 6}
,
    TestID->"ReadMaps-1D-mp"
]
