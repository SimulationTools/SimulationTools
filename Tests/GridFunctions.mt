(* Mathematica Test File *)

$SimulationPath = {$SimulationToolsTestSimulationDirectory};
TestReferenceDirectory = FileNameJoin[{$SimulationToolsInstallationDirectory,"Data/TestReference"}];

var = "phi";

dims1D = {"x"};
dims2D = {"x","y"};
dims3D = {"x","y","z"};

(* ReadGridFunction (pre 1.0) *)

Module[{data},
    Test[
        data = GetData[Block[{$SimulationToolsCompatibilityVersion = 0}, ReadGridFunction[$SimulationToolsTestSimulation, var, {"x"},Iteration -> 256]]];
        Part[data, {36, 59, 35, 35, 53, 13, 22, 57, 14, 58}]
    ,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadGridFunction-1D.m"}]]
    ,
        TestID->"ReadGridFunction-1D"
    ]
]

Module[{data},
    Test[
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
	Test[
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

Test[
    ReadIterations[$SimulationToolsTestSimulation, var, {"x"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-1D.m"}]]
,
    TestID->"ReadIterations-1D"
]

Test[
    ReadIterations[$SimulationToolsTestSimulation, var, {"x","y"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-2D.m"}]]
,
    TestID->"ReadIterations-2D"
]

Test[
    ReadIterations[$SimulationToolsTestSimulation, var, {"x","y","z"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-3D.m"}]]
,
    TestID->"ReadIterations-3D"
]

(* Dimensions specifications *)

Test[
    ReadIterations[$SimulationToolsTestSimulation, var, {1}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-1D.m"}]]
,
    TestID->"ReadIterations-dims-1D"
]

Test[
    ReadIterations[$SimulationToolsTestSimulation, var, {1,2}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-2D.m"}]]
,
    TestID->"ReadIterations-dims-2D"
]

Test[
    ReadIterations[$SimulationToolsTestSimulation, var, {1,2,3}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-3D.m"}]]
,
    TestID->"ReadIterations-dims-3D"
]

Test[
    ReadIterations[$SimulationToolsTestSimulation, var, "x"]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-1D.m"}]]
,
    TestID->"ReadIterations-dimsstrings-1D"
]

Test[
    ReadIterations[$SimulationToolsTestSimulation, var, "xy"]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-2D.m"}]]
,
    TestID->"ReadIterations-dimsstrings-2D"
]

Test[
    ReadIterations[$SimulationToolsTestSimulation, var, "xyz"]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-3D.m"}]]
,
    TestID->"ReadIterations-dimsstrings-3D"
]


(* ReadMaps *)

Test[
    ReadMaps[$SimulationToolsTestSimulation, var, {"x"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadMaps-1D.m"}]]
,
    TestID->"ReadMaps-1D"
]

Test[
    ReadMaps[$SimulationToolsTestSimulation, var, {"x","y"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadMaps-2D.m"}]]
,
    TestID->"ReadMaps-2D"
]

Test[
    ReadMaps[$SimulationToolsTestSimulation, var, {"x","y","z"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadMaps-3D.m"}]]
,
    TestID->"ReadMaps-3D"
]

(* ReadRefinementLevels *)

Test[
    ReadRefinementLevels[$SimulationToolsTestSimulation, var, {"x"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadRefinementLevels-1D.m"}]]
,
    TestID->"ReadRefinementLevels-1D"
]

Test[
    ReadRefinementLevels[$SimulationToolsTestSimulation, var, {"x","y"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadRefinementLevels-2D.m"}]]
,
    TestID->"ReadRefinementLevels-2D"
]

Test[
    ReadRefinementLevels[$SimulationToolsTestSimulation, var, {"x","y","z"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadRefinementLevels-3D.m"}]]
,
    TestID->"ReadRefinementLevels-3D"
]


(* ReadTime *)

Test[
    ReadTime[$SimulationToolsTestSimulation, var, {"x"}, Iteration -> 256, RefinementLevel -> 5]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTime-1D.m"}]]
,
    TestID->"ReadTime-1D"
]

Test[
    ReadTime[$SimulationToolsTestSimulation, var, {"x","y"}, Iteration -> 1024, RefinementLevel -> 2]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTime-2D.m"}]]
,
    TestID->"ReadTime-2D"
]

Test[
    ReadTime[$SimulationToolsTestSimulation, var, {"x","y","z"}, Iteration -> 8192, RefinementLevel -> 0]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTime-3D.m"}]]
,
    TestID->"ReadTime-3D"
]

(* ReadTimeLevels *)

Test[
    ReadTimeLevels[$SimulationToolsTestSimulation, var, {"x"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTimeLevels-1D.m"}]]
,
    TestID->"ReadTimeLevels-1D"
]

Test[
    ReadTimeLevels[$SimulationToolsTestSimulation, var, {"x","y"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTimeLevels-2D.m"}]]
,
    TestID->"ReadTimeLevels-2D"
]

Test[
    ReadTimeLevels[$SimulationToolsTestSimulation, var, {"x","y","z"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTimeLevels-3D.m"}]]
,
    TestID->"ReadTimeLevels-3D"
]
