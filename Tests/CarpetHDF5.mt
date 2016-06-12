Get[FileNameJoin[{$SimulationToolsInstallationDirectory,"Tests","Common.m"}]];

(* Mathematica Test File *)

$SimulationPath = {$SimulationToolsTestSimulationDirectory};
TestReferenceDirectory = FileNameJoin[{$SimulationToolsInstallationDirectory,"Data/TestReference"}];

var1D = "phi.x.h5";
var2D = "phi.xy.h5";
var3D = "phi.file_0.h5";

(* ReadGridFunction *)

Module[{data},
    VerificationTest[
        data = GetData[ReadGridFunction[$SimulationToolsTestSimulation, var1D, 256]];
        Part[data, {36, 59, 35, 35, 53, 13, 22, 57, 14, 58}]
    ,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadGridFunction-1D.m"}]]
    ,
        TestID->"ReadGridFunction-1D"
    ]
]

Module[{data},
    VerificationTest[
        data = GetData[ReadGridFunction[$SimulationToolsTestSimulation, var2D, 1024]];
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
        data = GetData[ReadGridFunction[$SimulationToolsTestSimulation, var3D, 0]];
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
    ReadIterations[$SimulationToolsTestSimulation, var1D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-1D.m"}]]
,
    TestID->"ReadIterations-1D"
]

VerificationTest[
    ReadIterations[$SimulationToolsTestSimulation, var2D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-2D.m"}]]
,
    TestID->"ReadIterations-2D"
]

VerificationTest[
    ReadIterations[$SimulationToolsTestSimulation, var3D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-3D.m"}]]
,
    TestID->"ReadIterations-3D"
]

(* ReadMaps *)

VerificationTest[
    ReadMaps[$SimulationToolsTestSimulation, var1D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadMaps-1D.m"}]]
,
    TestID->"ReadMaps-1D"
]

VerificationTest[
    ReadMaps[$SimulationToolsTestSimulation, var2D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadMaps-2D.m"}]]
,
    TestID->"ReadMaps-2D"
]

VerificationTest[
    ReadMaps[$SimulationToolsTestSimulation, var3D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadMaps-3D.m"}]]
,
    TestID->"ReadMaps-3D"
]

(* ReadRefinementLevels *)

VerificationTest[
    ReadRefinementLevels[$SimulationToolsTestSimulation, var1D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadRefinementLevels-1D.m"}]]
,
    TestID->"ReadRefinementLevels-1D"
]

VerificationTest[
    ReadRefinementLevels[$SimulationToolsTestSimulation, var2D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadRefinementLevels-2D.m"}]]
,
    TestID->"ReadRefinementLevels-2D"
]

VerificationTest[
    ReadRefinementLevels[$SimulationToolsTestSimulation, var3D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadRefinementLevels-3D.m"}]]
,
    TestID->"ReadRefinementLevels-3D"
]


(* ReadTime *)

VerificationTest[
    ReadTime[$SimulationToolsTestSimulation, var1D, 256, 5]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTime-1D.m"}]]
,
    TestID->"ReadTime-1D"
]

VerificationTest[
    ReadTime[$SimulationToolsTestSimulation, var2D, 1024, 2]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTime-2D.m"}]]
,
    TestID->"ReadTime-2D"
]

VerificationTest[
    ReadTime[$SimulationToolsTestSimulation, var3D, 8192, 0]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTime-3D.m"}]]
,
    TestID->"ReadTime-3D"
]

(* ReadTimeLevels *)

VerificationTest[
    ReadTimeLevels[$SimulationToolsTestSimulation, var1D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTimeLevels-1D.m"}]]
,
    TestID->"ReadTimeLevels-1D"
]

VerificationTest[
    ReadTimeLevels[$SimulationToolsTestSimulation, var2D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTimeLevels-2D.m"}]]
,
    TestID->"ReadTimeLevels-2D"
]

VerificationTest[
    ReadTimeLevels[$SimulationToolsTestSimulation, var3D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTimeLevels-3D.m"}]]
,
    TestID->"ReadTimeLevels-3D"
]

(* ReadVariables *)

VerificationTest[
    ReadVariables[$SimulationToolsTestSimulation, var1D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadVariables-1D.m"}]]
,
    TestID->"ReadVariabless-1D"
]

VerificationTest[
    ReadVariables[$SimulationToolsTestSimulation, var2D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadVariables-2D.m"}]]
,
    TestID->"ReadVariables-2D"
]

VerificationTest[
    ReadVariables[$SimulationToolsTestSimulation, var3D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadVariables-3D.m"}]]
,
    TestID->"ReadVariables-3D"
]
