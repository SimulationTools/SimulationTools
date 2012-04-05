(* Mathematica Test File *)

RunDirectory = $NRMMATestRunDirectory;
TestReferenceDirectory = FileNameJoin[{FileNameDrop[FindFile["nrmma`"],-2],"Data/TestReference"}];

var1D = "phi.x.h5";
var2D = "phi.xy.h5";
var3D = "phi.file_0.h5";

(* ReadGridFunction *)

Module[{data},
    Test[
        data = GetData[ReadGridFunction[$NRMMATestRun, var1D, 256]];
        Part[data, {36, 59, 35, 35, 53, 13, 22, 57, 14, 58}]
    ,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadGridFunction-1D.m"}]]
    ,
        TestID->"ReadGridFunction-1D"
    ]
]

Module[{data},
    Test[
        data = Transpose[GetData[ReadGridFunction[$NRMMATestRun, var2D, 1024]]];
        Map[Part[data, Sequence @@ #] &, 
            {{39, 16}, {17, 4}, {58, 28}, {12, 96}, {38, 32},
             {52, 96}, {47, 27}, {5, 114}, {34, 105}, {5, 14}}
            ]
    ,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadGridFunction-2D.m"}]]
    ,
        TestID->"ReadGridFunction-2D"
    ]
]

Module[{data},
	Test[
        data = Transpose[GetData[
        	ReadGridFunction[$NRMMATestRun, var3D, 0]], {3, 2, 1}];
        Map[Part[data, Sequence @@ #] &, 
            {{4, 1, 49}, {41, 59, 1}, {52, 36, 15}, {38, 20, 14}, {2, 71, 41},
            {35, 1, 18}, {53, 38, 8}, {37, 21, 25}, {44, 80, 51}, {37, 113, 44}}
            ]
    ,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadGridFunction-3D.m"}]]
    ,
        TestID->"ReadGridFunction-3D"
	]
]


(* ReadIterations *)

Test[
    ReadIterations[$NRMMATestRun, var1D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-1D.m"}]]
,
    TestID->"ReadIterations-1D"
]

Test[
    ReadIterations[$NRMMATestRun, var2D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-2D.m"}]]
,
    TestID->"ReadIterations-2D"
]

Test[
    ReadIterations[$NRMMATestRun, var3D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-3D.m"}]]
,
    TestID->"ReadIterations-3D"
]

(* ReadMaps *)

Test[
    ReadMaps[$NRMMATestRun, var1D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadMaps-1D.m"}]]
,
    TestID->"ReadMaps-1D"
]

Test[
    ReadMaps[$NRMMATestRun, var2D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadMaps-2D.m"}]]
,
    TestID->"ReadMaps-2D"
]

Test[
    ReadMaps[$NRMMATestRun, var3D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadMaps-3D.m"}]]
,
    TestID->"ReadMaps-3D"
]

(* ReadRefinementLevels *)

Test[
    ReadRefinementLevels[$NRMMATestRun, var1D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadRefinementLevels-1D.m"}]]
,
    TestID->"ReadRefinementLevels-1D"
]

Test[
    ReadRefinementLevels[$NRMMATestRun, var2D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadRefinementLevels-2D.m"}]]
,
    TestID->"ReadRefinementLevels-2D"
]

Test[
    ReadRefinementLevels[$NRMMATestRun, var3D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadRefinementLevels-3D.m"}]]
,
    TestID->"ReadRefinementLevels-3D"
]


(* ReadTime *)

Test[
    ReadTime[$NRMMATestRun, var1D, 256, 5]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTime-1D.m"}]]
,
    TestID->"ReadTime-1D"
]

Test[
    ReadTime[$NRMMATestRun, var2D, 1024, 2]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTime-2D.m"}]]
,
    TestID->"ReadTime-2D"
]

Test[
    ReadTime[$NRMMATestRun, var3D, 8192, 0]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTime-3D.m"}]]
,
    TestID->"ReadTime-3D"
]

(* ReadTimeLevels *)

Test[
    ReadTimeLevels[$NRMMATestRun, var1D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTimeLevels-1D.m"}]]
,
    TestID->"ReadTimeLevels-1D"
]

Test[
    ReadTimeLevels[$NRMMATestRun, var2D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTimeLevels-2D.m"}]]
,
    TestID->"ReadTimeLevels-2D"
]

Test[
    ReadTimeLevels[$NRMMATestRun, var3D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTimeLevels-3D.m"}]]
,
    TestID->"ReadTimeLevels-3D"
]

(* ReadVariables *)

Test[
    ReadVariables[$NRMMATestRun, var1D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadVariables-1D.m"}]]
,
    TestID->"ReadVariabless-1D"
]

Test[
    ReadVariables[$NRMMATestRun, var2D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadVariables-2D.m"}]]
,
    TestID->"ReadVariables-2D"
]

Test[
    ReadVariables[$NRMMATestRun, var3D]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadVariables-3D.m"}]]
,
    TestID->"ReadVariables-3D"
]
