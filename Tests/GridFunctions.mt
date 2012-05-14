(* Mathematica Test File *)

RunDirectory = $NRMMATestRunDirectory;
TestReferenceDirectory = FileNameJoin[{$NRMMADirectory,"Data/TestReference"}];

var = "phi";

dims1D = {"x"};
dims2D = {"x","y"};
dims3D = {"x","y","z"};

(* ReadGridFunction *)

Module[{data},
    Test[
        data = GetData[ReadGridFunction[$NRMMATestRun, var, {"x"},Iteration -> 256]];
        Part[data, {36, 59, 35, 35, 53, 13, 22, 57, 14, 58}]
    ,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadGridFunction-1D.m"}]]
    ,
        TestID->"ReadGridFunction-1D"
    ]
]

Module[{data},
    Test[
        data = Transpose[GetData[ReadGridFunction[$NRMMATestRun, var, {"x","y"}, Iteration -> 1024]]];
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
        	ReadGridFunction[$NRMMATestRun, var, {"x","y","z"}]], {3, 2, 1}];
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
    ReadIterations[$NRMMATestRun, var, {"x"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-1D.m"}]]
,
    TestID->"ReadIterations-1D"
]

Test[
    ReadIterations[$NRMMATestRun, var, {"x","y"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-2D.m"}]]
,
    TestID->"ReadIterations-2D"
]

Test[
    ReadIterations[$NRMMATestRun, var, {"x","y","z"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadIterations-3D.m"}]]
,
    TestID->"ReadIterations-3D"
]

(* ReadMaps *)

Test[
    ReadMaps[$NRMMATestRun, var, {"x"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadMaps-1D.m"}]]
,
    TestID->"ReadMaps-1D"
]

Test[
    ReadMaps[$NRMMATestRun, var, {"x","y"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadMaps-2D.m"}]]
,
    TestID->"ReadMaps-2D"
]

Test[
    ReadMaps[$NRMMATestRun, var, {"x","y","z"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadMaps-3D.m"}]]
,
    TestID->"ReadMaps-3D"
]

(* ReadRefinementLevels *)

Test[
    ReadRefinementLevels[$NRMMATestRun, var, {"x"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadRefinementLevels-1D.m"}]]
,
    TestID->"ReadRefinementLevels-1D"
]

Test[
    ReadRefinementLevels[$NRMMATestRun, var, {"x","y"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadRefinementLevels-2D.m"}]]
,
    TestID->"ReadRefinementLevels-2D"
]

Test[
    ReadRefinementLevels[$NRMMATestRun, var, {"x","y","z"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadRefinementLevels-3D.m"}]]
,
    TestID->"ReadRefinementLevels-3D"
]


(* ReadTime *)

Test[
    ReadTime[$NRMMATestRun, var, {"x"}, 256, 5]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTime-1D.m"}]]
,
    TestID->"ReadTime-1D"
]

Test[
    ReadTime[$NRMMATestRun, var, {"x","y"}, 1024, 2]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTime-2D.m"}]]
,
    TestID->"ReadTime-2D"
]

Test[
    ReadTime[$NRMMATestRun, var, {"x","y","z"}, 8192, 0]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTime-3D.m"}]]
,
    TestID->"ReadTime-3D"
]

(* ReadTimeLevels *)

Test[
    ReadTimeLevels[$NRMMATestRun, var, {"x"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTimeLevels-1D.m"}]]
,
    TestID->"ReadTimeLevels-1D"
]

Test[
    ReadTimeLevels[$NRMMATestRun, var, {"x","y"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTimeLevels-2D.m"}]]
,
    TestID->"ReadTimeLevels-2D"
]

Test[
    ReadTimeLevels[$NRMMATestRun, var, {"x","y","z"}]
,
    Get[FileNameJoin[{TestReferenceDirectory,"ReadTimeLevels-3D.m"}]]
,
    TestID->"ReadTimeLevels-3D"
]
