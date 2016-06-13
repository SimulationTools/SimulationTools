(* ::Package:: *)

<<SimulationTools`


Print["Running tests"];


Print["Mathematica Version: "<>$Version];;


testfiles=FileNames["*.wlt",FileNameJoin[{$SimulationToolsInstallationDirectory,"Tests"}]];


testRun[file_String]:=Module[{report},
report=TestReport[file];
Print["  ", StringReplace[report["Title"],"Test Report: "->""], ": ", report["TestsSucceededCount"], "/",report["TestsSucceededCount"]+report["TestsFailedCount"]," passed"];
report
];


reports=testRun/@testfiles;


totalFailed=Total[#["TestsFailedCount"]&/@reports];


totalFailedWithErrors=Total[#["TestsFailedWithErrorsCount"]&/@reports];


If[totalFailedWithErrors>0,
Print["Tests failed with errors:"];
Do[
Scan[Print["  ",StringReplace[report["Title"],{"Test Report: "->"",".wlt"->""}],"/",#["TestID"]]&,report["TestsFailedWithErrors"]],{report,reports}];
]


totalFailedWithMessages=Total[#["TestsFailedWithMessagesCount"]&/@reports];


If[totalFailedWithMessages>0,
Print["Tests failed with messages:"];
Do[
Scan[Print["  ",StringReplace[report["Title"],{"Test Report: "->"",".wlt"->""}],"/",#["TestID"]]&,report["TestsFailedWithMessages"]],{report,reports}];
]


totalFailedWrongResults=Total[#["TestsFailedWrongResultsCount"]&/@reports];


If[totalFailedWrongResults>0,
Print["Tests failed due to wrong results:"];
Do[
Scan[Print["  ",StringReplace[report["Title"],{"Test Report: "->"",".wlt"->""}],"/",#["TestID"]]&,report["TestsFailedWrongResults"]],{report,reports}];
]


Print["Total tests passed: ",Total[#["TestsSucceededCount"]&/@reports]]


Print["Total tests failed: ",Total[#["TestsFailedCount"]&/@reports]]


If[And@@(#["AllTestsSucceeded"]&/@reports),Print["All tests passed"]]
