(* Mathematica Test File *)

testDirectory = FileNameJoin[{$TemporaryDirectory,"SimulationTools-unit-tests","RunFiles"}];
If[FileExistsQ[testDirectory], DeleteDirectory[testDirectory,DeleteContents->True]];
CreateDirectory[testDirectory];
simulationPath = FileNameJoin[{testDirectory,"simulations"}];
$SimulationPath = {simulationPath};

(* FindRunDir *)

Module[{runDir,runName="testrun"},
	runDir = FileNameJoin[{simulationPath,runName}];
	CreateDirectory[runDir];
	Test[
		FindRunDir[runName]
	,
		runDir
	,
		TestID->"FindRunDir"
	]]

Module[{runDir,runName="testrunall"},
	runDir = FileNameJoin[{simulationPath,runName<>"-all"}];
	CreateDirectory[runDir];
	Test[
		FindRunDir[runName]
	,
		runDir
	,
		TestID->"FindRunDir_All"
	]]

Module[{runDir,runName="testrun2"},
	runDir = FileNameJoin[{simulationPath,"intermediate",runName}];
	CreateDirectory[runDir, CreateIntermediateDirectories->True];
	Test[
		FindRunDir[runName]
	,
		runDir
	,
		TestID->"FindRunDir_Int"
	]]
	
Module[{runDir,runName="testrun3"},
	runDir = FileNameJoin[{testDirectory,runName}];
	CreateDirectory[runDir];
	Test[
		FindRunDir[runDir]
	,
		runDir
	,
		TestID->"FindRunDir_Abs"
	]]


(* FindRunSegments *)

Module[{runDir,runName="testrun5"},
	runDir = FileNameJoin[{simulationPath,runName}];
	CreateDirectory[runDir];
	Test[
		FindRunSegments[runName]
	,
		{runDir}
	,
		TestID->"FindRunSegments_PlainDir"
	]]	
	
Module[{runDir,runName="testrun6",testDirPaths},
	runDir = FileNameJoin[{simulationPath,runName}];
	CreateDirectory[runDir];
	CreateDirectory[FileNameJoin[{runDir,"SIMFACTORY"}]];
	testDirPaths = Table[Module[{f=FileNameJoin[{runDir,"output-000"<>ToString[i],runName}]},
		CreateDirectory[f,CreateIntermediateDirectories->True];
		f],{i,0,3}];
	Test[
		FindRunSegments[runName]
	,
		testDirPaths
	,
		TestID->"FindRunSegments_SimFactory2"
	]]

Module[{runDir,runName="testrun7",testDirPaths},
	runDir = FileNameJoin[{simulationPath,runName}];
	CreateDirectory[runDir];
	Export[FileNameJoin[{runDir,"SIMULATION_ID"}],"Test","Text"];
	testDirPaths = Table[Module[{f=FileNameJoin[{runDir,"output-000"<>ToString[i],runName}]},
		CreateDirectory[f,CreateIntermediateDirectories->True];
		f],{i,0,3}];
	Test[
		FindRunSegments[runName]
	,
		testDirPaths
	,
		TestID->"FindRunSegments_SimFactory1"
	]]
	
Module[{runDir,runName="testrun8",testDirPaths},
	runDir = FileNameJoin[{simulationPath,runName}];
	CreateDirectory[runDir];
	CreateDirectory[FileNameJoin[{runDir,"SIMFACTORY"}]];
	testDirPaths = Table[Module[{f=FileNameJoin[{runDir,"output-000"<>ToString[i],"different"}]},
		CreateDirectory[f,CreateIntermediateDirectories->True];
		Export[FileNameJoin[{f,"diff.par"}],"Test","Text"];
		f],{i,0,3}];
	Test[
		FindRunSegments[runName]
	,
		testDirPaths
	,
		TestID->"FindRunSegments_SimFactory2_DiffSubDir"
	]]	
	
(* FindRunFile *)
	
Module[{runDir,runName="testrun9",testFile="MyFile.asc",testFilePath},
	runDir = FileNameJoin[{simulationPath,runName}];
	CreateDirectory[runDir];
	testFilePath = FileNameJoin[{runDir,testFile}];
	Export[testFilePath,"Test","Text"];
	Test[
		FindRunFile[runName,testFile]
	,
		{testFilePath}
	,
		TestID->"FindRunFile_PlainDir"
	]]	
	
	
Module[{runDir,runName="testrun9.1",testFile="MyFile.asc",testFilePath},
	runDir = FileNameJoin[{simulationPath,runName}];
	CreateDirectory[runDir];
	CreateDirectory[FileNameJoin[{runDir,"intermediate"}]];
	testFilePath = FileNameJoin[{runDir,"intermediate",testFile}];
	Export[testFilePath,"Test","Text"];
	Test[
		FindRunFile[runName,testFile]
	,
		{testFilePath}
	,
		TestID->"FindRunFile_PlainDir_Intermediate"
	]]
	
Module[{runDir,runName="testrun10",testFile="MyFile.asc",testFilePaths},
	runDir = FileNameJoin[{simulationPath,runName}];
	CreateDirectory[runDir];
	CreateDirectory[FileNameJoin[{runDir,"SIMFACTORY"}]];
	testFilePaths = Table[Module[{f=FileNameJoin[{runDir,"output-000"<>ToString[i],runName}]},
		CreateDirectory[f,CreateIntermediateDirectories->True];
		Export[FileNameJoin[{f,testFile}],"Test","Text"]],{i,0,3}];
	Test[
		FindRunFile[runName,testFile]
	,
		testFilePaths
	,
		TestID->"FindRunFile_SimFactory"
	]]

(* FindRunFilesFromPattern *)

Module[{runDir,runName="testrun11",testFiles={"MyFile.asc","MyOtherFile.asc"}},
	runDir = FileNameJoin[{simulationPath,runName}];
	CreateDirectory[runDir];
	CreateDirectory[FileNameJoin[{runDir,"SIMFACTORY"}]];
	Do[Module[{f=FileNameJoin[{runDir,"output-000"<>ToString[i],runName}]},
		CreateDirectory[f,CreateIntermediateDirectories->True];
		Map[Export[FileNameJoin[{f,#}],"Test","Text"] &,testFiles]],{i,0,3}];
	Test[
		FindRunFilesFromPattern[runName,"*.asc"]
	,
		testFiles
	,
		TestID->"FindRunFilesFromPattern"
	]]
	
(* FindFirstRunFile *)	

Module[{runDir,runName="testrun12",testFile="MyFile.asc",testFilePaths},
	runDir = FileNameJoin[{simulationPath,runName}];
	CreateDirectory[runDir];
	CreateDirectory[FileNameJoin[{runDir,"SIMFACTORY"}]];
	testFilePaths = Table[Module[{f=FileNameJoin[{runDir,"output-000"<>ToString[i],runName}]},
		CreateDirectory[f,CreateIntermediateDirectories->True];
		Export[FileNameJoin[{f,testFile}],"Test","Text"]],{i,0,3}];
	Test[
		FindFirstRunFile[runName,testFile]
	,
		First[testFilePaths]
	,
		TestID->"FindFirstRunFile"
	]]
	
(* FindSimulationFiles *)	
	
Module[{runDir,runName="testrun12.1",testDirPaths,testFile="MyFile.asc",testFilePaths},
	runDir = FileNameJoin[{simulationPath,runName}];
	CreateDirectory[runDir];
	CreateDirectory[FileNameJoin[{runDir,"SIMFACTORY"}]];
	testDirPaths = Table[Module[{f=FileNameJoin[{runDir,"output-000"<>ToString[i],runName}]},
		CreateDirectory[f,CreateIntermediateDirectories->True];
		f],{i,0,3}];

	testFilePaths = Table[Module[{f=FileNameJoin[{runDir,"output-000"<>ToString[i],runName}]},
		Export[FileNameJoin[{f,testFile}],"Test","Text"]],{i,0,3}];

	Test[
		FindSimulationFiles[runName,testFile]
	,
		testFilePaths
	,
		TestID->"FindSimulationFiles_SimFactory2"
	]]	
	
Module[{runDir,runName="testrun13",otherRunName="mysim",testDirPaths,testFile="MyFile.asc",testFilePaths},
	runDir = FileNameJoin[{simulationPath,runName}];
	CreateDirectory[runDir];
	CreateDirectory[FileNameJoin[{runDir,"SIMFACTORY"}]];
	testDirPaths = Table[Module[{f=FileNameJoin[{runDir,"output-000"<>ToString[i],otherRunName}]},
		CreateDirectory[f,CreateIntermediateDirectories->True];
		f],{i,0,3}];

	testFilePaths = Table[Module[{f=FileNameJoin[{runDir,"output-000"<>ToString[i],otherRunName}]},
		Export[FileNameJoin[{f,"anything.par"}],"Test","Text"];
		Export[FileNameJoin[{f,testFile}],"Test","Text"]],{i,0,3}];

	Test[
		FindSimulationFiles[runName,testFile]
	,
		testFilePaths
	,
		TestID->"FindSimulationFiles_SimFactory2_OtherName"
	]]	