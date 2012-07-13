(* Copyright (C) 2012 Ian Hinder and Barry Wardell *)

BeginPackage["SimFactory`",
 {
  "DataRepresentations`",
  "DataTable`",
  "Error`",
  "IniFile`",
  "Memo`",
  "RunFiles`"
 }];

Begin["`Private`"];

SimFactory`RunFiles`HaveData[runDir_,___] :=
  FileType[FileNameJoin[{runDir, "SIMULATION_ID"}]] =!= None ||
  FileType[FileNameJoin[{runDir, "SIMFACTORY"}]] =!= None;

getDataSubDir[output_String] :=
  Module[
    {parFiles,runName,dir},

    (* First, check if there is a directory with the same name as the run name *)
    runName = FileNameTake[output, {-2}];
    dir = FileNameJoin[{output,runName}];
    If[FileType[dir] === Directory,
       Return[runName]];

    (* If this fails, look for the directory containing the parfile *)
    parFiles = FileNames["*/*.par", {output}, 2];
    If[Length[parFiles] === 0, Return[None]];
    If[Length[parFiles] =!= 1, Error["Found more than one */*.par in " <> output]];
    FileNameTake[parFiles[[1]], {-2}]
];

SimFactory`RunFiles`FindRunDirSegments[dir_] :=
  Module[
    {restarts, dataSubDir, dataDirs, segments},
    restarts = Select[FileNames["output-*", dir], ! StringMatchQ[#, "*/output-*-*"] &];
    dataSubDir = getDataSubDir[restarts[[1]]];
    dataDirs = FileNameJoin[{#, dataSubDir}] & /@ restarts;
    segments = Select[dataDirs, DirectoryQ]
];

SimFactory`RunFiles`ReadCores[dir_, runName_] :=
  Module[
    {files, file},
    files = Select[{FileNameJoin[{dir, "SIMFACTORY", "PROCS"}],
                    FileNameJoin[{dir, "PROCS"}],
                    FileNameJoin[{dir, "SIMFACTORY/properties.ini"}]},
                   FileExistsQ];
    If[Length[files] === 0,
       ReadCores::nodata = "No data for ReadCores in run `1`";
       Error[ReadCores::nodata, runName]];
    file = First[files];
    If[FileNameTake[file,-1] === "properties.ini",
       ToExpression@IniVariable[FindRunSegments[runName][[1]]<>"/../SIMFACTORY/properties.ini", "procs"],
       ReadList[file, Number][[1]]]];

End[];

EndPackage[];
