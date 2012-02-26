
BeginPackage["SimFactory`", {"RunFiles`", "DataTable`", "Memo`", "Error`", "IniFile`"}];

Begin["`Private`"];

SimFactory`RunFiles`HaveData[runDir_,___] :=
  FileType[FileNameJoin[{runDir, "SIMULATION_ID"}]] =!= None ||
  FileType[FileNameJoin[{runDir, "SIMFACTORY"}]] =!= None;

addDataSubDir[output_String] :=
  Module[
    {parFiles,runName,dir},

    runName = FileNameSplit[output][[-2]];
    dir = FileNameJoin[{output,runName}];
    If[FileType[dir] === Directory,
       Return[dir]];

    parFiles = FileNames["*/*.par", {output}, 2];
    If[Length[parFiles] === 0, Return[None]];
    If[Length[parFiles] =!= 1, Error["Found more than one */*.par in " <> output]];
    FileNameJoin[Drop[FileNameSplit[parFiles[[1]]],-1]]];

SimFactory`RunFiles`FindRunDirSegments[dir_] :=
  Module[
    {restarts, segments},
    restarts = Select[FileNames["output-*", dir], ! StringMatchQ[#, "*/output-*-*"] &];
    segments = Select[Map[addDataSubDir, restarts], (# =!= None) &]];

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
