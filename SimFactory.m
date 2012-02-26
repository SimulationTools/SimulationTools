
BeginPackage["SimFactory`", {"RunFiles`", "DataTable`", "Memo`", "Error`", "IniFile`"}];

Begin["`Private`"];

SimFactory`RunFiles`HaveData[runDir_] :=
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

End[];

EndPackage[];
