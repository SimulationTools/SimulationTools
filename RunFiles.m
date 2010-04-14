
BeginPackage["RunFiles`", {"Profile`"}];

FileInRun;
ReadColumnFile;
ReadColumnFile2;
FindRunFile;
FindRunSegments;
FindRunFilesFromPattern;
StandardOutputOfRun;

Begin["`Private`"];

RunDirectory = Global`RunDirectory;

If[FileNameJoin[{"a","b"}] =!= "a/b",

FileNameJoin[elems_] :=
  StringJoin[Riffle[elems, "/"]];

FileNameDepth[s_] :=
  Length[StringCases[s,"/"]] + 1;

FileNameTake[s_] :=
  Last[StringSplit[s, "/"]];

]; 

(*--------------------------------------------------------------------
  File reading
  --------------------------------------------------------------------*)

addDataSubDir[output_String] :=
  Module[{},
    parFiles = FileNames["*/*.par", {output}, 2];
    If[Length[parFiles] === 0, Return[None]];
    If[Length[parFiles] =!= 1, Throw["Found more than one */*.par in " <> output]];
    FileNameJoin[Drop[FileNameSplit[parFiles[[1]]],-1]]];

FindRunSegments[runName_] :=
  Module[{dirName1, dirName2, restarts, files1, files2},
    dirName1 = 
      If[FileNameDepth[runName] == 1,
        FileNameJoin[{RunDirectory, runName}],
        runName];
    dirName2 = 
      If[FileType[dirName1] === None,
        If[FileType[dirName1 <> "-all"] =!= None,
          dirName1 <> "-all",
          Throw["Cannot find run directory for run " <> runName]],
        dirName1];

    If[FileType[FileNameJoin[{dirName2, "SIMULATION_ID"}]] =!= None,
      restarts = Select[FileNames["output-*", dirName2], ! StringMatchQ[#, "*/output-*-*"] &];
      segments = Select[Map[addDataSubDir, restarts], (# =!= None) &];
      Return[segments],
      Return[{dirName2}]];
  ];

FindRunFile[runName_String, fileName_String] :=
  Module[{segments, files1, files2},
    segments = FindRunSegments[runName];
    files1 = Map[FileNameJoin[{#, fileName}] &, segments];
    files2 = Select[files1, FileType[#] =!= None &];
    Return[files2];
  ];

FindRunFilesFromPattern[runName_String, filePattern_String] :=
  Module[{segments, files1, files2},
    segments = FindRunSegments[runName];
    names = Union[Map[FileNameTake, Flatten[Map[FileNames[filePattern, #] &, segments], 1]]]
  ];

StandardOutputOfRun[runName_String] :=
  Module[{segments, files1, files2},
    segments = FindRunSegments[runName];
    files1 = Map[FileNameJoin[{#, "../"<>runName<>".out"}] &, segments];
    files2 = Select[files1, FileType[#] =!= None &]];

stringToReal[s_String] :=
  Profile["stringToReal",
 Module[{p, n, mantissa, exponent},
  p = StringPosition[s, "e", 1];
  If[Length[p] == 0,
   ToExpression[s],
   n = p[[1, 1]];
   mantissa = ToExpression[StringTake[s, n - 1]];
   exponent = ToExpression[StringDrop[s, n]];
   1.0*mantissa*10^exponent]
  ]];

ReadColumnFile[fileName_String] :=
  Module[{list, list2, isComment, file2},
  Profile["ReadColumnFile[fileName]",
    If[FileType[fileName] === None, Throw["File " <> fileName <> " not found"]];
    list = ReadList[fileName, String]; (* Blank lines omitted *)
    isComment[x_] :=
      StringQ[x] && StringMatchQ[x, "#" ~~ ___];
    list2 = Select[list, !isComment[#] &];
    file2 = StringJoin[Riffle[list2, "\n"]];
    Return[Profile["ReadColumnFile:ImportString", ImportString[file2,"Table"]]]]];

ReadColumnFile[fileName_String, cols_List] :=
  extractColumns[ReadColumnFile[fileName], cols];

extractColumns[file_List, cols_List] := 
  Map[Extract[#, Map[List, cols]] &, file];

mergeFiles[files_List] :=
  Module[{file1, fileEndIndex, rest, rest0, restIndex, truncated},
    If[Length[files] == 0, Return[{}]];
    If[Length[files] == 1, Return[First[files]]];

    file1 = First[files];

    (* The first column of the files will be taken to be an
    index. Usually this will be an interation number or a coordinate
    time. *)
    fileEndIndex = First[Last[file1]];
    rest = mergeFiles[Rest[files]];

    rest0 = First[rest];
    restIndex = First[rest0];

    (* The most common case: no overlap *)
    If[restIndex > fileEndIndex,
      Return[Join[file1, rest]]];

    (* We have some overlap *)
    truncated = Select[file1, (First[#] < restIndex) &];
    Return[Join[truncated, rest]];
  ];

ReadColumnFile[fileNames_List] :=
  Module[{files},
    files = Select[Map[ReadColumnFile, fileNames], Length[#] != 0 &];
    mergeFiles[files]
  ];

ReadColumnFile[fileNames_List, cols_List] :=
  extractColumns[ReadColumnFile[fileNames], cols];

ReadColumnFile[runName_String, fileName_String] :=
  Module[{files = FindRunFile[runName, fileName]},
    If[files === {}, Throw["Cannot find file " <> fileName <> " in run " <> runName]];
  ReadColumnFile[files]];

ReadColumnFile[runName_String, fileName_String, cols_List] :=
  extractColumns[ReadColumnFile[runName, fileName], cols];

End[];

EndPackage[];
