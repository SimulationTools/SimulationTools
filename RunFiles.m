(* ::Package:: *)


(* FIXME: if a run doesn't have a parameter file in it, nrmma cannot find ANY files! *)


(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["RunFiles`", {"Profile`", "Memo`"}];

ReadColumnFile;
ReadColumnFile2;
FindRunFile;
FindRunSegments;
FindRunFilesFromPattern;
StandardOutputOfRun;
CarpetASCIIColumns;
MergeFiles;
SegmentStartDate;
SegmentEndDate;
SegmentDuration;
RunDutyCycle;
SegmentCoordinateTimeInterval;
SegmentStartTimes;
FullFilenames;
LeafNamesOnly;
FindFirstRunFile;

Begin["`Private`"];

RunDirectory := Global`RunDirectory;

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

(* This only finds data subdirectories which contain a parameter file
- this doesn't work with some testsuite parameter files.  Need to find
a better way. *)

addDataSubDir[output_String] :=
  Module[{parFiles},
    parFiles = FileNames["*/*.par", {output}, 2];
    If[Length[parFiles] === 0, Return[None]];
    If[Length[parFiles] =!= 1, Throw["Found more than one */*.par in " <> output]];
    FileNameJoin[Drop[FileNameSplit[parFiles[[1]]],-1]]];

runDirType[dir_String] :=
  If[FileType[dir] =!= Directory,
    None,
    If[FileType[FileNameJoin[{dir, "SIMULATION_ID"}]] =!= None ||
       FileType[FileNameJoin[{dir, "SIMFACTORY"}]] =!= None,
       SimFactory,
       Standard
    ]
  ];

(* Given the name of a run directory, return a path to it *)
FindRunDir[runName_] :=
 Module[{possibles},
  If[runDirType[runName] =!= None,
    (* The run is given by a name which can be found directly *)
    runName,

    (* If we give anything containing a slash, and it doesn't exist, then give up
       rather than trying to look in RunDirectory. *)
    If[FileNameDepth[runName]>1, Return[None]];

    If[!StringQ[RunDirectory], Return[None]];

    (* Cannot find directly, so look under RunDirectory *)
    If[runDirType[FileNameJoin[{RunDirectory, runName}]] =!= None,
      FileNameJoin[{RunDirectory, runName}],
      possibles = Select[FileNames[runName, {RunDirectory}, 2], runDirType =!= None &];
      If[possibles === {},
        None,
        First[possibles]]]]
 ];

FindRunSegments[runName_] :=
  Module[{dir, restarts, segments},
    dir = FindRunDir[runName];
    If[dir === None,
      dir = FindRunDir[runName<>"-all"];
      If[dir === None,
        Throw["Cannot find run directory for run " <> runName]]];
    If[runDirType[dir] === SimFactory,
      restarts = Select[FileNames["output-*", dir], ! StringMatchQ[#, "*/output-*-*"] &];
      segments = Select[Map[addDataSubDir, restarts], (# =!= None) &],
      {dir}]];

FindRunFile[runName_String, fileName_String] :=
  Module[{segments, files1, files2},
    segments = FindRunSegments[runName];
    files1 = Map[FileNameJoin[{#, fileName}] &, segments];
    files2 = Select[files1, FileType[#] =!= None &];

    If[files2 === {},
      Return[FindRunFilesFromPattern[runName, fileName, FullFilenames->True]]];

    Return[files2];
  ];

DefineMemoFunction[FindFirstRunFile[runName_String, fileName_String],
  Profile["FindFirstRunFile",
  Module[{files},
    files = FindRunFile[runName, fileName];
    If[files === {}, Throw["File " <> fileName <> " not found in run " <> runName]];
    files[[1]]]]];

Options[FindRunFilesFromPattern] = {FullFilenames -> False, LeafNamesOnly -> False};
FindRunFilesFromPattern[runName_String, filePattern_String, opts:OptionsPattern[]] :=
  Module[{segments, nToDrop, names},
    segments = FindRunSegments[runName];
    If[segments === {}, Return[{}]];
    nToDrop = If[OptionValue[FullFilenames], 0, Length[FileNameSplit[segments[[1]]]]];
    names = Union[Map[FileNameDrop[#, nToDrop] &, Flatten[Map[FileNames[filePattern, #, Infinity] &, segments], 1]]];
    If[OptionValue[LeafNamesOnly],
      names = Map[FileNameTake[#,-1]&, names]];
    names
  ];

StandardOutputOfRun[runName_String] :=
  Module[{segments, files1, files2},
    segments = FindRunSegments[runName];
    files1 = Map[FileNameJoin[{#, "../"<>Last@FileNameSplit@runName<>".out"}] &, segments];
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
  ReadColumnFileWithFileName[fileName];

DefineMemoFunction[ReadColumnFileWithFileName[fileName_String],
  Module[{list, list2, isComment, file2, data},
  Profile["ReadColumnFile[" <> fileName <> "]",
    If[FileType[fileName] === None, Throw["File " <> fileName <> " not found (ReadColumnFileWithFileName)"]];
    list = ReadList[fileName, String]; (* Blank lines omitted *)
    isComment[x_] :=
      StringQ[x] && StringMatchQ[x, "#" ~~ ___];
    list2 = Select[list, !isComment[#] &];
    file2 = StringJoin[Riffle[list2, "\n"]];
    data = Profile["ReadColumnFile:ImportString", ImportString[file2,"Table"]];
    If[!ArrayQ[data],
      Throw["File "<>fileName<>" missing data."];
    ,
      Return[data];
    ];
  ]
  ]
];

ReadColumnFile[fileName_String, cols_List] :=
  extractColumns[ReadColumnFile[fileName], cols];

extractColumns[file_List, cols_List] := 
  Map[Extract[#, Map[List, cols]] &, file];

MergeFiles[files_List] :=
  Module[{file1, fileEndIndex, rest, rest0, restIndex, truncated},
    If[Length[files] == 0, Return[{}]];
    If[Length[files] == 1, Return[First[files]]];

    file1 = First[files];

    (* The first column of the files will be taken to be an
    index. Usually this will be an interation number or a coordinate
    time. *)
    fileEndIndex = First[Last[file1]];
    rest = MergeFiles[Rest[files]];

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
    MergeFiles[files]
  ];

ReadColumnFile[fileNames_List, cols_List] :=
  extractColumns[ReadColumnFile[fileNames], cols];

ReadColumnFile[runName_String, fileName_String] :=
  Module[{files = FindRunFile[runName, fileName]},
    If[files === {}, Throw["Cannot find file " <> fileName <> " in run " <> runName]];
  ReadColumnFile[files]];

ReadColumnFile[runName_String, fileName_String, cols_List] :=
  Module[{cols2},
    If[Or@@(StringQ /@ cols),
      cols2 = ColumnNumbers[runName, fileName, cols],
      cols2 = cols];
  extractColumns[ReadColumnFile[runName, fileName], cols2]];

(* Column numbering functions *)

(*stripWhiteSpace[s_String] := StringCases[s, (StartOfLine ~~ (Whitespace ~~ x___ ~~ Whitespace ~~ EndOfLine) :> x][[1]];*)

stripWhitespace[s_String] :=
  StringReplace[StringReplace[s, StartOfString ~~ Whitespace -> ""], Whitespace ~~ EndOfString -> ""];

CarpetASCIIColumns[fileName_String] :=
 Module[{lines, descLine, colDescs, colLines},
  If[FileType[fileName] === None,
    Throw["CarpetASCIIColumns: File " <> fileName <> " not found"]];
  lines = ReadList[fileName, String, 20];
  colLines = Select[lines,
     StringMatchQ[#, StartOfLine ~~ "#" ~~ Whitespace ~~ (NumberString | "data columns" | "column format") ~~ ":" ~~ __] &];
  descLine = StringJoin[Riffle[colLines, " "]];
  colDescs = 
   StringCases[descLine, 
    col : (DigitCharacter ..) ~~ ":" ~~ 
      id : Shortest[__ ~~ (Whitespace|EndOfString)] :> (stripWhitespace[id] -> ToExpression@col)];
   colDescs];

CarpetASCIIColumns[run_String, fileName_String] :=
  Module[{files = FindRunFile[run, fileName]},
    If[files === {},
      Throw["Cannot find file " <> fileName <> " in run " <> run]];
    CarpetASCIIColumns[First@files]];

ColumnNumbers[run_String, fileName_String, colIDs_] :=
 Module[{colMap, result},
  colMap = CarpetASCIIColumns[run, fileName];
  result = colIDs /. colMap;
  If[Or@@(StringQ /@ result),
    Throw["ColumnNumbers: Unknown columns: " <> ToString[result], UnknownColumns]];
  result];

SegmentStartDate[dir_] :=
  FileDate[FileNameJoin[{dir, "/carpet::timing..asc"}]];

SegmentEndDate[dir_] :=
  Module[{parFile},
    parFile = First[FileNames["*.par", dir]];
    FileDate[parFile]];

SegmentDuration[dir_] :=
  DateDifference[SegmentEndDate[dir], SegmentStartDate[dir], "Second"][[1]];

RunDutyCycle[run_] :=
  Module[{segs = FindRunSegments[run], totalRunTime, totalElapsedTime},
    totalRunTime = Plus@@(SegmentDuration /@ segs);
    totalElapsedTime = DateDifference[SegmentStartDate[First[segs]],
                                      SegmentEndDate[Last[segs]], "Second"][[1]];
    totalRunTime / totalElapsedTime //N];

SegmentCoordinateTimeInterval[dir_] :=
 Module[{times =
    Catch[First /@ ReadColumnFile[dir, "carpet::timing..asc", {9}]]},
  If[! ListQ[times], Return[None], Return[{times[[1]], times[[-1]]}]]];

SegmentStartTimes[run_] :=
 Module[{segs = FindRunSegments[run]},
  First /@
   Select[SegmentCoordinateTimeInterval /@ segs, # =!= None &]];

End[];

EndPackage[];
