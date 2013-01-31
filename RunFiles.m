(* Copyright 2010-2012 Ian Hinder and Barry Wardell

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

BeginPackage["SimulationTools`RunFiles`",
 {
  "SimulationTools`Error`",
  "SimulationTools`Memo`",
  "SimulationTools`Profile`",
  "SimulationTools`Providers`",
  "SimulationTools`"
 }];

FindSimulationFiles::usage = "FindSimulationFiles[simname, filename] gives all the files with the given name across all the segments of a simulation.  filename can be a string, a string expression or a regular expression.  This function always returns full pathnames.";
ReadSimulationCoreCount::usage    = "ReadSimulationCoreCount[simname] gives the number of cores used by a simulation.";
ReadSimulationRunTime::usage = "ReadSimulationRuntime[sim] gives the real time in seconds elapsed during the execution of a simulation.";


(* TODO: These don't belong here *)
ReadColumnFile;
ReadColumnFile2;
CarpetASCIIColumns;
ColumnNumbers;
MergeFiles;

StandardOutputOfRun;
SegmentStartDate;
SegmentEndDate;
SegmentDuration;
RunDutyCycle;
SegmentCoordinateTimeInterval;
SegmentStartTimes;
HaveRunDir;
ReadWalltimeHours;

(* Deprecated *)
FindRunSegments;
FindRunDir;
FindRunFile;
FindRunFilesFromPattern;
FindFirstRunFile;
FileIsInRun;

(* TODO: Convert these to strings and make them options to FindRunFiles *)
FullFilenames;
LeafNamesOnly;

(* Old names *)
ReadCores = ReadSimulationCoreCount;
FindRunFiles = FindSimulationFiles;
ReadWalltime = ReadSimulationRunTime;

(* Exceptions *)
NoSimulationCoreCountAvailable;
NoSimulationRunTimeAvailable;

Begin["`Private`"];

If[Head[FileNameJoin[{"a","b"}]] === FileNameJoin,

(* FileNameJoin (available since v7) is not defined.
   $PathnameSeparator has been available since v2, according to the
   documentation. *)

FileNameJoin[elems_] :=
  StringJoin[Riffle[elems, $PathnameSeparator]];

FileNameDepth[s_] :=
  Length[StringCases[s,$PathnameSeparator]] + 1;

FileNameTake[s_] :=
  Last[StringSplit[s, $PathnameSeparator]];

]; 

(*--------------------------------------------------------------------
  Finding run directories
  --------------------------------------------------------------------*)

HaveRunDir[runName_String] :=
  findRunDir[runName] =!= None;

(* Given the name of a run directory, return a path to it *)
findRunDir[runNamep_String] :=
 Module[{dir, dirs},
  If[FileExistsQ[runNamep], Return[runNamep]];

  If[StringTake[runNamep,1] === "/", Return[None]];

  dirs = FileNames[runNamep, SimulationPath[], 2];
  Which[
   Length[dirs]>1,
    Error["Multiple runs called "<>runNamep<>" found: "<>ToString[dirs]];,
   Length[dirs] === 1,
    dir = First[dirs];,
   Length[dirs] === 0,
    dir = None;
  ];
  dir
];

FindRunDir[runName_String] :=
  Module[
    {d},

    d = findRunDir[runName];
    If[d =!= None, Return[d]];

    (* Handle the case of the PSU merged run directory *)
    d = findRunDir[runName<>"-all"];
    If[d =!= None, Return[d]];

    Error["Cannot find run "<>runName]
];

ReadCores[run_] :=
  If[HaveData["RunFiles", FindRunDir[run]],
    CallProvidedFunction["RunFiles","ReadCores",{FindRunDir[run],run}],
    Error[NoSimulationCoreCountAvailable, "Simulation core count not available in \""<>run<>"\""]];

(*--------------------------------------------------------------------
  Finding segments in run directories
  --------------------------------------------------------------------*)

FindRunSegments[runName_] :=
  FindRunDirSegments[FindRunDir[runName]];

FindRunDirSegments[runDir_] :=
  If[HaveData["RunFiles", runDir],
     CallProvidedFunction["RunFiles","FindRunDirSegments",{runDir}],
     (* else *)
     If[FileType[runDir] === Directory,
     {runDir},
        Error[runDir<>" is not a directory or simulation"]]];

(*--------------------------------------------------------------------
  Finding files from runs
  --------------------------------------------------------------------*)

DocumentationBuilder`SymbolDescription["FindSimulationFiles"] =
  "find instances of a specific file across all segments of a simulation";

FindRunFiles[run_String, filename_String] := FindRunFile[run, filename];
FindRunFiles[run_String, filename:(_StringExpression|_RegularExpression)] :=
  FindRunFilesFromPattern[run, filename, FullFilenames -> True];

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
    If[files === {}, Error["File " <> fileName <> " not found in run " <> runName]];
    files[[1]]]]];

Options[FindRunFilesFromPattern] = {"FullFilenames" -> False, "LeafNamesOnly" -> False};
DefineMemoFunction[FindRunFilesFromPattern[runName_String, filePattern:(_String|_StringExpression|_RegularExpression), opts:OptionsPattern[]],
  Module[{segments, nToDrop, names},
    segments = FindRunSegments[runName];
    If[segments === {}, Return[{}]];
    nToDrop = If[OptionValue[FullFilenames], 0, Length[FileNameSplit[segments[[1]]]]];
    names = Union[Map[FileNameDrop[#, nToDrop] &, Flatten[Map[FileNames[filePattern, #, Infinity] &, segments], 1]]];
    If[OptionValue[LeafNamesOnly],
      names = Map[FileNameTake[#,-1]&, names]];
    names
]];


(*--------------------------------------------------------------------
  High-level operations on files from runs
  --------------------------------------------------------------------*)

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
    If[FileType[fileName] === None, Error["File " <> fileName <> " not found (ReadColumnFileWithFileName)"]];
    list = ReadList[fileName, String]; (* Blank lines omitted *)
    isComment[x_] :=
      StringQ[x] && StringMatchQ[x, "#" ~~ ___];
    list2 = Select[list, !isComment[#] &];
    file2 = StringJoin[Riffle[list2, "\n"]];
    data = Profile["ReadColumnFile:ImportString", ImportString[file2,"Table"]];
    If[!ArrayQ[data],
      Error["File "<>fileName<>" missing data."];
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
    If[files === {}, Error["Cannot find file " <> fileName <> " in run " <> runName]];
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
    Error["CarpetASCIIColumns: File " <> fileName <> " not found"]];
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
      Error["Cannot find file " <> fileName <> " in run " <> run]];
    CarpetASCIIColumns[First@files]];

ColumnNumbers[run_String, fileName_String, colIDs_] :=
  ColumnNumbers[First[FindRunFile[run, fileName]], colIDs];

ColumnNumbers[fileName_String, colIDs_] :=
 Module[{colMap, result},
  colMap = CarpetASCIIColumns[fileName];
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

FileIsInRun[run_, file_] :=
  FindRunFile[run, file] =!= {};

ReadWalltime[runName_] :=
  Module[{segmentTime, files},
    segmentTime[file_] :=
      ReadColumnFile[file, {9, 14}][[-1,2]];
    files = FindRunFile[runName, "carpet::timing..asc"];
    Plus@@(segmentTime /@ files)];

ReadWalltimeHours[runName_] := 
  If[FindRunFile[runName, "carpet::timing..asc"] =!= {},
    ReadWalltime[runName]/3600,
    (* else *)
    Error[NoSimulationRunTimeAvailable,
          "Simulation run time not available in \""<>runName<>"\""]];

End[];

EndPackage[];
