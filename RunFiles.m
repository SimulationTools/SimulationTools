(* ::Package:: *)


(* FIXME: if a run doesn't have a parameter file in it, nrmma cannot find ANY files! *)


(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["RunFiles`", {"Profile`", "Memo`", "Providers`", "Error`", "h5mma`"}];

ReadColumnFile;
ReadColumnFile2;
FindRunFile::usage = "FindRunFile[run, filename] returns a list containing the full names of files named filename in the different segments of run.";
FindRunSegments::usage = "FindRunSegments[run] returns a list of all the segment directories for a run which is split across multiple segments.";
FindRunDir::usage = "FindRunDir[run] returns the directory which contains the output data for run.  You can give a full or relative pathname to the run directory, or a run name relative to the (Global) variable RunDirectory.";
FindRunFilesFromPattern::usage = "FindRunFilesFromPattern[run, pattern] returns a list of leaf file names matching pattern which are found in any of the segments of run.  To find the paths within each segment, use FindRunFile on each result from this list.";
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
FindFirstRunFile::usage = "FindFirstRunFile[run, file] returns the full pathname of file in the first segment of run.";
FileIsInRun::usage = "FileIsInRun[run, filename] returns True or False depending on whether a file named filename can be found in run.";
HaveRunDir;
ReadCores::usage    = "ReadCores[run] returns the number of cores used by sim.";

Begin["`Private`"];

If[FileNameJoin[{"a","b"}] =!= "a/b",

FileNameJoin[elems_] :=
  StringJoin[Riffle[elems, "/"]];

FileNameDepth[s_] :=
  Length[StringCases[s,"/"]] + 1;

FileNameTake[s_] :=
  Last[StringSplit[s, "/"]];

]; 

(*--------------------------------------------------------------------
  Finding run directories
  --------------------------------------------------------------------*)

HaveRunDir[runName_String] :=
  Catch[FindRunDir[runName];True,ErrorTag[FindRunDir::nofind],False &];
  (* Probably don't really need to use exceptions for this *)

(* Given the name of a run directory, return a path to it *)
FindRunDir[runName_String] :=
  Module[
    {d, findRunDir},

    findRunDir[runNamep_String] :=
    Module[
      {dir, dirs},

      dir = runNamep;
      If[FileExistsQ[dir], Return[dir]];

      If[StringTake[runNamep,1] === "/", Return[None]];

      If[StringQ[Global`RunDirectory],
         dir = FileNameJoin[Join[FileNameSplit[Global`RunDirectory],FileNameSplit[runNamep]]];
         If[FileExistsQ[dir], Return[dir]];

         dirs = FileNames[runNamep, {Global`RunDirectory}, 2];
         If[Length[dirs] > 1,
            Error["Multiple runs called "<>runNamep<>" found: "<>ToString[dirs]]];

         If[Length[dirs] === 1, Return[dirs[[1]]]]];
      
      None];

    d = findRunDir[runName];
    If[d =!= None, Return[d]];

    (* Handle the case of the PSU merged run directory *)
    d = findRunDir[runName<>"-all"];
    If[d =!= None, Return[d]];

    FindRunDir::nofind = "Cannot find run `1`";
    ErrorMessage[FindRunDir::nofind,runName];
];

ReadCores[run_] :=
  CallProvidedFunction["RunFiles","ReadCores",{FindRunDir[run],run}];

(*--------------------------------------------------------------------
  Finding segments in run directories
  --------------------------------------------------------------------*)

FindRunSegments[runName_] :=
  FindRunDirSegments[FindRunDir[runName]];

FindRunDirSegments[runDir_] :=        
  Module[
    {segments},
    segments = Catch[CallProvidedFunction["RunFiles","FindRunDirSegments",{runDir}],
                     ErrorTag[CallProvidedFunction::nodata],
                     If[FileType[runDir] === Directory,
                        (* Assume we have pointed to a standard single-segment run directory *)
                        Return[{runDir}],
                        Throw[#1,#2]] &];
    segments];

(*--------------------------------------------------------------------
  Finding files from runs
  --------------------------------------------------------------------*)

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

Options[FindRunFilesFromPattern] = {FullFilenames -> False, LeafNamesOnly -> False};
DefineMemoFunction[FindRunFilesFromPattern[runName_String, filePattern_String, opts:OptionsPattern[]],
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

ImportGzip[file_String, as_] :=
  ImportString[ReadGzipFile[file],as];

DefineMemoFunction[ReadColumnFileWithFileName[fileName_String],
  Module[{list, list2, isComment, file2, data},
  Profile["ReadColumnFile[" <> fileName <> "]",
    If[FileType[fileName] === None, Error["File " <> fileName <> " not found (ReadColumnFileWithFileName)"]];

(* Print[1]; *)
    If[FileExtension[fileName] === "gz",
       (* Print["Importing gzip"]; *)
       (* Print[fileName]; *)
       list = StringSplit[ReadGzipFile[fileName],"\n"],
       list = ReadList[fileName, String]; (* Blank lines omitted *)];

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

FileIsInRun[run_, file_] :=
  FindRunFile[run, file] =!= {};

End[];

EndPackage[];
