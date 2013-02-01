(* Copyright 2010-2013 Ian Hinder and Barry Wardell

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

BeginPackage["SimulationTools`ColumnFile`",
 {"SimulationTools`Error`",
  "SimulationTools`Memo`",
  "SimulationTools`Profile`",
  "SimulationTools`Providers`",
  "SimulationTools`RunFiles`",
  "SimulationTools`"
 }];

ReadColumnFile;
ReadColumnFile2;
CarpetASCIIColumns;
ColumnNumbers;
MergeFiles;

(* Exceptions *)
UnknownColumns;

Begin["`Private`"];

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

ReadColumnFile[fileName_String, cols_List] :=
  Module[
    {data},
    data = ReadColumnFile[fileName];
    colNums = ColumnNumbers[fileName, cols];
    extractColumns[data,colNums]];

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

End[];
EndPackage[];
