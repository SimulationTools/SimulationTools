
BeginPackage["AsciiData1D`", {"DataTable`"}];

AsciiData1D::usage = "AsciiData1D[fileName] loads the 1D data in
fileName (which should be in ygraph format) and returns it as a list
in the form {{t, {{x, f}...}}...}.";

AsciiDataOfIndex::usage = "AsciiDataOfIndex[data,i] returns the data,
f, of the ith element of data where data is in the form {{t, {{x,
f}...}}...}";

AsciiTimeOfIndex::usage = "AsciiTimeOfIndex[data,i] returns the time,
t, of the ith element of data where data is in the form {{t, {{x,
f}...}}...}";

ReadCarpetASCII1D;

Begin["`Private`"];

asciiStringPair[string_] :=
 Module[{stream, x, y},
  stream = StringToStream[string];
  x = Read[stream, Real];
  y = Read[stream, Real];
  Close[stream];
  Return[{x, y}]]

asciiTimeHeader[string_] :=
 Module[{},
  StringReplace[string, {"\"Time = " -> "", "#Time = " -> ""}]]

asciiTimeSlice[list_] :=
 Module[{t, data},
  t = asciiTimeHeader[list[[1]]];
  data = Map[asciiStringPair, Drop[list, 1]];
  Return[{ToExpression[t], data}]]

(* isTimeString[s_] := 
  StringMatchQ[s, ("\"" | "#") ~~ "Time =" ~~ __];

firstTimeString[lines_] :=
  Position[lines, _ ? isTimeString, 1]; *)

AsciiData1D[fileName_] :=
  Module[{lines, useLines, firstTime},
   lines = ReadList[fileName, String, RecordLists -> True];
   useLines = Take[lines, {1, Length[lines], 1}];
   Map[asciiTimeSlice, useLines]];

AsciiDataOfIndex[data_, index_] :=
  data[[index]][[2]];

AsciiTimeOfIndex[data_, index_] :=
  data[[index]][[1]];

fixData[d_] :=
 DeleteDuplicates[Sort[d, #1[[1]] < #2[[1]] &], (Abs[#1[[1]]-#2[[1]]] < 10^-10) &];

ReadCarpetASCII1D[fileName_, level_:0, dir_:1] :=
 Module[{lines, lines2, lines3, lines4, takeCols, lines5, lines6, 
   levels, splitIterations, levels2, levelsPresent, data, data2, 
   data3, data4, data5, times, dataWithTimes},
  lines = ReadList[fileName, String, NullRecords -> True];
  data = Import[fileName, "Table"];
  data2 = Select[data, Length[#] != 0 && #[[1]] != "#" &];
  levelsPresent = Union[Map[#[[3]] &, data2]];
  If[! MemberQ[levelsPresent, level], 
   Throw["Refinement level " <> ToString[level] <> " not found in " <>
      fileName]];
  data3 = Select[data2, #[[3]] == level &];
  data4 = SplitBy[data3, #[[1]] &];
  takeCols[it_List, c1_, c2_] := Map[{#[[c1]], #[[c2]]} &, it];
  data5 = Map[MakeDataTable@fixData[takeCols[#, 10+dir-1, 13]] &, data4];
  times = Map[#[[1]][[9]] &, data4];
  dataWithTimes = MapThread[List, {times, data5}];
  Return[dataWithTimes]]

End[];

EndPackage[];
