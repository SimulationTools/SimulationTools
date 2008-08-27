
BeginPackage["AsciiData1D`"];

AsciiData1D::usage = "AsciiData1D[fileName] loads the 1D data in
fileName (which should be in ygraph format) and returns it as a list
in the form {{t, {{x, f}...}}...}.";

AsciiDataOfIndex::usage = "AsciiDataOfIndex[data,i] returns the data,
f, of the ith element of data where data is in the form {{t, {{x,
f}...}}...}";

AsciiTimeOfIndex::usage = "AsciiTimeOfIndex[data,i] returns the time,
t, of the ith element of data where data is in the form {{t, {{x,
f}...}}...}";

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
   useLines = Take[lines, {2, Length[lines], 2}];
   Map[asciiTimeSlice, useLines]];

AsciiDataOfIndex[data_, index_] :=
  data[[index]][[2]];

AsciiTimeOfIndex[data_, index_] :=
  data[[index]][[1]];

End[];

EndPackage[];
