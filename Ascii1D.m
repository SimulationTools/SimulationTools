
BeginPackage["Ascii1D`", {"DataTable`", "Profile`", "Memo`"}];

(* AsciiData1D::usage = "AsciiData1D[fileName] loads the 1D data in
fileName (which should be in ygraph format) and returns it as a list
in the form {{t, {{x, f}...}}...}."; *)

AsciiDataOfIndex::usage = "AsciiDataOfIndex[data,i] returns the data,
f, of the ith element of data where data is in the form {{t, {{x,
f}...}}...}";

AsciiTimeOfIndex::usage = "AsciiTimeOfIndex[data,i] returns the time,
t, of the ith element of data where data is in the form {{t, {{x,
f}...}}...}";

ReadCarpetASCII1D::usage = "ReadCarpetASCII1D[filename, level, dir] reads a 1D CarpetIOASCII output file.  It selects the given refinement level and assumes that the output is in direction dir, running from 1 to 3. The data is returned as a list of the form {{t1, d1}, {t2, d2}, ..., {tn, dn}} where the ti are the times and the di are DataTables representing the 1D data at those times.  If the 1D data is not contiguous, for example if there is a gap in this refinement level, the behaviour of the resulting DataTable objects with other DataTable functions is undefined.";

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

(* fixData[d_] :=
  Profile["ReadCarpetASCII1D: Fixing data",
    DeleteDuplicates[Sort[d, #1[[1]] < #2[[1]] &], 
                     (Abs[#1[[1]]-#2[[1]]] < 10^-10) &]]; *)

cmp = 
  Compile[{{x1, _Real, 1}, {x2, _Real, 1}},  
    Abs[x1[[1]] - x2[[1]]] < 10.^-10];

fixData[d_] :=
  Profile["ReadCarpetASCII1D: Fixing data",
    DeleteDuplicates[d, 
                     (Abs[#1[[1]]-#2[[1]]] < 10.^-10) &]];

(*fixData[d_] :=
  Profile["ReadCarpetASCII1D: Fixing data",
    DeleteDuplicates[d, cmp]];*)

(*fixData[d_] :=
  Profile["ReadCarpetASCII1D: Fixing data",
    DeleteDuplicates[d, 
                     #1[[1]] != #2[[1]] &]];*)

takeCols[it_List, c1_, c2_] := 
  Profile["ReadCarpetASCII1D: takeCols", 
    Map[{#[[c1]], #[[c2]]} &, it]];

DefineMemoFunction[tableImport[fn_],
  Import[fn, "Table"]];

ReadCarpetASCII1D[fileName_, dir_:1] :=
 Module[{lines, lines2, lines3, lines4, lines5, lines6, 
   levels, splitIterations, levels2, levelsPresent, data, data2, 
   rls, processLevel, prls},
  Print[fileName];
(*  lines = ReadList[fileName, String, NullRecords -> True];*)
  If[FileType[fileName] === None, Throw["ReadCarpetASCII1D: File "<>fileName<>" not found"]];
  Profile["ReadCarpetASCII1D: " <> fileName,
  Profile["ReadCarpetASCII1D: Reading file", 
    data = tableImport[fileName]];
  Profile["ReadCarpetASCII1D: Eliminating white space", 
    data2 = Select[data, Length[#] != 0 && #[[1]] != "#" &]];
  Profile["ReadCarpetASCII1D: Determining levels present",
    levelsPresent = Union[Map[#[[3]] &, data2]]];

  levels = GatherBy[data2, #[[3]]&];
  rls = Map[First[#][[3]]&, levels];

  processLevel[data3_] :=
    Module[{data4, data5, times, dataWithTimes},
(*      Print["rl" <> ToString[data3[[1,3]]]];*)
      Profile["ReadCarpetASCII1D: Splitting by iteration", 
        data4 = N[SplitBy[data3, #[[1]] &]]];
      data5 = Map[MakeDataTable@fixData[takeCols[#, 10+dir-1, 13]] &, data4];
      Profile["ReadCarpetASCII1D: Extracting times",
        times = Map[#[[1]][[9]] &, data4]];
      dataWithTimes = MapThread[List, {times, data5}]];

(*  prls = MapThread[List, {rls, processLevel /@ levels}];*)
  prls = processLevel /@ levels;
  Return[prls]]];

ReadCarpetASCII1DFromRun[run_String, fileName_String, dir_] :=
  Module[{},
    files = FindRunFile[run, fileName];
  ];

MGraph[data__] :=
  Manipulate[
   ListLinePlot[data[[i, 2]], 
    PlotLabel -> "t = " <> ToString[data[[i, 1]]]], {i, 1, Length[data],
     1}];

End[];

EndPackage[];
