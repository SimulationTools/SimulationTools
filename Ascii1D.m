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

BeginPackage["SimulationTools`Ascii1D`",
 {
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`Memo`",
  "SimulationTools`Profile`",
  "SimulationTools`RunFiles`"
 }];

(* These functions should implement the GridFunctions interface so
   that ASCII and HDF5 data is accessed in the same way.  There will
   probably only be one type available, so we would just take whatever
   we found, preferring HDF5 if present.  *)

(* AsciiData1D::usage = "AsciiData1D[fileName] loads the 1D data in
fileName (which should be in ygraph format) and returns it as a list
in the form {{t, {{x, f}...}}...}."; *)

AsciiDataOfIndex(*::usage = "AsciiDataOfIndex[data,i] returns the data, f, of the ith element of data where data is in the form {{t, {{x, f}...}}...}"*);

AsciiTimeOfIndex(*::usage = "AsciiTimeOfIndex[data,i] returns the time, t, of the ith element of data where data is in the form {{t, {{x, f}...}}...}"*);

ReadCarpetASCII1D(*::usage = "ReadCarpetASCII1D[filename, dir] reads a 1D CarpetIOASCII output file.  It assumes that the output is in direction dir, running from 1 to 3. The data is returned as a list of lists of the form {{t1, d1}, {t2, d2}, ..., {tn, dn}} where the ti are the times and the di are DataTables representing the 1D data at those times, and the outermost list is over refinement levels."*);

ReadCarpetASCII1DFromRun(*::usage = "UNTESTED!! ReadCarpetASCII1DFromRun[sim, filename, rl, dir] reads a 1D CarpetIOASCII output file from filename in sim.  The function returns a single refinement level rl from the file.  It assumes that the output is in direction dir, running from 1 to 3. The data is returned as a list of the form {{t1, d1}, {t2, d2}, ..., {tn, dn}} where the ti are the times and the di are DataTables representing the 1D data at those times."*);

AsciiData1D;

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
  Module[{lines, useLines},
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
 Module[{levels, levelsPresent, data, data2, rls, processLevel, prls},
(*  lines = ReadList[fileName, String, NullRecords -> True];*)
  If[FileType[fileName] === None, Error["ReadCarpetASCII1D: File "<>fileName<>" not found"]];
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

(* This function is untested *)
ReadCarpetASCII1DFromRun[run_, fileName_, rl_:0, dir_:1] :=
  Flatten[Map[ReadCarpetASCII1D[#, dir][[rl+1]] &, FindRunFile[run, fileName]], 1];

MGraph[data__] :=
  Manipulate[
   ListLinePlot[data[[i, 2]], 
    PlotLabel -> "t = " <> ToString[data[[i, 1]]]], {i, 1, Length[data],
     1}];

End[];

EndPackage[];
