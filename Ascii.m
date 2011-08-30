(* ::Package:: *)

(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["Ascii`", {"Ascii1D`","DataTable`", "Profile`", "Memo`", "RunFiles`"}];

(* AsciiData1D::usage = "AsciiData1D[fileName] loads the 1D data in
fileName (which should be in ygraph format) and returns it as a list
in the form {{t, {{x, f}...}}...}."; *)

ReadCarpetASCIIScalar::usage = "ReadCarpetASCIIScalar[filename, dir] reads a 1D CarpetIOASCII output file.  It assumes that the output is in direction dir, running from 1 to 3. The data is returned as a list of lists of the form {{t1, d1}, {t2, d2}, ..., {tn, dn}} where the ti are the times and the di are DataTables representing the 1D data at those times, and the outermost list is over refinement levels.";
ReadCarpetASCIIScalarFromRun::usage = "UNTESTED!! ReadCarpetASCIIScalarFromRun[run, filename, rl, dir] reads a 1D CarpetIOASCII output file from filename in run.  The function returns a single refinement level rl from the file.  It assumes that the output is in direction dir, running from 1 to 3. The data is returned as a list of the form {{t1, d1}, {t2, d2}, ..., {tn, dn}} where the ti are the times and the di are DataTables representing the 1D data at those times.";
ReadCarpetASCIIFromRun::usage = "UNTESTED!! ReadCarpetASCIIFromRun[run, filename, rl, dir] reads a 1D CarpetIOASCII output file from filename in run.  The function returns a single refinement level rl from the file.  It assumes that the output is in direction dir, running from 1 to 3. The data is returned as a list of the form {{t1, d1}, {t2, d2}, ..., {tn, dn}} where the ti are the times and the di are DataTables representing the 1D data at those times.";

Begin["`Private`"];

takeCols[it_List, c1_, c2_] := 
  Profile["ReadCarpetASCII1D: takeCols", 
    Map[{#[[c1]], #[[c2]]} &, it]];

DefineMemoFunction[tableImport[fn_],
  Import[fn, "Table"]];

fixData[d_] :=
  Profile["ReadCarpetASCII1D: Fixing data",
    DeleteDuplicates[d, 
                     (Abs[#1[[1]]-#2[[1]]] < 10.^-10) &]];

ReadCarpetASCIIScalar[fileName_, dir_:1] :=
 Module[{levels, levelsPresent,coltime,coldata, data, data2, rls, processLevel, prls},
(*  lines = ReadList[fileName, String, NullRecords -> True];*)
  If[FileType[fileName] === None, Throw["ReadCarpetASCIIScalar: File "<>fileName<>" not found"]];
  Profile["ReadCarpetASCIIScalar: Check if we want to plot scalar output or 0D output. They have different formats.",
    If[StringLength[StringSplit[FileNameTake[fileName],"."][[2]]]<= 2,coltime=9;coldata=13;,coltime=2;coldata=3;]];
  Profile["ReadCarpetASCIIScalar: " <> fileName,
  Profile["ReadCarpetASCIIScalar: Reading file", 
    data = tableImport[fileName]];
  Profile["ReadCarpetASCIIScalar: Eliminating white space", 
    data2 = Select[data, Length[#] != 0 && #[[1]] != "#" &]];
  Profile["ReadCarpetASCIIScalar: Determining levels present",
    levelsPresent = Union[Map[#[[3]] &, data2]]];

  levels = GatherBy[data2, #[[3]]&];
  rls = Map[First[#][[3]]&, levels];

  processLevel[data3_] :=
    Module[{data4, data5, data6,times, dataWithTimes},
(*      Print["rl" <> ToString[data3[[1,3]]]];*)
      Profile["ReadCarpetASCIIScalar: Splitting by iteration", 
        data4 = N[SplitBy[data3, #[[1]] &]]];
      data5 = fixData[data4];
 (*     Profile["ReadCarpetASCII1D: Extracting times",
        times = Map[#[[1]][[2]] &, data4]];
      dataWithTimes = Flatten[MapThread[List, {data5}],1]];
(*  prls = MapThread[List, {rls, processLevel /@ levels}];*)
  prls = processLevel /@ levels;
  Return[prls]]];*)
      Profile["ReadCarpetASCIIScalar: Extracting times",
        times = Map[#[[1]][[coltime]] &, data5]];
      Profile["ReadCarpetASCIIScalar: Extracting data",
        data6 = Map[#[[1]][[coldata]] &, data5]];
      dataWithTimes = MapThread[List, {times, data6}]];

(*  prls = MapThread[List, {rls, processLevel /@ levels}];*)
  prls = processLevel /@ levels;
  Return[prls]]];
(* This function is untested *)
ReadCarpetASCIIScalarFromRun[run_, fileName_, rl_:0] :=
  DeleteDuplicates[Sort[Flatten[Map[ReadCarpetASCIIScalar[#] &, FindRunFile[run, fileName]],2]],(Abs[#1[[1]]-#2[[1]]] < 10.^-10) &];
ReadCarpetASCIIFromRun[run_, fileName_, rl_:0, dir_:1] :=
  If[StringLength[StringSplit[fileName,"."][[2]]]== 1,
    DeleteDuplicates[Sort[Flatten[Map[ReadCarpetASCII1D[#, dir][[rl+1]] &, FindRunFile[run, fileName]], 1]],(Abs[#1[[1]]-#2[[1]]] < 10.^-10) &],
    DeleteDuplicates[Sort[Flatten[Map[ReadCarpetASCIIScalar[#] &, FindRunFile[run, fileName]],2]],(Abs[#1[[1]]-#2[[1]]] < 10.^-10) &]]
(* This function is untested *)

End[];

EndPackage[];
