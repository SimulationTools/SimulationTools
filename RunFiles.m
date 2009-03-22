
BeginPackage["RunFiles`"];

FileInRun;
ReadColumnFile;
ReadColumnFile2;

Begin["`Private`"];

(*--------------------------------------------------------------------
  File reading
  --------------------------------------------------------------------*)

FileInRun[runName_String, fileName_] :=
  Module[{fullPath = ToFileName[runName, fileName],
          runPath = ToFileName[{Global`RunDirectory, runName <> "-all"}, fileName],
          sfPath1 = ToFileName[{Global`RunDirectory, runName, "output-0000-active", runName}, fileName],
          sfPath2 = ToFileName[{Global`RunDirectory, runName, "output-0000", runName}, fileName]},

    Which[
      StringMatchQ[runName, __ ~~ "/" ~~ __], fullPath,
      FileType[runPath] =!= None, runPath,
      FileType[sfPath1] =!= None, sfPath1,
      FileType[sfPath2] =!= None, sfPath2,
      Throw["Can't find file "<>fileName<>" in run "<>runName], Null]];

ReadColumnFile[fileName_String, cols_List] :=
  Module[{list, list2, isComment},
    If[FileType[fileName] === None, Throw["File " <> fileName <> " not found"]];
    list = Import[fileName, "TSV"];

    isComment[x_] :=
      StringQ[x[[1]]] && StringMatchQ[x[[1]], "#" ~~ ___];

    list2 = Select[list, !isComment[#] &];

(*    list = ReadList[fileName, Real, RecordLists->True];*)
    Return[Map[Extract[#, Map[List, cols]] &, list2]]];

ReadColumnFile2[fileName_String, cols_List] :=
  Module[{list, list2, isComment},
    If[FileType[fileName] === None, Throw["File " <> fileName <> " not found"]];

    list = ReadList[fileName, String];

    isComment[x_] :=
      StringQ[x] && StringMatchQ[x, "#" ~~ ___];

    list2 = Select[list, !isComment[#] &];

    list3 = Map[StringSplit, list2];
    list4 = Map[Map[ToExpression, #] &, list3];

    Return[Map[Extract[#, Map[List, cols]] &, list4]]];







End[];

EndPackage[];
