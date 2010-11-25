(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["Parameters`", {"RunFiles`", "Memo`"}];

ParseParameterFile;
LookupParameter;
FindParameters;

Begin["`Private`"];

unbreakBrokenStrings[lines2_List] :=
  Module[{oddQuotes, positions, pairs, lines},
    oddQuotes[s_String] := OddQ[Length[StringCases[s, "\""]]];
    lines = lines2;
    positions = First/@Position[lines, _?oddQuotes, {1}];
    pairs = Partition[positions, 2];

    Scan[Function[pair,
      lines[[pair[[1]] ;; pair[[2]]]] =
        StringJoin[Riffle[lines[[pair[[1]] ;; pair[[2]]]], " "]];
      lines = Drop[lines, {pair[[1]]+1, pair[[2]]}]],

      Reverse[pairs]];
    Return[lines]];

DefineMemoFunction[ParseParameterFile[from_String],
 Module[{lines, parseLine, thorns, param, val, removeWhiteSpace, fileName, fileNames},
  (* Is "from" a full parameter file name? *)
  If[StringMatchQ[from, __ ~~ ".par"],
    fileNames = {from},
(*    fileNames = FindRunFile[from, from <> ".par"];*)
    (* Is "from" a run name? *)
    fileNames = FindRunFilesFromPattern[from, "*.par"];
    If[Length[fileNames] == 0,
      fileNames = FindRunFile[from, from <> "-1.par"];
      If[Length[fileNames] == 0,
        Throw["Cannot find parameter file " <> ToString[from]]],
      fileNames = FindRunFile[from, fileNames[[1]]]]];
  fileName = First[fileNames];
  lines = ReadList[fileName, String];
  removeWhiteSpace[t_] := 
   StringReplace[
    t, (StartOfString ~~ Whitespace) | (Whitespace ~~ EndOfString) :> 
     ""];

  lines = unbreakBrokenStrings[lines];

  parseLine[s_] :=
   If[StringMatchQ[s, RegularExpression["[ \t]*#.*"]],
    Comment[s],
    If[StringMatchQ[
      s, (Whitespace | StartOfString) ~~ 
       "ActiveThorns" ~~ (Whitespace | "=") ~~ __, IgnoreCase -> True],
     ActiveThorns[
      StringSplit[
       StringCases[s, "\"" ~~ thorns__ ~~ "\"" -> thorns][[1]]]],
     If[StringMatchQ[s, RegularExpression[".*::.*=.*"]],
      ParameterSetting[
       ToLowerCase[
        removeWhiteSpace[
         StringCases[s, param__ ~~ "=" ~~ val__ -> param][[1]]]], 
       removeWhiteSpace[
        StringCases[s, param__ ~~ "=" ~~ val__ -> val][[1]]]],
      Throw["Unrecognized line in parameter file: " <> s]]]];
  Map[parseLine, lines]
  ]];

LookupParameter[parFile_List, name_, default_:None] :=
 Module[{l},
  l = Cases[parFile, ParameterSetting[ToLowerCase[name], x_] -> x];
  If[l === {} && default =!= None, Return[default]];
  If[Length[l] == 0, Throw["Parameter " <> name <> " not found"]];
  First[l]];

LookupParameter[from_String, name_, default_:None] :=
  Module[{},
    (* Assume the parameter file is named after the run *)
    LookupParameter[ParseParameterFile[from], name, default]
  ];

FindParameters[parFile_String, pattern_] :=
  FindParameters[ParseParameterFile[parFile], pattern];

FindParameters[parFile_List, pattern_] :=
  Module[{parameters},
    parameters = Cases[parFile, ParameterSetting[name_,value_] -> name];
    Select[parameters, StringMatchQ[#, pattern, IgnoreCase->True] &]];

End[];

EndPackage[];
