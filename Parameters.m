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

BeginPackage["Parameters`",
 {
  "Memo`",
  "Error`",
  "RunFiles`"
 }];

FindSimulationParameters::usage  = "FindSimulationParameters[sim, pattern] gives a list of the names of all the parameters which match pattern which were set in the simulation.";
ReadSimulationParameter::usage = "ReadSimulationParameter[sim, pname] reads the value of the named parameter from the simulation.";
ParseParameterFile;

LookupParameter = ReadSimulationParameter;
FindParameters = FindSimulationParameters;

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
 Module[{lines, parseLine, strip, fileName, fileNames},
  If[StringMatchQ[from, __ ~~ ".par"],
    (* Is "from" a full parameter file name? *)
    fileNames = {from},
    (* Is "from" a run name? *)
    fileNames = FindRunFilesFromPattern[from, "*.par"];
    If[Length[fileNames] == 0,
      fileNames = FindRunFile[from, from <> "-1.par"];
      If[Length[fileNames] == 0,
        Error["Cannot find parameter file " <> ToString[from]]],
      fileNames = FindRunFile[from, fileNames[[1]]]]
  ];

  fileName = First[fileNames];
  lines = ReadList[fileName, String];
  lines = unbreakBrokenStrings[lines];

  strip[t_] :=
   StringTrim[t, RegularExpression["[\t\" ]*"]];

  parseLine[s_] :=
   Which[
   StringMatchQ[s, RegularExpression["[ \t]*#.*"]],
    Comment[s],
   StringMatchQ[s, (Whitespace | StartOfString) ~~
       "ActiveThorns" ~~ (Whitespace | "=") ~~ __, IgnoreCase -> True],
    ActiveThorns[StringSplit[StringCases[s, "\"" ~~ thorns__ ~~ "\"" -> thorns][[1]]]],
   StringMatchQ[s, RegularExpression[".*::.*=.*"]],
    ParameterSetting[
        ToLowerCase[strip[StringCases[s, param__ ~~ "=" ~~ val__ -> param][[1]]]],
        strip[StringCases[s, param__ ~~ "=" ~~ val__ -> val][[1]]]],
   True,
    Error["Unrecognized line in parameter file: " <> s]
   ];

  Map[parseLine, lines]
 ]
];

ReadSimulationParameter[parFile_List, name_, default_:None] :=
 Module[{l},
  l = Cases[parFile, ParameterSetting[ToLowerCase[name], x_] -> x];
  If[l === {} && default =!= None, Return[default]];
  If[Length[l] == 0, Error["Parameter " <> name <> " not found"]];
  First[l]];

ReadSimulationParameter[from_String, name_, default_:None] :=
  Module[{},
    (* Assume the parameter file is named after the run *)
    LookupParameter[ParseParameterFile[from], name, default]
  ];

FindSimulationParameters[parFile_String, pattern_] :=
  FindParameters[ParseParameterFile[parFile], pattern];

FindSimulationParameters[parFile_List, pattern_] :=
  Module[{parameters},
    parameters = Cases[parFile, ParameterSetting[name_,value_] -> name];
    Select[parameters, StringMatchQ[#, pattern, IgnoreCase->True] &]];

End[];

EndPackage[];
