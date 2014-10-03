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

BeginPackage["SimulationTools`Parameters`",
 {
  "SimulationTools`Memo`",
  "SimulationTools`Error`",
  "SimulationTools`RunFiles`"
 }];

FindSimulationParameters::usage  = "FindSimulationParameters[sim, pattern] gives a list of the names of all the parameters which match pattern which were set in the simulation.";
ReadSimulationParameter::usage = "ReadSimulationParameter[sim, pname] reads the value of the named parameter from the simulation.";

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

findParameterFile[sim_String] :=
  Module[{fileNames, fileName},
    fileNames = FindSimulationFiles[sim, "*.par"];
    If[fileNames === {},
      Error["Cannot find parameter file in simulation "<>sim]];
    fileName = First[fileNames]];

parseParameterFile[fileName_String] :=
  parseParameterFile[fileName, FileDate[fileName]];

SetAttributes[profile, HoldFirst];
profile[x_] :=
  Module[{r,t},
    {t,r} = AbsoluteTiming[x];
    r];

parseParameterFile[fileName_String, fileDate_List] := parseParameterFile[fileName, fileDate] =
Module[{lines, parseLine, strip},
  profile[lines = ReadList[fileName, String];
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
        StringReplace[ToLowerCase[strip[StringCases[s, (Shortest[param__] ~~ "=" ~~ val__ ~~ EndOfString) -> param][[1]]]]," "->""],
        strip[StringCases[s, Shortest[param__] ~~ "=" ~~ val__ ~~ EndOfString -> val][[1]]]],
   True,
    Error["Unrecognized line in parameter file: " <> s]
   ];

  Map[parseLine, lines]]];

getParameterValue[parFile_List, name_, default_] :=
 Module[{l},
  l = Cases[parFile, ParameterSetting[ToLowerCase[name], x_] -> x];
  If[l === {} && default =!= None, Return[default]];
  If[Length[l] == 0, Error["Parameter " <> name <> " not found"]];
  First[l]];

DocumentationBuilder`SymbolDescription["ReadSimulationParameter"] =
  "read the value of a specified parameter from a simulation";

ReadSimulationParameter[sim_String, parName_String, default_:None] :=
  readSimulationParameter[sim, parName, default, FileDate[findParameterFile[sim]]];

readSimulationParameter[sim_String, parName_String, default_, fileDate_] := readSimulationParameter[sim, parName, default, fileDate] =
      getParameterValue[parseParameterFile[findParameterFile[sim]], parName, default];


DocumentationBuilder`SymbolDescription["FindSimulationParameters"] =
  "search for available parameters";

FindSimulationParameters[sim_String, pattern_] :=
  findParameters[parseParameterFile[findParameterFile[sim]], pattern];

findParameters[parFile_List, pattern_] :=
  Module[{parameters},
    parameters = Cases[parFile, ParameterSetting[name_,value_] -> name];
    Select[parameters, StringMatchQ[#, pattern, IgnoreCase->True] &]];

End[];

EndPackage[];
