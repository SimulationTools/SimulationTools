(* Copyright 2015 Ian Hinder

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

BeginPackage["SimulationTools`Formaline`",
 {"Piraha`", "SimulationTools`RunFiles`"}];

ReadSimulationFormalineParameters;
CompareParameters;
CompareSimulationParameters;
ReadFormalineMetadata;

Begin["`Private`"];

(* Process parse tree *)
processValue["real"[x_]] := Internal`StringToDouble[x]
processValue["int"[x_]] := ToExpression[x]
processValue[
  s : ("stringvalue"["quote"["\""], frag___, "quote"["\""]])] :=
 StringJoin[processValue /@ {frag}]
processValue["stringfragment"[s_String]] := s
processValue["stringfragment"[]] := ""
processValue["stringfragment"["quotedchar"[s_String]]] := StringDrop[s,1];
processValue["symbol"[s_]] :=
 s
processValue[x_] :=
 Error["Processing not implemented for " <> ToString[x]]

ParseFormalineMetadataFile[file_String] :=
  Module[{parseTree},
    parseTree = CleanParseTree[ParsePEG["formaline.peg", "file", file]];
    Cases[parseTree, 
      "element"["key"[k__], "value"[v__]] :> (k -> processValue[v])]];

ReadFormalineMetadata[sim_String] :=
  Module[{files, file},
    files = FindSimulationFiles[sim, "formaline-jar.txt"];
    file = First[files];
    ParseFormalineMetadataFile[file]];

ReadSimulationFormalineParameters[sim_String] :=
  Module[{files, file, elements, parameterElements, keys, values},
    files = FindSimulationFiles[sim, "formaline-jar.txt"];
    file = First[files];
    elements = ParseFormalineMetadataFile[file];
    parameterElements = Select[elements, StringMatchQ[#[[1]], StartOfString~~"parameters/"~~__]&];
    {keys, values} = Transpose[Apply[List, parameterElements, 2]];
    keys2 = Map[StringReplace[#,StartOfString~~"parameters/"->""] &, keys];
    Thread[keys2 -> values]];

CompareParameters[ps1_, ps2_] :=
  Module[{diffs1, diffs2, only1Names, only2Names, only1, only2, modifiedNames, removed,
    added, modified},
    diffs1 = Complement[ps1, Intersection[ps1,ps2]];
    diffs2 = Complement[ps2, Intersection[ps1,ps2]];

    only1Names = Complement[diffs1[[All,1]], diffs2[[All,1]]];
    only2Names = Complement[diffs2[[All,1]], diffs1[[All,1]]];

    only1 = Thread[only1Names -> (only1Names /. diffs1)];
    only2 = Thread[only2Names -> (only2Names /. diffs2)];

    modifiedNames = Intersection[diffs1[[All,1]], diffs2[[All,1]]];

    removed = Thread[only1Names -> Transpose@{only1Names/.diffs1, ConstantArray[None,Length[only1Names]]}];
    added = Thread[only2Names -> Transpose@{ConstantArray[None,Length[only2Names]], only2Names/.diffs2}];
    modified = Thread[modifiedNames -> Thread[{modifiedNames/.diffs1, modifiedNames/.diffs2}]];

    Sort[Join[added, removed, modified]]]

FormatParameterComparison[comp_] :=
  TraditionalForm@
   Style[Grid[Map[{Style[#[[1]], 
         If[#[[2, 1]] === None, Darker@Green, 
          If[#[[2, 2]] === None, Darker@Red, Blue]]], #[[2, 1]], #[[2, 
          2]]} &, comp] /. None -> "", 
    Alignment -> Left, Dividers -> All, Spacings -> {1, 1.5}],FontFamily->"Sans"];

CompareSimulationParameters[sim1_String, sim2_String] :=
  FormatParameterComparison[CompareParameters[
    ReadSimulationFormalineParameters[sim1], ReadSimulationFormalineParameters[sim2]]];

End[];
EndPackage[];
