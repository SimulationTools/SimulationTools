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

BeginPackage["SimulationTools`SimulationCompare`",
 {"SimulationTools`GridFunctions`"}];

CompareSimulations;

Begin["`Private`"];

absTol = 1.*^-11;
relTol = 1.*^-11;

compareData[sim1_String, sim2_String, var_String, dims_, opts___] :=
 Module[
   {d1, d2, absDiff, relDiff, statusLine},
   {d1, d2} = 
   Table[ReadGridFunction[sim, var, dims, 
                          opts], {sim, {sim1, sim2}}];
   
   statusLine[result_] :=
   If[result === True,
      WriteString["stdout", "."],
      Print[Row[
        Flatten@{"Variable" -> var, "Dimensions" -> dims, {opts} (* /.((a_->b_):>(ToString[a]->b)) *), 
                 "Result" -> If[result, "Pass", "Fail"]}, ", "]]];
  
  absDiff = Max[Abs[d1 - d2]];
  
  If[absDiff < absTol,
   statusLine[True];
   Return[True],
   relDiff = Max[Abs[d1 - d2]/(0.5 Abs[d1 + d2])];
   If[relDiff < relTol,
    statusLine[True];
    Return[True],
    statusLine[False];
    Print[
     "AbsoluteDifference -> " <> ToString[absDiff] <> 
      ", RelativeDifference->" <> ToString[relDiff]];
    Return[False]]]];

CompareSimulations[sim1_String, sim2_String] :=
  Module[{vars, maps, levels},
   vars = FindGridFunctions[sim1];
   (*Print["vars = ",vars];
   *)Table[compareSimulationVariable[sim1, sim2, var], {var, vars}]];

compareSimulationVariable[sim1_, sim2_, var_] :=
  Module[{levels},
   levels = ReadRefinementLevels[sim1, var, All, Map -> 0];
   (*Print["levels = ",levels];
   *)Table[
    compareSimulationLevel[sim1, sim2, var, level], {level, levels}]];

compareSimulationLevel[sim1_, sim2_, var_, level_] :=
 Module[{maps},
  maps = ReadMaps[sim1, var, All, RefinementLevel -> level];
  (*Print["maps = ",maps];*)
  
  Table[compareSimulationMap[sim1, sim2, var, level, map], {map, 
    maps}]];

compareSimulationMap[sim1_, sim2_, var_, level_, map_] :=
 Module[{maps, its},
  its = ReadIterations[sim1, var, All, RefinementLevel -> level, 
    Map -> map];
  Table[
   compareData[sim1, sim2, var, All, RefinementLevel -> level, 
    Map -> map, Iteration -> it], {it, its}]];

End[];
EndPackage[];
