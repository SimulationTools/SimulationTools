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

BeginPackage["SimulationTools`ArgumentChecker`",
 {
  "SimulationTools`Error`"
 }];

WithArgumentChecking;

Begin["`Private`"];

cleanArg[arg_] :=
  arg /. ((y_Symbol)[xs___] /; MemberQ[Attributes[y], ReadProtected]) -> y; (* Not ideal, but robust *)

forStringForm[s_String] := StringReplace[s,"`"->"`.`"];

SetAttributes[WithArgumentChecking, HoldFirst];
WithArgumentChecking[expr_] :=
  Module[
    {expr1, inCheck},
    expr1 := expr;
    Internal`InheritedBlock[
      {SetDelayed},

      Unprotect[SetDelayed];
      SetDelayed[(fn_Symbol /; (Context[fn] =!= "System`" && Global`StandardDefinition[fn] =!= True))[args___], rhs_] /; !TrueQ[inCheck] :=
        Block[
          {inCheck = True},
          (* Print["Defining ", fn, "[", StringJoin@Riffle[ToString/@{args},","],"]"]; *)
          fn[x___] := Error["Invalid arguments to "<>forStringForm@ToString[fn]<>"\nCalled as "<>forStringForm@ToString[fn]<>"["<>
                            StringJoin[Riffle[forStringForm@ToString[InputForm[cleanArg[#]]] & /@ {x}, ", "]] <> 
                            "]"];
          fn[args] := rhs];

      Protect[SetDelayed];

      expr1]];

End[];
EndPackage[];
