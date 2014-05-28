(* Copyright 2010-2014 Ian Hinder and Barry Wardell

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

BeginPackage["StackTrace`",
 {
  "Stack`"
 }];

WithStackTrace;
StandardStackDefinition;
CurrentStackTrace;

Begin["`Private`"];

$stack = {};

SetAttributes[WithStackTrace, HoldAll];
WithStackTrace[symbolPattern_, expr_] :=
  Module[
    {expr1, inCheck},
    expr1 := expr;
    Internal`InheritedBlock[
      {SetDelayed},

      Unprotect[SetDelayed];
      SetDelayed[(fn_Symbol /; (StringMatchQ[Context[fn], symbolPattern] &&
        StandardStackDefinition[fn] =!= True))[args___], rhs_] /; !TrueQ[inCheck] :=
        Block[
          {inCheck = True},
          (* This construction using Part avoids introducing an explicit
             temporary with Module which causes a big performance hit. *)
          fn[args] :=
          {StackPush[$stack,fn],
            Catch[rhs, _, (StackPop[$stack]; Throw[#1,#2]) &],
            StackPop[$stack]}[[2]]];

      Protect[SetDelayed];

      expr1]];

CurrentStackTrace[] :=
  StackRead[$stack];

End[];
EndPackage[];
