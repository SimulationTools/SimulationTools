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

BeginPackage["Memo`"];

DefineMemoFunction::usage = "DefineMemoFunction[f[args], body] defines a function f with arguments args and body body such that the result of the function is remembered after the first time it is called.  This is used to cache data that is expensive to read or compute.  See ClearAllMemos for flushing this cache.";
ClearAllMemos::usage = "ClearAllMemos[] wipes the cache of functions that have been memoised using DefineMemoFunction.";

Begin["`Private`"];

If[!ValueQ[$cache], $cache = {}];

Global`StandardDefinition[DefineMemoFunction] = True;

SetAttributes[DefineMemoFunction, HoldAll];
DefineMemoFunction[name_[args___], body_] :=
    pat : name[args] := (AppendTo[$cache, Hold[pat]]; pat = body);

ClearAllMemos[] :=
  ($cache /. Hold -> Unset; $cache = {};)

End[];

EndPackage[];
