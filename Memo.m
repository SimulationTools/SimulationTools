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

BeginPackage["SimulationTools`Memo`",
 {
  "SimulationTools`ArgumentChecker`",
  "SimulationTools`"
 }
];

DefineMemoFunction::usage = "DefineMemoFunction[f[args], body] defines a function f with arguments args and body body such that the result of the function is remembered after the first time it is called.  This is used to cache data that is expensive to read or compute.  See ClearAllMemos for flushing this cache.";
ClearAllMemos::usage = "ClearAllMemos[] wipes the cache of functions that have been memoised using DefineMemoFunction.";
$Memoisation::usage = "$Memoisation is a variable which controls whether expensive functions are memoised or not.";

Begin["`Private`"];

$Memoisation := If[$SimulationToolsCompatibilityVersion < 1, True, False];

If[!ValueQ[$cache], $cache = {}];

StandardDefinition[DefineMemoFunction] = True;

DocumentationBuilder`SymbolDescription["ClearAllMemos"] =
  "memoisation of functions; i.e. in-memory caching of values so that they don't have to be recomputed";

SetAttributes[DefineMemoFunction, HoldAll];
DefineMemoFunction[name_[args___], body_] :=
 Module[{},
  pat : name[args] /; $Memoisation  := (AppendTo[$cache, Hold[pat]]; pat = body);
  pat : name[args] /; !$Memoisation := body;
 ]

ClearAllMemos[] :=
   (* Use Quiet here to avoid an unexplained warning message about
      assignments not being found.  This seems to be a problem after
      an exception has been thrown previously, but I don't understand
      why. *)
   Quiet[($cache /. Hold -> Unset; $cache = {};),Unset::norep];

End[];

EndPackage[];
