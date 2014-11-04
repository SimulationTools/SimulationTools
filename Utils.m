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

BeginPackage["SimulationTools`Utils`"];

FilterNaNs(*::usage = "FilterNaNs[d] replaces any NaN (Not a Number) values in the DataRegion d with Missing[], which is Mathematica's notation for missing data."*);
NaNQ(*::usage = "NaNQ[x] returns True if x is a NaN (Not a Number) value and False if it is not.  Mathematica deals strangely with NaN values imported from other programs.  This function was developed for use with the h5mma package for reading HDF5 data."*);
RunSubprocess;

(* Experimental *)
MapMonitored;
TailFile;
ShowIt;
MapSuccessive;

Begin["`Private`"];

(**********************************************************)
(* FilterNaNs                                             *)
(**********************************************************)

SyntaxInformation[FilterNaNs] =
 {"ArgumentsPattern" -> {_}};

SetAttributes[FilterNaNs, {Listable}];

FilterNaNs[d_] :=
 If[NaNQ[d], Missing[], d];


(**********************************************************)
(* NaNQ                                                   *)
(**********************************************************)

SyntaxInformation[NaNQ] =
 {"ArgumentsPattern" -> {_}};

NaNQ[x_] :=
 Round[x] == -2147483648;

If[$VersionNumber < 9.,
   CreateTemporary[] :=
   Module[
     {name},
     name = FileNameJoin[{$TemporaryDirectory, IntegerString[RandomInteger[10^50], 16]}];
     Close[OpenWrite[name]];
     name]];

RunSubprocess[{cmd_, args___}] :=
  Module[
    {stdoutFile, stderrFile, cmdString, retCode, stdout, stderr},
    stdoutFile = CreateTemporary[];
    stderrFile = CreateTemporary[];
    (* TODO: handle quoting *)
    cmdString = StringJoin[Riffle[{cmd, args}, " "]];
    (* Run executes the cmdString using a shell *)
    retCode = Run[cmdString <> ">"<>stdoutFile<>" 2>"<>stderrFile];
    stdout = ReadList[stdoutFile, String, NullRecords -> True];
    stderr = ReadList[stderrFile, String, NullRecords -> True];
    DeleteFile[stdoutFile];
    DeleteFile[stderrFile];
    {retCode, stdout, stderr}];

MapMonitored[f_, args_List] :=
 Module[{x = 0},
  Monitor[MapIndexed[(x = #2[[1]]; f[#1]) &, args], 
   ProgressIndicator[x/Length[args]]]];

MapSuccessive[f_, l_List] :=
 MapThread[f, Drop[#, -1] & /@ {l, RotateLeft[l]}]


TailFile[filename_String, n_Integer] :=
 Module[{size, handle, lines},
  size = FileByteCount[filename];
  handle = OpenRead[filename];
  SetStreamPosition[handle, Max[size - n, 0]];
  lines = ReadList[handle, String];
  Close[handle];
  StringJoin[Riffle[lines, "\n"]]];

(* http://mathematica.stackexchange.com/questions/2230/mathematica-debuggability *)
SetAttributes[ShowIt, HoldAll];
ShowIt[code_] := 
   Module[{y}, 
      Print[ToString[Unevaluated[code]], " = ", y = code]; 
      y]

End[];
EndPackage[];
