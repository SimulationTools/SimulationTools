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

BeginPackage["SimulationTools`IniFile`",
 {
  "SimulationTools`Error`",
  "SimulationTools`Memo`"
 }];

ReadIniFile;
IniVariable;

Begin["`Private`"];

DefineMemoFunction[ReadIniFile[file_],
  If[FileType[file] === None, 
     Error["Ini file "<>file<>" not found"],
     (* else *)
     Flatten[StringCases[
       ReadList[file, String],
       key__ ~~ "=" ~~ value___ :> {StringTrim@key -> StringTrim@value}]]]];

IniVariable[file_, key_] :=
  Module[{map = ReadIniFile[file]},
    If[!MemberQ[First/@map, key], Error["Key not found"],
    Return[key /. map]]];

End[];

EndPackage[];
