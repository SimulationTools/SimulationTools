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

BeginPackage["SimulationTools`Ascii`",
 {
  "SimulationTools`ColumnFile`",
  "SimulationTools`DataTable`",
  "SimulationTools`Profile`"
 }];

(* This should be handled through some sort of abstract "simulation
   scalar" API, similar tot he GridFunctions API. *)

ReadCarpetASCIIScalar(*::usage = "ReadCarpetASCIIScalar[sim,filename] Reads Carpet ASCII 0D and Scalar data."*);

Begin["`Private`"];

ReadCarpetASCIIScalar[run_,fileName_] :=
 Module[{coltime, coldata, data},
(*  lines = ReadList[fileName, String, NullRecords -> True];*)
  (*If[FileType[fileName] === None, Error["ReadCarpetASCIIScalar: File "<>fileName<>" not found"]];*)
  Profile["ReadCarpetASCIIScalar: Check if we want to plot scalar output or 0D output. They have different formats.",
    If[StringLength[StringSplit[FileNameTake[fileName],"."][[2]]]<= 2,coltime=9;coldata=13;,coltime=2;coldata=3;]];
  Profile["ReadCarpetASCIIScalar: " <> fileName,
  Profile["ReadCarpetASCIIScalar: Reading file", 
    data = MakeDataTable[ReadColumnFile[run,fileName,{coltime,coldata}]]]]];
End[];

EndPackage[];
